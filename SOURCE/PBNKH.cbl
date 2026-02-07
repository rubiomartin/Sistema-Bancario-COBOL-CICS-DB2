       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PBNKH.
       AUTHOR. IBMUSER.
      *
      *****************************************************************
      ** PROYECTO .........: BANKPRJ                                **
      ** PROGRAMA .........: PBNKH                                  **
      ** TITULO ...........: CONSULTA DE HISTORIAL DE MOVIMIENTOS   **
      ** **
      ** TIPO .............: ONLINE                                 **
      ** - LENGUAJE ...............: COBOL                          **
      ** - ENTORNO ................: CICS                           **
      ** - BASE DE DATOS ..........: DB2                            **
      ** **
      ** DESCRIPCION ......:                                        **
      ** **
      ** - Visualiza el historial de transacciones del usuario.     **
      ** Soporta Paginación (PF8/PF7) y Filtros (PF5).              **
      ** **
      ** Funcionalidades DB2:                                       **
      ** - Utiliza cursores (CUR-SMART-ASC/DESC) para               **
      ** navegacion eficiente.                                      **
      ** - Permite ordenar por ID Ascendente o Descendente.         **
      ** **
      ** Teclas:                                                    **
      ** PF5 : Cambiar filtro (Deposito/Retiro/Transf).             **
      ** PF7 : Volver al inicio / Pagina anterior.                  **
      ** PF8 : Pagina siguiente.                                    **
      ** PF10: Invertir orden (ASC/DESC).                           **
      ** PF11: Limpiar filtros.                                     **
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * COPIES DE MAPAS, COMMAREA Y UTILIDADES                         *
      *----------------------------------------------------------------*
       COPY BNKHMP.
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY WSCOMM.
       COPY CPYVALWD.
       COPY CICSATTR.

      * DEFINICIONES DB2
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLCLIEN END-EXEC.
           EXEC SQL INCLUDE DCLMOVIM END-EXEC.

      * VARIABLES DE TRABAJO
       01 WS-FORMATOS.
           05 WS-FECHA-FMT.
               10 FILLER           PIC X(01) VALUE SPACE.
               10 WS-DIA           PIC X(02).
               10 FILLER           PIC X(01) VALUE '/'.
               10 WS-MES           PIC X(02).
               10 FILLER           PIC X(01) VALUE '/'.
               10 WS-ANIO          PIC X(04).
           05 WS-MONTO-FORMATO     PIC $$$,$$$,$$9.99.
           05 WS-TIPO-DISPLAY      PIC X(15).
           05 WS-USER-REL          PIC X(12).

       01 WS-VARIABLES-CONTROL.
           05 WS-CONTADOR-REGS     PIC 9(01) VALUE 0.
           05 L                    PIC S9(4) COMP.
           05 WS-USER-DB2          PIC X(8).
           05 WS-OPER              PIC X(01) VALUE SPACE.
           05 WS-ORDEN             PIC X(01) VALUE SPACE.
           05 WA-RESPUESTA-CICS    PIC S9(8) COMP.
      * Variable auxiliar para el "Scan" de paginacion
           05 WS-ID-PUNTERO-AUX    PIC S9(9) COMP.

      * SWITCHES DE ESTADO
       01 WS-FLAGS.
           03 SW-ENVIO-MAPA        PIC X     VALUE '0'.
              88 ENVIO-ERASE                 VALUE '1'.
              88 ENVIO-DATAONLY              VALUE '2'.

       01 WC-CONSTANTES.
           03 WC-TRANSACCION       PIC X(4)  VALUE 'BNKH'.
           03 WC-MAP               PIC X(8)  VALUE 'BNKMAPH'.
           03 WC-MAPSET            PIC X(8)  VALUE 'BNKHMP'.
           03 WS-MENSAJE-LOGN       PIC X(25)
              VALUE 'DEBE INGRESAR POR LOGN'.


       LINKAGE SECTION.
       01 DFHCOMMAREA              PIC X(128).

       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
      * 0000 - MAIN DRIVER                                             *
      *----------------------------------------------------------------*
       0000-PROCESO-TAREA.

           IF EIBCALEN > 0
               MOVE DFHCOMMAREA TO COMMAREA-GLOBAL
           END-IF.

           EVALUATE TRUE
               WHEN EIBCALEN = 0
                   SET ESTADO-ERROR-LOGN TO TRUE
                   PERFORM 9200-ENVIAR-AVISO-TEXTO

               WHEN EIBCALEN > 0 AND ESTADO-ERROR-LOGN
                   PERFORM 9300-SALIR-A-LOGN

               WHEN EIBTRNID NOT = WC-TRANSACCION
                   PERFORM 1000-INICIALIZACION

               WHEN OTHER
                   PERFORM 1100-PROCESAR-ENTRADA
           END-EVALUATE.

           PERFORM 9000-ENVIO-MAPA.
           PERFORM 9999-RETORNO-CICS.

      *----------------------------------------------------------------*
      * 1000 - LOGICA DE ENTRADA Y NAVEGACION                          *
      *----------------------------------------------------------------*
       1000-INICIALIZACION.
           MOVE LOW-VALUES TO BNKMAPHO.
           MOVE CG-M-USER  TO WS-USER-DB2.
           MOVE CG-H-OPER  TO WS-OPER.
           MOVE CG-H-ORDEN TO WS-ORDEN.
           MOVE CG-M-USER TO NOMBREUSO.
           PERFORM 8000-LEER-SALDO-DB2.
           IF SQLCODE = 0
               MOVE HV-SALDO TO SALDOO
           ELSE
              MOVE 'ADVERTENCIA: NO SE PUDO LEER SALDO INICIAL' TO MSGO
              MOVE DFHRED TO MSGC
           END-IF
           MOVE 'D'        TO CG-H-ORDEN.
           MOVE ' D'       TO FILORDO.

           PERFORM 2000-RESET-PUNTEROS.
           PERFORM 3000-CARGAR-GRILLA.
           SET ENVIO-ERASE TO TRUE.

       1100-PROCESAR-ENTRADA.
           MOVE CG-M-USER  TO WS-USER-DB2.
           MOVE CG-H-OPER  TO WS-OPER.
           MOVE CG-H-ORDEN TO WS-ORDEN.

           PERFORM 9100-RECIBIR-MAPA.
           SET ENVIO-DATAONLY TO TRUE.

           EVALUATE TRUE
               WHEN EIBAID = DFHPF3
                   PERFORM 9400-VOLVER-AL-MENU

               WHEN EIBAID = DFHPF5
                   PERFORM 2100-CAMBIAR-FILTRO
                   PERFORM 2000-RESET-PUNTEROS
                   PERFORM 3000-CARGAR-GRILLA

               WHEN EIBAID = DFHPF7
                   MOVE SPACES TO MSGO
                   PERFORM 2000-RESET-PUNTEROS
                   MOVE ' SE HA VUELTO AL INICIO' TO MSGO
                   PERFORM 3000-CARGAR-GRILLA

               WHEN EIBAID = DFHPF8
                   MOVE SPACES TO MSGO
                   PERFORM 3500-PAGINACION-ADELANTE
                   PERFORM 3000-CARGAR-GRILLA

               WHEN EIBAID = DFHPF10
                   PERFORM 2200-CAMBIAR-ORDEN
                   PERFORM 2000-RESET-PUNTEROS
                   PERFORM 3000-CARGAR-GRILLA

               WHEN EIBAID = DFHPF11
                   PERFORM 2300-LIMPIAR-FILTROS
                   PERFORM 2000-RESET-PUNTEROS
                   PERFORM 3000-CARGAR-GRILLA

               WHEN OTHER
                   MOVE ' PRESIONE UNA TECLA CORRECTA' TO MSGO
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 2000 - LOGICA DE FILTRADO (FILTROS Y ORDEN)                    *
      *----------------------------------------------------------------*
       2000-RESET-PUNTEROS.
           IF CG-H-ORDEN = 'A'
              MOVE 0          TO HV-ID-MOV
           ELSE
              MOVE 999999999  TO HV-ID-MOV
           END-IF.

       2100-CAMBIAR-FILTRO.
           MOVE SPACE TO SELDEPO SELRETO SELRECO SELTRNO.

           EVALUATE CG-H-OPER
               WHEN ' ' MOVE 'D' TO CG-H-OPER
               WHEN 'D' MOVE 'Z' TO CG-H-OPER
               WHEN 'Z' MOVE 'R' TO CG-H-OPER
               WHEN 'R' MOVE 'T' TO CG-H-OPER
               WHEN 'T' MOVE 'D' TO CG-H-OPER
               WHEN OTHER MOVE 'D' TO CG-H-OPER
           END-EVALUATE.

           PERFORM 2150-ACTUALIZAR-UI-FILTRO.
           MOVE CG-H-OPER TO WS-OPER.

       2150-ACTUALIZAR-UI-FILTRO.
           EVALUATE CG-H-OPER
               WHEN 'D'
                   MOVE ' *' TO SELDEPO
                   MOVE ' FILTRO: SOLO DEPOSITOS' TO MSGO
               WHEN 'Z'
                   MOVE ' *' TO SELRETO
                   MOVE ' FILTRO: SOLO RETIROS'   TO MSGO
               WHEN 'R'
                   MOVE ' *' TO SELRECO
                   MOVE ' FILTRO: SOLO RECIBOS'   TO MSGO
               WHEN 'T'
                   MOVE ' *' TO SELTRNO
                   MOVE ' FILTRO: SOLO TRANSF.'   TO MSGO
           END-EVALUATE.

       2200-CAMBIAR-ORDEN.
           IF CG-H-ORDEN = 'A'
               MOVE 'D' TO CG-H-ORDEN
               MOVE ' D' TO FILORDO
               MOVE ' ORDEN CAMBIADO: DESCENDENTE' TO MSGO
           ELSE
               MOVE 'A' TO CG-H-ORDEN
               MOVE ' A' TO FILORDO
               MOVE ' ORDEN CAMBIADO: ASCENDENTE' TO MSGO
           END-IF.
           MOVE CG-H-ORDEN TO WS-ORDEN.

       2300-LIMPIAR-FILTROS.
           MOVE SPACE TO CG-H-OPER WS-OPER
           MOVE SPACE TO SELDEPO SELRECO SELRETO SELTRNO
           MOVE 'D'   TO CG-H-ORDEN
           MOVE ' FILTROS ELIMINADOS' TO MSGO.

      *----------------------------------------------------------------*
      * 3000 - RELLENAMOS PANTALLA CON REGISTROS                       *
      *----------------------------------------------------------------*
       3000-CARGAR-GRILLA.
           PERFORM 4200-LIMPIAR-GRILLA-COMPLETA.
           PERFORM 7100-OPEN-CURSOR.

           PERFORM 3100-FETCH-LOOP.

           PERFORM 7900-CLOSE-CURSOR.
           PERFORM 3600-CALCULAR-INDICADORES.

       3100-FETCH-LOOP.
           MOVE 1 TO L.
           PERFORM 7400-FETCH-CURSOR.

           PERFORM UNTIL SQLCODE = +100 OR L > 4
              PERFORM 4000-MOVER-A-MAPA

              IF L = 1
                 MOVE HV-ID-MOV TO CG-H-ID1
              END-IF
              MOVE HV-ID-MOV TO CG-H-ID4

              ADD 1 TO L
              PERFORM 7400-FETCH-CURSOR
           END-PERFORM.

       3500-PAGINACION-ADELANTE.
           IF CG-H-DOWN-MORE = '-'
              MOVE ' YA ESTA EN LA ULTIMA PAGINA' TO MSGO
           ELSE
              MOVE CG-H-ID4 TO HV-ID-MOV
           END-IF.

       3600-CALCULAR-INDICADORES.
      * Verificamos si hay mas registros abajo
           MOVE CG-H-ID4 TO HV-ID-MOV
           PERFORM 7100-OPEN-CURSOR
           PERFORM 7400-FETCH-CURSOR
           IF SQLCODE = 0
              MOVE ' +' TO MDOWNO
              MOVE '+'  TO CG-H-DOWN-MORE
           ELSE
              MOVE ' -' TO MDOWNO
              MOVE '-'  TO CG-H-DOWN-MORE
           END-IF
           PERFORM 7900-CLOSE-CURSOR.

      * Verificamos si hay mas registros arriba
           PERFORM 7500-INVIERTE-SENTIDO-ORDEN.
           MOVE CG-H-ID1 TO HV-ID-MOV
           PERFORM 7100-OPEN-CURSOR
           PERFORM 7400-FETCH-CURSOR
           IF SQLCODE = 0
              MOVE ' +' TO MUPO
              MOVE '+'  TO CG-H-UP-MORE
           ELSE
              MOVE ' -' TO MUPO
              MOVE '-'  TO CG-H-UP-MORE
           END-IF
           PERFORM 7900-CLOSE-CURSOR
           PERFORM 7500-INVIERTE-SENTIDO-ORDEN.

      *----------------------------------------------------------------*
      * 4000 - LIMPIEZA DE DATOS Y DISPLAY                             *
      *----------------------------------------------------------------*
       4000-MOVER-A-MAPA.
           EVALUATE HV-TIPO-OPER
              WHEN 'D' MOVE ' DEPOSITO'      TO WS-TIPO-DISPLAY
              WHEN 'R' MOVE ' RECIBO'        TO WS-TIPO-DISPLAY
              WHEN 'Z' MOVE ' RETIRO'        TO WS-TIPO-DISPLAY
              WHEN 'T' MOVE ' TRANSFERENCIA' TO WS-TIPO-DISPLAY
           END-EVALUATE.

           MOVE HV-MONTO       TO WS-MONTO-FORMATO.
           MOVE HV-FECHA(9:2)  TO WS-DIA.
           MOVE HV-FECHA(6:2)  TO WS-MES.
           MOVE HV-FECHA(1:4)  TO WS-ANIO.
           MOVE SPACES         TO WS-USER-REL.
           MOVE HV-USUARIO-REL TO WS-USER-REL(2:8).

           EVALUATE L
               WHEN 1
                   MOVE WS-TIPO-DISPLAY  TO TYP1O
                   MOVE WS-MONTO-FORMATO TO MTO1O
                   MOVE WS-USER-REL      TO REL1O
                   MOVE WS-FECHA-FMT     TO FEC1O
               WHEN 2
                   MOVE WS-TIPO-DISPLAY  TO TYP2O
                   MOVE WS-MONTO-FORMATO TO MTO2O
                   MOVE WS-USER-REL      TO REL2O
                   MOVE WS-FECHA-FMT     TO FEC2O
               WHEN 3
                   MOVE WS-TIPO-DISPLAY  TO TYP3O
                   MOVE WS-MONTO-FORMATO TO MTO3O
                   MOVE WS-USER-REL      TO REL3O
                   MOVE WS-FECHA-FMT     TO FEC3O
               WHEN 4
                   MOVE WS-TIPO-DISPLAY  TO TYP4O
                   MOVE WS-MONTO-FORMATO TO MTO4O
                   MOVE WS-USER-REL      TO REL4O
                   MOVE WS-FECHA-FMT     TO FEC4O
           END-EVALUATE.

       4200-LIMPIAR-GRILLA-COMPLETA.
           MOVE SPACES TO TYP1O MTO1O REL1O FEC1O.
           MOVE SPACES TO TYP2O MTO2O REL2O FEC2O.
           MOVE SPACES TO TYP3O MTO3O REL3O FEC3O.
           MOVE SPACES TO TYP4O MTO4O REL4O FEC4O.

      *----------------------------------------------------------------*
      * 7000 - RUTINAS DB2                                             *
      *----------------------------------------------------------------*
       7000-SQL-DECLARATIONS.
            CONTINUE.
           EXEC SQL DECLARE CUR-SMART-ASC CURSOR FOR
               SELECT ID_MOV, TIPO_OPER, MONTO, USUARIO_REL, FECHA
               FROM IBMUSER.MOVIMIENTOS
               WHERE USUARIO = :WS-USER-DB2
                 AND ID_MOV  > :HV-ID-MOV
                 AND (:WS-OPER = ' ' OR TIPO_OPER = :WS-OPER)
               ORDER BY ID_MOV ASC
           END-EXEC.

           EXEC SQL DECLARE CUR-SMART-DESC CURSOR FOR
               SELECT ID_MOV, TIPO_OPER, MONTO, USUARIO_REL, FECHA
               FROM IBMUSER.MOVIMIENTOS
               WHERE USUARIO = :WS-USER-DB2
                 AND ID_MOV  < :HV-ID-MOV
                 AND (:WS-OPER = ' ' OR TIPO_OPER = :WS-OPER)
               ORDER BY ID_MOV DESC
           END-EXEC.

       7100-OPEN-CURSOR.
           IF CG-H-ORDEN = 'A'
              EXEC SQL OPEN CUR-SMART-ASC END-EXEC
           ELSE
              EXEC SQL OPEN CUR-SMART-DESC END-EXEC
           END-IF.

       7400-FETCH-CURSOR.
           IF CG-H-ORDEN = 'A'
              EXEC SQL FETCH CUR-SMART-ASC
              INTO :HV-ID-MOV, :HV-TIPO-OPER, :HV-MONTO,
                   :HV-USUARIO-REL, :HV-FECHA END-EXEC
           ELSE
              EXEC SQL FETCH CUR-SMART-DESC
              INTO :HV-ID-MOV, :HV-TIPO-OPER, :HV-MONTO,
                   :HV-USUARIO-REL, :HV-FECHA END-EXEC
           END-IF.

       7900-CLOSE-CURSOR.
           IF CG-H-ORDEN = 'A'
              EXEC SQL CLOSE CUR-SMART-ASC END-EXEC
           ELSE
              EXEC SQL CLOSE CUR-SMART-DESC END-EXEC
           END-IF.
       8000-LEER-SALDO-DB2.
           MOVE CG-M-USER TO WS-USER-DB2.
           EXEC SQL SELECT SALDO INTO :HV-SALDO
                FROM IBMUSER.CLIENTES WHERE USUARIO = :WS-USER-DB2
           END-EXEC.

       7500-INVIERTE-SENTIDO-ORDEN.
           IF CG-H-ORDEN = 'A' MOVE 'D' TO CG-H-ORDEN
           ELSE                MOVE 'A' TO CG-H-ORDEN
           END-IF.

      *----------------------------------------------------------------*
      * 9000 - RUTINAS CICS ESTANDAR                                   *
      *----------------------------------------------------------------*
       9000-ENVIO-MAPA.
           EVALUATE TRUE
               WHEN ENVIO-ERASE
                   EXEC CICS SEND MAP(WC-MAP) MAPSET(WC-MAPSET)
                        FROM(BNKMAPHO) ERASE FREEKB END-EXEC
               WHEN ENVIO-DATAONLY
                   EXEC CICS SEND MAP(WC-MAP) MAPSET(WC-MAPSET)
                        FROM(BNKMAPHO) DATAONLY FREEKB END-EXEC
           END-EVALUATE.

       9100-RECIBIR-MAPA.
           EXEC CICS RECEIVE MAP(WC-MAP) MAPSET(WC-MAPSET)
                INTO(BNKMAPHI) RESP(WA-RESPUESTA-CICS) END-EXEC.

       9200-ENVIAR-AVISO-TEXTO.
           EXEC CICS SEND TEXT
               FROM (WS-MENSAJE-LOGN)
               LENGTH (LENGTH OF WS-MENSAJE-LOGN)
               ERASE
               FREEKB
           END-EXEC.

       9300-SALIR-A-LOGN.
           EXEC CICS XCTL PROGRAM(CS-PGM-LOGIN) END-EXEC.

       9400-VOLVER-AL-MENU.
           INITIALIZE CG-HISTORIAL.
           MOVE 'D' TO CG-H-ORDEN.
           MOVE '-' TO CG-H-UP-MORE.
           MOVE '-' TO CG-H-DOWN-MORE.
           INITIALIZE CH-COMUN.
           MOVE WC-TRANSACCION TO CH-TRANS-RETORNO.
           EXEC CICS XCTL PROGRAM(CS-PGM-MENU)
                COMMAREA(COMMAREA-GLOBAL)
           END-EXEC.

       9999-RETORNO-CICS.
           EXEC CICS RETURN TRANSID(WC-TRANSACCION)
               COMMAREA(COMMAREA-GLOBAL)
           END-EXEC.
