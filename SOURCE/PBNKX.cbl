       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PBNKX.
       AUTHOR. MARTIN RUBIO.
      *
      *****************************************************************
      ** PROGRAMA .........: PBNKX                                  **
      ** TITULO ...........: TRANSACCION DE DEPOSITOS Y RETIROS     **
      ** **
      ** TIPO .............: ONLINE                                 **
      ** - LENGUAJE ...............: COBOL                          **
      ** - ENTORNO ................: CICS                           **
      ** - BASE DE DATOS ..........: DB2                            **
      ** **
      ** DESCRIPCION ......:                                        **
      ** **
      ** - Permite realizar operaciones de Caja.                    **
      ** Tipos de operacion:                                        **
      ** 'D' -> Deposito (Suma al saldo).                           **
      ** 'R' -> Retiro   (Resta al saldo si hay fondos).            **
      ** **
      ** Actualiza la tabla de CLIENTES y genera un registro        **
      ** en la tabla de MOVIMIENTOS.                                **
      ** Incluye doble verificacion (Confirmacion de usuario)       **
      ** Utiliza COMMIT/ROLLBACK (SYNCPOINT) para integridad.       **

      ** - 9/6/2026 Separé la logica del manejo del mapa con la     **
      ** interacciones con la base de datos en dos programas:       **
      ** PBNKX PARA EL MANEJO DEL MAPA Y CONFIRMACION               **
      ** PBNKDB2X PARA LAS INTERACCIONES CON DB2                    ** 
      ** estos dos programas se comunican a traves de channels      **
      ** y containers                                               **
      *****************************************************************
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * COPIES DE MAPAS Y UTILIDADES                                   *
      *----------------------------------------------------------------*
       COPY BNKXMP.
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY WSCOMM.
       COPY CPYVALWD.
       COPY CICSATTR.

      *----------------------------------------------------------------*
      * ESTRUCTURAS DE CONTENEDORES (CANALES)                          *
      *----------------------------------------------------------------*
       01  WS-DATOS-CONSULTA.
           05 WS-CONS-USER          PIC X(08) VALUE SPACES.
           05 WS-CONS-SALDO         PIC 9(10)V99.
           05 WS-CONS-MENSAJE       PIC X(60).
           05 SW-SALDO-LEIDO        PIC X(01).

       01  WS-DATOS-OPERACION.
           05 WS-OPER-TIPO          PIC X(01).
           05 WS-OPER-MONTO         PIC 9(10)V99.
           05 WS-OPER-SALDO-NUEVO   PIC 9(10)V99.
           05 WS-OPER-MSGO          PIC X(60).
           
           05 WS-BANDERAS-CONTROL.
              10 SW-RESULTADO       PIC X(01).
                 88 OPERACION-EXITOSA         VALUE 'S'.
                 88 OPERACION-FALLIDA         VALUE 'N'.
              10 SW-ERRORES         PIC X(01).
                 88 HAY-ERROR-VALIDACION      VALUE 'S'.
                 88 NO-HAY-ERRORES            VALUE 'N'.

      *----------------------------------------------------------------*
      * VARIABLES DE CONTROL LOCALES                                   *
      *----------------------------------------------------------------*
       01  WA-RESPUESTA-CICS       PIC S9(8) COMP.
       01  WS-MONTO-EDITADO        PIC Z.ZZZ.ZZZ.ZZ9,99.

       01  WS-VARIABLES-TRABAJO.
           05 WS-MONTO-DECIMAL     PIC 9(10)V99.
           05 WS-SALDO-PANTALLA    PIC 9(10)V99.

       01  WS-CONTROL.
           05 SW-ENVIO-MAPA PIC X.
              88 ENVIO-ERASE                  VALUE '1'.
              88 ENVIO-DATAONLY               VALUE '2'.

       01  WS-CONSTANTES.
           05 WS-MENSAJE-LOGN      PIC X(25)
              VALUE 'DEBE INGRESAR POR LOGN'.

       01  WC-CONSTANTES.
           03  WC-PROGRAMA         PIC X(8)  VALUE 'PBNKX'.
           03  WC-TRANSACCION      PIC X(4)  VALUE 'BNKX'.
           03  WC-MAP              PIC X(8)  VALUE 'BNKMAPX'.
           03  WC-MAPSET           PIC X(8)  VALUE 'BNKXMP'.

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(126).

       PROCEDURE DIVISION.

      *================================================================*
      * 0000 - CONTROL PRINCIPAL                                       *
      *================================================================*
       0000-PROCESO-TAREA.
           IF EIBCALEN > 0
               MOVE DFHCOMMAREA TO COMMAREA-GLOBAL
           END-IF.

           EVALUATE TRUE
               WHEN EIBCALEN = 0
                   SET ESTADO-ERROR-LOGN TO TRUE
                   PERFORM 9200-ENVIAR-AVISO-TEXTO

               WHEN EIBCALEN > 0 AND ESTADO-ERROR-LOGN
                   PERFORM 9100-SALIR-A-LOGN

               WHEN EIBTRNID NOT = WC-TRANSACCION
                   PERFORM 1000-PREPARAR-DATOS

               WHEN OTHER
                   PERFORM 2000-PROCESAR-INTERACCION
           END-EVALUATE.

           PERFORM 9999-RETORNO-CICS.

      *================================================================*
      * 1000 - INICIALIZACION                                          *
      *================================================================*
       1000-PREPARAR-DATOS.
           MOVE LOW-VALUES TO BNKMAPXO.

           PERFORM 1234-CONSULTA-SALDO.

           IF WS-CONS-SALDO > 0 OR WS-CONS-MENSAJE = SPACES
               MOVE WS-CONS-SALDO TO SALDOO
               MOVE WS-CONS-SALDO TO WS-SALDO-PANTALLA
               MOVE 'S'           TO SW-SALDO-LEIDO
           ELSE
               MOVE 0 TO WS-SALDO-PANTALLA
               MOVE ' ADVERTENCIA: NO SE PUDO LEER SALDO INICIAL'
                 TO MSGO
               MOVE ATTR-RED TO MSGC
           END-IF.

           SET ENVIO-ERASE TO TRUE
           PERFORM 4000-ENVIO-MAPA.

      *================================================================*
      * 2000 - LOGICA DE UI Y VALIDACIONES BASICAS                     *
      *================================================================*
       2000-PROCESAR-INTERACCION.
           PERFORM 4100-RECIBIR-MAPA.
           SET ENVIO-DATAONLY TO TRUE.

           EVALUATE TRUE
               WHEN EIBAID = DFHENTER
                   PERFORM 2100-ACCION-MAPA
               WHEN EIBAID = DFHPF3
                   PERFORM 2200-TRATAR-SALIDA
               WHEN OTHER
                   MOVE SPACES TO MSGO
                   MOVE ' TECLA INVALIDA' TO MSGO
           END-EVALUATE.

           PERFORM 4000-ENVIO-MAPA.

       2100-ACCION-MAPA.
           PERFORM 2300-VALIDAR-CAMPOS.

           IF NO-HAY-ERRORES
               IF CONFIRMACION-PENDIENTE
                   PERFORM 2500-EJECUTAR-NEGOCIO
               ELSE
                   PERFORM 2400-PREPARAR-CONFIRMACION
               END-IF
           END-IF.

       2200-TRATAR-SALIDA.
           IF CONFIRMACION-PENDIENTE
               MOVE 'N' TO SW-CONFIRMACION
               MOVE SPACES TO MSGO
               MOVE SPACES TO CONFRMO
               MOVE ' OPERACION CANCELADA' TO MSGO
               PERFORM 4200-DESBLOQUEAR-CAMPOS
           ELSE
               PERFORM 9000-VOLVER-AL-MENU
           END-IF.

       2300-VALIDAR-CAMPOS.
           MOVE 'N' TO SW-ERRORES.
           MOVE FUNCTION UPPER-CASE(TIPOOPERI) TO TIPOOPERI.

           MOVE MONTOI TO WS-VAL-ENTRADA.
           PERFORM 9900-RUTINA-VALIDAR-NUMERO.

           IF NO-HAY-ERRORES
               MOVE WS-VAL-SALIDA-V TO WS-MONTO-DECIMAL
           END-IF.

           MOVE SPACES TO MSGO.

           EVALUATE TRUE
               WHEN VAL-HAY-ERROR
                   MOVE ' ERROR: MONTO INVALIDO' TO MSGO
                   SET HAY-ERROR-VALIDACION TO TRUE
               WHEN WS-VAL-SALIDA <= 0
                   MOVE ' ERROR: MONTO DEBE SER > 0' TO MSGO
                   SET HAY-ERROR-VALIDACION TO TRUE
               WHEN TIPOOPERI NOT = 'D' AND TIPOOPERI NOT = 'R'
                   MOVE ' ERROR: OPERACION INVALIDA' TO MSGO
                   SET HAY-ERROR-VALIDACION TO TRUE
           END-EVALUATE.

           IF HAY-ERROR-VALIDACION
               MOVE 'N' TO SW-CONFIRMACION
           END-IF.

       2400-PREPARAR-CONFIRMACION.
           MOVE SPACES TO MSGO
           MOVE ' CONFIRME: ENTER=SI PF3=CANCELAR' TO MSGO.
           MOVE ATTR-YELLOW TO MSGC.

           MOVE WS-MONTO-DECIMAL TO WS-MONTO-EDITADO.
           MOVE WS-MONTO-EDITADO TO WS-TRIM-STR-IN.
           MOVE 16               TO WS-TRIM-MAX-LEN.
           PERFORM 9950-ELIMINAR-ESPACIOS-IZQ.
           INITIALIZE CONFRMO.

           IF TIPOOPERI = 'D'
               STRING ' ¿SEGURO QUIERE DEPOSITAR $' DELIMITED BY SIZE
               WS-TRIM-STR-OUT                DELIMITED BY SPACES
               '?'                            DELIMITED BY SIZE
               INTO CONFRMO
           ELSE
               STRING ' ¿SEGURO QUIERE RETIRAR $' DELIMITED BY SIZE
               WS-TRIM-STR-OUT                DELIMITED BY SPACES
               '?'                            DELIMITED BY SIZE
               INTO CONFRMO
           END-IF.

           MOVE 'S' TO SW-CONFIRMACION.
           MOVE ATTR-PROT TO TIPOOPERA.
           MOVE ATTR-PROT TO MONTOA.

       2500-EJECUTAR-NEGOCIO.
           MOVE TIPOOPERI        TO WS-OPER-TIPO.
           MOVE WS-MONTO-DECIMAL TO WS-OPER-MONTO.

           PERFORM 1234-REALIZAR-OPERACION.

      * Evaluamos la respuesta que nos devolvio el backend
           IF OPERACION-EXITOSA
               MOVE LOW-VALUES      TO BNKMAPXO
               MOVE SPACES          TO CONFRMO
               MOVE WS-OPER-MSGO    TO MSGO
               MOVE ATTR-GREEN      TO MSGC
               MOVE SPACES          TO MONTOO
               MOVE SPACES          TO TIPOOPERO
               MOVE WS-OPER-SALDO-NUEVO TO SALDOO
               MOVE WS-OPER-SALDO-NUEVO TO WS-SALDO-PANTALLA

               PERFORM 4200-DESBLOQUEAR-CAMPOS
               SET ENVIO-ERASE      TO TRUE
               MOVE 'N'             TO SW-CONFIRMACION
           ELSE
               MOVE WS-OPER-MSGO    TO MSGO
               MOVE ATTR-RED        TO MSGC
               PERFORM 4200-DESBLOQUEAR-CAMPOS
               MOVE 'N'             TO SW-CONFIRMACION
           END-IF.

      *================================================================*
      * 4000 - MANEJO DE MAPAS                                         *
      *================================================================*
       4000-ENVIO-MAPA.
           IF SW-SALDO-LEIDO = 'N'
               PERFORM 1234-CONSULTA-SALDO
               MOVE WS-CONS-SALDO TO WS-SALDO-PANTALLA
           END-IF.

           MOVE CG-M-USER TO NOMBREUSO
           MOVE WS-SALDO-PANTALLA TO SALDOO.

           EVALUATE TRUE
               WHEN ENVIO-ERASE
                   EXEC CICS SEND MAP('BNKMAPX') MAPSET('BNKXMP')
                        FROM(BNKMAPXO) ERASE FREEKB END-EXEC
               WHEN ENVIO-DATAONLY
                   EXEC CICS SEND MAP('BNKMAPX') MAPSET('BNKXMP')
                        FROM(BNKMAPXO) DATAONLY FREEKB END-EXEC
           END-EVALUATE.

       4100-RECIBIR-MAPA.
           EXEC CICS RECEIVE MAP('BNKMAPX') MAPSET('BNKXMP')
               INTO(BNKMAPXI) RESP(WA-RESPUESTA-CICS) END-EXEC.

       4200-DESBLOQUEAR-CAMPOS.
           MOVE ATTR-UNPROT-MDT     TO TIPOOPERA.
           MOVE ATTR-UNPROT-NUM-MDT TO MONTOA.

      *================================================================*
      * 8000 - RUTINAS DE COMUNICACION CON EL BACKEND (CANALES)        *
      *================================================================*
       1234-CONSULTA-SALDO.
           MOVE CG-M-USER TO WS-CONS-USER.

           EXEC CICS PUT CONTAINER('DATOS-CLIENTE')
                         CHANNEL('CH-CAJA')
                         FROM(WS-DATOS-CONSULTA)
           END-EXEC.

           EXEC CICS LINK PROGRAM('PBNKDB2X') CHANNEL('CH-CAJA')
           END-EXEC.

           EXEC CICS GET CONTAINER('DATOS-CLIENTE')
                         CHANNEL('CH-CAJA')
                         INTO(WS-DATOS-CONSULTA)
           END-EXEC.

       1234-REALIZAR-OPERACION.
           MOVE CG-M-USER TO WS-CONS-USER.

           EXEC CICS PUT CONTAINER('DATOS-CLIENTE')
                         CHANNEL('CH-CAJA')
                         FROM(WS-DATOS-CONSULTA)
           END-EXEC.

           EXEC CICS PUT CONTAINER('DATOS-ENTRADA')
                         CHANNEL('CH-CAJA')
                         FROM(WS-DATOS-OPERACION)
           END-EXEC.

           EXEC CICS LINK PROGRAM('PBNKDB2X') CHANNEL('CH-CAJA')
           END-EXEC.

           EXEC CICS GET CONTAINER('DATOS-CLIENTE')
                         CHANNEL('CH-CAJA')
                         INTO(WS-DATOS-CONSULTA)
           END-EXEC.

           EXEC CICS GET CONTAINER('DATOS-ENTRADA')
                         CHANNEL('CH-CAJA')
                         INTO(WS-DATOS-OPERACION)
           END-EXEC.

      *================================================================*
      * 9000 - NAVEGACION Y SALIDA                                     *
      *================================================================*
       9000-VOLVER-AL-MENU.
      *    INITIALIZE CH-COMUN.
           MOVE 'BNKX' TO CH-TRANS-RETORNO.
           EXEC CICS XCTL PROGRAM(CS-PGM-MENU)
               COMMAREA(COMMAREA-GLOBAL) RESP(WA-RESPUESTA-CICS)
           END-EXEC.

       9100-SALIR-A-LOGN.
           EXEC CICS XCTL PROGRAM(CS-PGM-LOGIN) END-EXEC.

       9200-ENVIAR-AVISO-TEXTO.
           EXEC CICS SEND TEXT
               FROM(WS-MENSAJE-LOGN) LENGTH(LENGTH OF WS-MENSAJE-LOGN)
               ERASE FREEKB
           END-EXEC.

       9999-RETORNO-CICS.
           EXEC CICS RETURN TRANSID(WC-TRANSACCION)
               COMMAREA(COMMAREA-GLOBAL)
           END-EXEC.

       COPY CPYVALPD.
