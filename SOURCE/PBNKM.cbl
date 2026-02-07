       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PBNKM.
       AUTHOR. MARTIN RUBIO.
      *
      *****************************************************************
      ** PROGRAMA .........: PBNKM                                  **
      ** TITULO ...........: MENU PRINCIPAL DEL SISTEMA             **
      ** **
      ** TIPO .............: ONLINE                                 **
      ** - LENGUAJE ...............: COBOL                          **
      ** - ENTORNO ................: CICS                           **
      ** - BASE DE DATOS ..........: N/A (Solo navegaciOn)          **
      ** **
      ** DESCRIPCION ......:                                        **
      **
      ** - Menu central de la aplicacion bancaria.                  **
      ** Permite al usuario seleccionar entre:                      **
      ** 1. Deposito/retiro de dinero (PBNKX)                       **
      ** 2. Transferir a otro usuario (PBNKT)                       **
      ** 3. Historial de Movimientos (PBNKH)                        **
      ** **
      ** Gestiona la navegacion mediante XCTL y controla            **
      ** el acceso no autorizado (si no viene de LOGN).             **
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * COPIES DE MAPAS, COMMAREA Y UTILIDADES                         *
      *----------------------------------------------------------------*
       COPY BNKMMP.
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY WSCOMM.

      *--- Variable faltante para codigos de respuesta CICS ---
       01  WA-RESPUESTA-CICS        PIC S9(8) COMP.

       01  WS-CONSTANTES.
           05 WS-MENSAJE-LOGN       PIC X(25)
              VALUE 'DEBE INGRESAR POR LOGN'.

       01  WC-CONSTANTES.
           03  WC-PROGRAMA          PIC X(8)  VALUE 'PBNKM'.
           03  WC-TRANSACCION       PIC X(4)  VALUE 'BNKM'.
           03  WC-MAP               PIC X(8)  VALUE 'BNKMAPM'.
           03  WC-MAPSET            PIC X(8)  VALUE 'BNKMMP'.

       01  WS-CONTROL.
           03 SW-ENVIO-MAPA         PIC X     VALUE '0'.
              88 ENVIO-ERASE                  VALUE '1'.
              88 ENVIO-DATAONLY               VALUE '2'.

       LINKAGE SECTION.
      * AJUSTE: Debe coincidir con el LOGIN (50 bytes)
       01  DFHCOMMAREA              PIC X(100).

       PROCEDURE DIVISION.
      *----------------------------------------------------------------
      * 0000: PROCESO PRINCIPAL (DRIVER)
      * Controla el flujo general del programa
      *----------------------------------------------------------------
       0000-PROCESO-TAREA.

           IF EIBCALEN > 0
               MOVE DFHCOMMAREA TO COMMAREA-GLOBAL
           END-IF.

           EVALUATE TRUE
      * 1. ERROR: Acceso directo sin Commarea (EIBCALEN = 0)
               WHEN EIBCALEN = 0
                   SET ESTADO-ERROR-LOGN TO TRUE
                   PERFORM 3100-ENVIAR-AVISO-TEXTO

      * 2. REDIRECCION: Usuario presiono Enter tras el error
               WHEN EIBCALEN > 0 AND ESTADO-ERROR-LOGN
                   PERFORM 9100-SALIR-A-LOGN

      * 3. CARGA INICIAL: Viene de LOGN o retorna de Sub-programa
               WHEN EIBTRNID NOT = 'BNKM'
                   MOVE LOW-VALUE          TO BNKMAPMO
                   INITIALIZE CH-COMUN
                   SET ENVIO-ERASE         TO TRUE
                   PERFORM 3000-ENVIO-MAPA

               WHEN EIBAID = DFHENTER
                 OR EIBAID = DFHPF1
                 OR EIBAID = DFHPF12
                    PERFORM 1000-PROCESO-NEGOCIO

               WHEN OTHER
                    MOVE LOW-VALUES TO BNKMAPMO
                    MOVE 'TECLA NO VALIDA' TO MSGO
                    SET ENVIO-DATAONLY TO TRUE
                    PERFORM 3000-ENVIO-MAPA

           END-EVALUATE.

      * Salida por defecto (vuelve a CICS esperando input)
           PERFORM 9000-RETORNO-CICS.

      *----------------------------------------------------------------
      * 1000: LOGICA DE NEGOCIO
      * Procesa la entrada del usuario
      *----------------------------------------------------------------
       1000-PROCESO-NEGOCIO.
           PERFORM 2000-RECIBE-MAPA.

           EVALUATE TRUE
               WHEN EIBAID = DFHENTER
                   PERFORM 1100-VALIDAR-OPCION

      * F12: Salir al Login
               WHEN EIBAID = DFHPF12
                   PERFORM 9100-SALIR-A-LOGN

               WHEN OTHER
                   MOVE LOW-VALUES TO BNKMAPMO
                   MOVE 'TECLA NO VALIDA' TO MSGO
                   SET ENVIO-DATAONLY TO TRUE
                   PERFORM 3000-ENVIO-MAPA
           END-EVALUATE.

      *----------------------------------------------------------------
      * 1100: VALIDACION DE OPCIONES
      * Determina a que programa redirigirnos
      *----------------------------------------------------------------
       1100-VALIDAR-OPCION.

           EVALUATE TRUE
               WHEN OPCIONI = '1'
                  MOVE CS-PGM-CONSULTA      TO CH-XCTL
                  PERFORM 8000-LLAMAR-PROGRAMA

               WHEN OPCIONI = '2'
                  MOVE CS-PGM-TRANSFERIR    TO CH-XCTL
                  PERFORM 8000-LLAMAR-PROGRAMA

               WHEN OPCIONI = '3'
                  MOVE CS-PGM-HISTORIAL     TO CH-XCTL
                  PERFORM 8000-LLAMAR-PROGRAMA

               WHEN OTHER
                   MOVE LOW-VALUES TO BNKMAPMO
                   MOVE ' SELECCIONE UNA OPCION CORRECTA' TO MSGO
                   SET ENVIO-DATAONLY TO TRUE
                   PERFORM 3000-ENVIO-MAPA

           END-EVALUATE.

      *----------------------------------------------------------------
      * 2000: ENTRADA DE DATOS (RECEIVE)
      *----------------------------------------------------------------
       2000-RECIBE-MAPA.
           EXEC CICS RECEIVE MAP('BNKMAPM')
                MAPSET('BNKMMP')
                INTO(BNKMAPMI)
                RESP(WA-RESPUESTA-CICS)
           END-EXEC.

      *----------------------------------------------------------------
      * 3000: SALIDA DE DATOS (SEND MAP)
      *----------------------------------------------------------------
       3000-ENVIO-MAPA.
           MOVE CG-M-USER TO NOMBREUSO
           MOVE DFHGREEN TO OPCIONC.
           EVALUATE TRUE
               WHEN ENVIO-ERASE
                   EXEC CICS SEND MAP('BNKMAPM')
                        MAPSET('BNKMMP')
                        ERASE
                        FREEKB
                   END-EXEC

               WHEN ENVIO-DATAONLY
                   EXEC CICS SEND MAP('BNKMAPM')
                        MAPSET('BNKMMP')
                        DATAONLY
                        FREEKB
                   END-EXEC
           END-EVALUATE.

      *----------------------------------------------------------------
      * 3100: SALIDA DE TEXTO (SEND TEXT)
      *----------------------------------------------------------------
       3100-ENVIAR-AVISO-TEXTO.
           EXEC CICS SEND TEXT
                FROM (WS-MENSAJE-LOGN)
                LENGTH (LENGTH OF WS-MENSAJE-LOGN)
                ERASE
                FREEKB
           END-EXEC.

      *----------------------------------------------------------------
      * 8000: NAVEGACION (XCTL)
      * Configura y ejecuta la llamada a otro programa
      *----------------------------------------------------------------
       8000-LLAMAR-PROGRAMA.

      * Preparar datos comunes para el programa llamado
           MOVE WC-TRANSACCION    TO CH-TRANSACCION
           MOVE WC-TRANSACCION    TO CH-TRANS-RETORNO
           MOVE WC-PROGRAMA       TO CH-PROGRAMA-RETORNO

      * XCTL al programa seleccionado (PBNKX, PBNKT, etc)
           EXEC CICS
               XCTL PROGRAM  (CH-XCTL)
                    COMMAREA (COMMAREA-GLOBAL)
                    RESP     (WA-RESPUESTA-CICS)
           END-EXEC.

      *----------------------------------------------------------------
      * 9000: RETORNO ESTANDAR
      * Vuelve a CICS esperando respuesta (Pseudo-conversacional)
      *----------------------------------------------------------------
       9000-RETORNO-CICS.
           EXEC CICS
               RETURN
               TRANSID('BNKM')
               COMMAREA(COMMAREA-GLOBAL)
           END-EXEC.

      *----------------------------------------------------------------
      * 9100: SALIDA FINAL
      * Redirige a LOGN (BNKL) inmediatamente sin pasar commarea
      *----------------------------------------------------------------
       9100-SALIR-A-LOGN.

           EXEC CICS XCTL
                PROGRAM (CS-PGM-LOGIN)
           END-EXEC.
