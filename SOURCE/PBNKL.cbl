       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PBNKL.
       AUTHOR. MARTIN RUBIO.
      *
      *****************************************************************
      ** PROGRAMA .........: PBNKL                                  **
      ** TITULO ...........: CONTROL DE ACCESO (LOGIN)              **
      ** **
      ** TIPO .............: ONLINE                                 **
      ** - LENGUAJE ...............: COBOL                          **
      ** - ENTORNO ................: CICS                           **
      ** - BASE DE DATOS ..........: DB2                            **
      ** **
      ** DESCRIPCION ......:                                        **
      ** **
      ** - Programa de inicio de sesion (Login).                    **
      ** Valida las credenciales (Usuario y Password) contra        **
      ** la tabla DB2 IBMUSER.CLIENTES.                             **
      ** **
      ** LOGICA PRINCIPAL:                                          **
      ** 1. Inicializacion (Borrado de mapa).                       **
      ** 2. Recepcion de mapa y validacion de teclas.               **
      ** 3. Consulta DB2 para recuperar password real.              **
      ** 4. Comparacion de password input vs real.                  **
      ** 5. Navegacion a Menu (PBNKM) o Mensaje Error.              **
      **                                                            **
      *****************************************************************
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * COPIES DE MAPAS, COMMAREA Y UTILIDADES                         *
      *----------------------------------------------------------------*
       COPY BNKLMP.
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY WSCOMM.

      *----------------------------------------------------------------*
      * DEFINICIONES DB2                                               *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLCLIEN END-EXEC.

      *----------------------------------------------------------------*
      * VARIABLES DE TRABAJO                                           *
      *----------------------------------------------------------------*
       01  WS-VARIABLES-TRABAJO.
           05 WA-RESPUESTA-CICS     PIC S9(8) COMP.
           05 WS-USUARIO-INPUT      PIC X(08).
           05 WS-PASSWORD-INPUT     PIC X(08).

       01  WS-CONSTANTES-PANTALLA.
           03 WC-TRANSACCION        PIC X(4)  VALUE 'BNKL'.
           03 WC-MAP                PIC X(8)  VALUE 'BNKMAPL'.
           03 WC-MAPSET             PIC X(8)  VALUE 'BNKLMP'.
           03 WC-MSG-SALIDA         PIC X(30)
              VALUE 'GRACIAS POR USAR EL SISTEMA'.

      * SWITCHES DE ESTADO
       01  WS-FLAGS.
           03 SW-ENVIO-MAPA         PIC X     VALUE '0'.
              88 ENVIO-ERASE                  VALUE '1'.
              88 ENVIO-DATAONLY               VALUE '2'.

       LINKAGE SECTION.
       01  DFHCOMMAREA              PIC X(100).

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
                   PERFORM 1000-INICIALIZACION

               WHEN EIBCALEN > 0
                   PERFORM 1100-PROCESAR-INTERACCION
           END-EVALUATE.

           PERFORM 9000-ENVIO-MAPA.
           PERFORM 9999-RETORNO-CICS.

      *----------------------------------------------------------------*
      * 1000 - LIMPIEZA PARA ENVIO DE MAPA                             *
      *----------------------------------------------------------------*
       1000-INICIALIZACION.
           MOVE LOW-VALUES TO BNKMAPLO.
           SET ENVIO-ERASE TO TRUE.

       1100-PROCESAR-INTERACCION.
           PERFORM 9100-RECIBIR-MAPA.

           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 2000-VALIDAR-ACCESO

               WHEN DFHCLEAR
               WHEN DFHPF3
                   PERFORM 9200-SALIR-DEL-SISTEMA

               WHEN OTHER
                   MOVE LOW-VALUES TO BNKMAPLO
                   PERFORM 9300-MANEJO-ERROR-TECLA
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 2000 - LOGICA DE VALIDACION                                    *
      *----------------------------------------------------------------*
       2000-VALIDAR-ACCESO.
           PERFORM 2100-PREPARAR-INPUTS.

           PERFORM 7000-CONSULTA-DB2.

           EVALUATE SQLCODE
               WHEN 0
                   PERFORM 2200-VERIFICAR-PASSWORD
               WHEN +100
                   MOVE 'USUARIO NO REGISTRADO' TO MSGFO
                   PERFORM 2900-SETEAR-ERROR-VISUAL
               WHEN OTHER
                   MOVE 'ERROR GENERAL DE BASE DE DATOS' TO MSGFO
                   PERFORM 2900-SETEAR-ERROR-VISUAL
           END-EVALUATE.

       2100-PREPARAR-INPUTS.
      * Limpieza de Low-Values que suelen llegar del mapa vacio
           INSPECT USERFI REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT PASSFI REPLACING ALL LOW-VALUES BY SPACES.

           MOVE FUNCTION UPPER-CASE(USERFI) TO WS-USUARIO-INPUT.
           MOVE FUNCTION UPPER-CASE(PASSFI) TO WS-PASSWORD-INPUT.

       2200-VERIFICAR-PASSWORD.
           IF HV-PASSWORD = WS-PASSWORD-INPUT
               PERFORM 9400-XCTL-MENU
           ELSE
               MOVE LOW-VALUES TO BNKMAPLO
               MOVE 'PASSWORD INCORRECTO' TO MSGFO
               PERFORM 2900-SETEAR-ERROR-VISUAL
           END-IF.

       2900-SETEAR-ERROR-VISUAL.
           MOVE DFHRED TO MSGFC.
           SET ENVIO-DATAONLY TO TRUE.

      *----------------------------------------------------------------*
      * 7000 - CONSULTA A DATOS (DB2)                                  *
      *----------------------------------------------------------------*
       7000-CONSULTA-DB2.
           EXEC SQL
                SELECT PASSWORD
                INTO :HV-PASSWORD
                FROM IBMUSER.CLIENTES
                WHERE USUARIO = :WS-USUARIO-INPUT
           END-EXEC.

      *----------------------------------------------------------------*
      * 9000 - RUTINAS CICS ESTANDAR                                   *
      *----------------------------------------------------------------*
       9000-ENVIO-MAPA.
           EVALUATE TRUE
               WHEN ENVIO-ERASE
                   EXEC CICS SEND MAP(WC-MAP) MAPSET(WC-MAPSET)
                        FROM(BNKMAPLO) ERASE FREEKB END-EXEC
               WHEN ENVIO-DATAONLY
                   EXEC CICS SEND MAP(WC-MAP) MAPSET(WC-MAPSET)
                        FROM(BNKMAPLO) DATAONLY FREEKB END-EXEC
           END-EVALUATE.

       9100-RECIBIR-MAPA.
           EXEC CICS RECEIVE MAP(WC-MAP) MAPSET(WC-MAPSET)
                INTO(BNKMAPLI) RESP(WA-RESPUESTA-CICS)
           END-EXEC.

       9200-SALIR-DEL-SISTEMA.
           EXEC CICS SEND TEXT
                FROM (WC-MSG-SALIDA)
                LENGTH (LENGTH OF WC-MSG-SALIDA)
                ERASE FREEKB
           END-EXEC
           EXEC CICS RETURN END-EXEC.

       9300-MANEJO-ERROR-TECLA.
           MOVE 'TECLA INVALIDA - USE ENTER' TO MSGFO.
           SET ENVIO-DATAONLY TO TRUE.

       9400-XCTL-MENU.
           INITIALIZE COMMAREA-GLOBAL.
           MOVE WS-USUARIO-INPUT     TO CG-M-USER.
           MOVE WC-TRANSACCION       TO CH-TRANS-RETORNO.

           EXEC CICS XCTL PROGRAM(CS-PGM-MENU)
                COMMAREA(COMMAREA-GLOBAL)
                RESP(WA-RESPUESTA-CICS)
           END-EXEC.

       9999-RETORNO-CICS.
           EXEC CICS RETURN TRANSID(WC-TRANSACCION)
               COMMAREA(COMMAREA-GLOBAL)
           END-EXEC.
