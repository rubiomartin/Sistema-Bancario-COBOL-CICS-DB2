       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBNKDB2X.
       AUTHOR. MARTIN RUBIO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * COPIES DE MAPAS, COMMAREA Y UTILIDADES                         *
      *----------------------------------------------------------------*
       COPY DFHAID.
       COPY DFHBMSCA.
      * COPY WSCOMM.
       COPY CPYVALWD.
       COPY CICSATTR.
      *----------------------------------------------------------------*
      * DEFINICIONES DB2
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLCLIEN END-EXEC.
           EXEC SQL INCLUDE DCLMOVIM END-EXEC.


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
      * VARIABLES LOCALES DE TRABAJO                                   *
      *----------------------------------------------------------------*
       01  WA-RESPUESTA-CICS        PIC S9(8) COMP.
       01  WS-MONTO-EDITADO         PIC Z.ZZZ.ZZZ.ZZ9,99.

       01  WS-VARIABLES-TRABAJO.
           05 WS-MSG-EXITO          PIC X(60).

       01  WS-CONTROL.
           05 SW-ENVIO-MAPA PIC X.
              88 ENVIO-ERASE                  VALUE '1'.
              88 ENVIO-DATAONLY               VALUE '2'.


       01  WS-CONSTANTES.
           05 WS-MENSAJE-LOGN       PIC X(25)
              VALUE 'DEBE INGRESAR POR LOGN'.

       01  WC-CONSTANTES.
           03  WC-PROGRAMA          PIC X(8)  VALUE 'PBNKX'.
           03  WC-TRANSACCION       PIC X(4)  VALUE 'BNKX'.
           03  WC-MAP               PIC X(8)  VALUE 'BNKMAPX'.
           03  WC-MAPSET            PIC X(8)  VALUE 'BNKXMP'.

       LINKAGE SECTION.

       PROCEDURE DIVISION.

       0000-PRINCIPAL.
           PERFORM 1000-RECIBIR-CONSULTA.

           PERFORM 7000-LEER-SALDO-DB2.

           PERFORM 1100-RECIBIR-OPERACION.

           PERFORM 9000-ENVIAR-RESPUESTA.

           EXEC CICS RETURN END-EXEC.


      *================================================================*
      * 1000 - RUTINAS DE RECEPCION DE CANALES                         *
      *================================================================*
       1000-RECIBIR-CONSULTA.
           EXEC CICS GET CONTAINER('DATOS-CLIENTE')
                         INTO(WS-DATOS-CONSULTA)
           END-EXEC.

       1100-RECIBIR-OPERACION.
           EXEC CICS GET CONTAINER('DATOS-ENTRADA')
                         INTO(WS-DATOS-OPERACION)
                         RESP(WA-RESPUESTA-CICS)
           END-EXEC.

           EVALUATE WA-RESPUESTA-CICS
               WHEN DFHRESP(NORMAL)
                   PERFORM 2500-EJECUTAR-NEGOCIO
               WHEN DFHRESP(CONTAINERERR)
                   CONTINUE
               WHEN OTHER
                   MOVE 'ERROR INTERNO CICS LECTURA CANAL'
                   TO WS-CONS-MENSAJE
           END-EVALUATE.


       2500-EJECUTAR-NEGOCIO.
           SET OPERACION-FALLIDA TO TRUE.
           MOVE SPACES TO WS-MSG-EXITO.

           IF SQLCODE NOT = 0
               MOVE 'ERROR CRITICO LECTURA SALDO' TO WS-CONS-MENSAJE
               SET OPERACION-FALLIDA TO TRUE
           ELSE
               MOVE 'S'      TO SW-SALDO-LEIDO
               MOVE SPACES   TO WS-CONS-MENSAJE

               EVALUATE WS-OPER-TIPO
                   WHEN 'D'
                       ADD WS-OPER-MONTO TO WS-CONS-SALDO
                           GIVING WS-OPER-SALDO-NUEVO
                       IF WS-OPER-SALDO-NUEVO > 99999999,99
                           MOVE ' ERROR: SALDO SUPERA LIMITE MAXIMO'
                             TO WS-OPER-MSGO
                           SET HAY-ERROR-VALIDACION TO TRUE
                       END-IF

                   WHEN 'R'
                       IF WS-CONS-SALDO < WS-OPER-MONTO
                           MOVE ' FONDOS INSUFICIENTES' TO WS-OPER-MSGO
                           SET HAY-ERROR-VALIDACION TO TRUE
                       ELSE
                           SUBTRACT WS-OPER-MONTO
                            FROM WS-CONS-SALDO GIVING
                             WS-OPER-SALDO-NUEVO
                       END-IF
               END-EVALUATE

               IF HAY-ERROR-VALIDACION
                   SET OPERACION-FALLIDA TO TRUE
               ELSE
                   PERFORM 3000-PERSISTENCIA-DATOS
               END-IF

               IF OPERACION-EXITOSA
                   MOVE WS-MSG-EXITO TO WS-OPER-MSGO
                   MOVE WS-OPER-SALDO-NUEVO TO WS-CONS-SALDO
               END-IF
           END-IF.


       3000-PERSISTENCIA-DATOS.
           PERFORM 7100-UPDATE-SALDO.
           IF SQLCODE = 0
               PERFORM 7200-INSERTAR-HISTORIAL
               IF SQLCODE = 0
                   EXEC CICS SYNCPOINT END-EXEC

                   SET OPERACION-EXITOSA TO TRUE
                   IF WS-OPER-TIPO = 'D'
                       MOVE ' DEPOSITO EXITOSO' TO WS-MSG-EXITO
                   ELSE
                       MOVE ' RETIRO EXITOSO'   TO WS-MSG-EXITO
                   END-IF
               ELSE
                   EXEC CICS SYNCPOINT ROLLBACK END-EXEC
                   MOVE ' ERROR HISTORIAL' TO WS-OPER-MSGO
               END-IF
           ELSE
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
               MOVE ' ERROR UPDATE' TO WS-OPER-MSGO
           END-IF.

       7000-LEER-SALDO-DB2.
           EXEC SQL SELECT SALDO INTO :HV-SALDO
               FROM IBMUSER.CLIENTES WHERE USUARIO = :WS-CONS-USER
           END-EXEC.

           IF SQLCODE = 0
               MOVE HV-SALDO TO WS-CONS-SALDO
           END-IF.

       7100-UPDATE-SALDO.
           MOVE WS-OPER-SALDO-NUEVO TO HV-SALDO.
           EXEC SQL UPDATE IBMUSER.CLIENTES SET SALDO = :HV-SALDO
               WHERE USUARIO = :WS-CONS-USER
           END-EXEC.

       7200-INSERTAR-HISTORIAL.
           IF WS-OPER-TIPO = 'R'
               MOVE 'Z'          TO HV-TIPO-OPER
           ELSE
               MOVE WS-OPER-TIPO TO HV-TIPO-OPER
           END-IF.
           MOVE WS-OPER-MONTO TO HV-MONTO.
           MOVE WS-CONS-USER  TO HV-USUARIO-MOV.
           EXEC SQL INSERT INTO IBMUSER.MOVIMIENTOS
               (USUARIO, TIPO_OPER, MONTO, FECHA)
               VALUES (:HV-USUARIO-MOV, :HV-TIPO-OPER, :HV-MONTO,
                CURRENT TIMESTAMP)
           END-EXEC.

      *================================================================*
      * 9000 - DEVOLUCION DE DATOS AL FRONT-END                        *
      *================================================================*
       9000-ENVIAR-RESPUESTA.
           EXEC CICS PUT CONTAINER('DATOS-CLIENTE')
                         FROM(WS-DATOS-CONSULTA)
           END-EXEC.

           IF WA-RESPUESTA-CICS = DFHRESP(NORMAL)
               EXEC CICS PUT CONTAINER('DATOS-ENTRADA')
                             FROM(WS-DATOS-OPERACION)
               END-EXEC
           END-IF.
