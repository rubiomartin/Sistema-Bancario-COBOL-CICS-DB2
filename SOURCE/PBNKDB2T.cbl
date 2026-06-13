       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBNKDB2T.
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

       01  WS-DATOS-OPERACION.
           05 WS-USER-DEST          PIC X(08) VALUE SPACES.
           05 WS-CONS-DEST          PIC X.       
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
           05 WS-MONTO-DECIMAL      PIC 9(10)V99.
           05 WS-SALDO-ACTUAL       PIC 9(10)V99.
           05 WS-SALDO-DESTINO-AUX  PIC 9(10)V99.


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

           PERFORM 7000-LEER-SALDO-ORIGEN.

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
                   IF WS-CONS-DEST = 'S'
                       PERFORM 2100-VALIDAR-SOLO-DESTINO
                   ELSE
                       IF WS-SALDO-ACTUAL < WS-OPER-MONTO
                           MOVE ' FONDOS INSUFICIENTES' TO WS-OPER-MSGO
                           SET HAY-ERROR-VALIDACION TO TRUE
                       ELSE
                           PERFORM 2600-VERIFICAR-LIMITE-DESTINO
                       END-IF
                       
                       IF NOT HAY-ERROR-VALIDACION
                           PERFORM 3000-PERSISTENCIA-DATOS  
                       END-IF                             
                   END-IF
               WHEN DFHRESP(CONTAINERERR)
                   CONTINUE
               WHEN OTHER
                   MOVE 'ERROR INTERNO CICS LECTURA CANAL'
                     TO WS-OPER-MSGO
           END-EVALUATE.


       2100-VALIDAR-SOLO-DESTINO.
           PERFORM 7300-VALIDAR-DESTINO-DB2.
           
           IF SQLCODE = 100 OR SQLCODE NOT = 0
               SET HAY-ERROR-VALIDACION TO TRUE
               MOVE ' ERROR: USUARIO DESTINO NO EXISTE' TO WS-OPER-MSGO
           ELSE
               SET NO-HAY-ERRORES TO TRUE
               MOVE SPACES TO WS-OPER-MSGO
           END-IF.
      *================================================================*
      * 2600 - VERIFICACION LIMITE DESTINO                             *
      *================================================================*
       2600-VERIFICAR-LIMITE-DESTINO.
           PERFORM 7300-VALIDAR-DESTINO-DB2.

           COMPUTE WS-SALDO-DESTINO-AUX = HV-SALDO + WS-OPER-MONTO

           IF WS-SALDO-DESTINO-AUX > 99999999,99
               MOVE ' ERROR: DESTINATARIO NO PUEDE RECIBIR TANTO MONTO'
                  TO WS-OPER-MSGO
               SET HAY-ERROR-VALIDACION TO TRUE
           END-IF.

      *================================================================*
      * 3000 - PERSISTENCIA (ACID)                                     *
      *================================================================*
       3000-PERSISTENCIA-DATOS.
           SUBTRACT WS-OPER-MONTO FROM WS-SALDO-ACTUAL
               GIVING WS-OPER-SALDO-NUEVO.

           PERFORM 7100-UPDATE-SALDO-ORIGEN.

           IF SQLCODE = 0
               PERFORM 7400-UPDATE-SALDO-DESTINO
               IF SQLCODE = 0
      * ---------------------------------------------------------
      * 1. REGISTRO PARA EL REMITENTE (Salida de dinero)
      * ---------------------------------------------------------
                   MOVE 'T'              TO HV-TIPO-OPER
                   MOVE WS-OPER-MONTO TO HV-MONTO
                   MOVE WS-CONS-USER        TO HV-USUARIO-MOV
                   MOVE WS-USER-DEST  TO HV-USUARIO-REL
                   PERFORM 7200-INSERTAR-HISTORIAL

                   IF SQLCODE = 0
      * ---------------------------------------------------------
      * 2. REGISTRO PARA EL DESTINATARIO (Entrada de dinero)
      * ---------------------------------------------------------
                       MOVE 'R'              TO HV-TIPO-OPER
                       MOVE WS-OPER-MONTO TO HV-MONTO
                       MOVE WS-USER-DEST  TO HV-USUARIO-MOV
                       MOVE WS-CONS-USER        TO HV-USUARIO-REL

                       PERFORM 7200-INSERTAR-HISTORIAL

                       IF SQLCODE = 0
                           EXEC CICS SYNCPOINT END-EXEC
                           SET OPERACION-EXITOSA TO TRUE
                           MOVE ' TRANSFERENCIA EXITOSA' TO WS-OPER-MSGO
                           MOVE WS-OPER-SALDO-NUEVO TO WS-CONS-SALDO
                       ELSE
                           EXEC CICS SYNCPOINT ROLLBACK END-EXEC
                           MOVE ' ERROR HISTORIAL DESTINO'
                            TO WS-OPER-MSGO
                       END-IF
                   ELSE
                       EXEC CICS SYNCPOINT ROLLBACK END-EXEC
                       MOVE ' ERROR HISTORIAL ORIGEN' TO WS-OPER-MSGO
                   END-IF
               ELSE
                   EXEC CICS SYNCPOINT ROLLBACK END-EXEC
                   MOVE ' ERROR AL ACREDITAR DESTINO' TO WS-OPER-MSGO
               END-IF
           ELSE
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
               MOVE ' ERROR AL DEBITAR ORIGEN' TO WS-OPER-MSGO
           END-IF.

      *================================================================*
      * 7000 - ACCESO A DATOS (DB2)                                    *
      *================================================================*
       7000-LEER-SALDO-ORIGEN.
           EXEC SQL SELECT SALDO INTO :HV-SALDO
               FROM IBMUSER.CLIENTES WHERE USUARIO = :WS-CONS-USER
           END-EXEC.
           IF SQLCODE = 0
               MOVE HV-SALDO TO WS-CONS-SALDO
               MOVE HV-SALDO TO WS-SALDO-ACTUAL
           ELSE
               CONTINUE
           END-IF.    

       7100-UPDATE-SALDO-ORIGEN.
           MOVE WS-OPER-SALDO-NUEVO TO HV-SALDO.
           EXEC SQL UPDATE IBMUSER.CLIENTES SET SALDO = :HV-SALDO
               WHERE USUARIO = :WS-CONS-USER
           END-EXEC.

       7200-INSERTAR-HISTORIAL.
           EXEC SQL INSERT INTO IBMUSER.MOVIMIENTOS
               (USUARIO, TIPO_OPER, MONTO, FECHA, USUARIO_REL)
               VALUES (:HV-USUARIO-MOV, :HV-TIPO-OPER, :HV-MONTO,
                CURRENT TIMESTAMP, :HV-USUARIO-REL)
           END-EXEC.

       7300-VALIDAR-DESTINO-DB2.
           EXEC SQL SELECT SALDO INTO :HV-SALDO
               FROM IBMUSER.CLIENTES WHERE USUARIO = :WS-USER-DEST
           END-EXEC.

       7400-UPDATE-SALDO-DESTINO.
           MOVE WS-OPER-MONTO TO HV-MONTO.
           EXEC SQL UPDATE IBMUSER.CLIENTES
               SET SALDO = SALDO + :HV-MONTO
               WHERE USUARIO = :WS-USER-DEST
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

