       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBNKT.
       AUTHOR. MARTIN RUBIO.
      *================================================================*
      * TITULO   : TRANSFERENCIAS ENTRE CUENTAS                        *
      * ENTORNO  : IBM i (OS/400) - ILE COBOL                          *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS CRT.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PANTALLA-TRANSF ASSIGN TO WORKSTATION-BNKTMP-SI
                  ORGANIZATION IS TRANSACTION
                  ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD PANTALLA-TRANSF.
       01 TRANSF-RECORD PIC X(200).

       WORKING-STORAGE SECTION.
       01 WS-TRANST-I.
          COPY DDS-BNKMAPT-I OF BNKTMP.

       01 WS-TRANST-O.
          COPY DDS-BNKMAPT-O OF BNKTMP.

       01 WS-CONFIRM-I.
          COPY DDS-WCONFTR-I OF BNKTMP.

       01 WS-CONFIRM-O.
          COPY DDS-WCONFTR-O OF BNKTMP.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01  DCLCLIEN.
           10 HV-USUARIO           PIC X(8).
           10 HV-PASSWORD          PIC X(8).
           10 HV-NOMBRE            PIC X(20).
           10 HV-SALDO             PIC S9(8)V9(2) USAGE COMP-3.

       01  DCLMOVIMIENTOS.
      *                       ID_MOV
           10 HV-ID-MOV            PIC S9(9) USAGE COMP.
      *                       USUARIO
           10 HV-USUARIO-MOV       PIC X(8).
      *                       TIPO_OPER
           10 HV-TIPO-OPER         PIC X(1).
      *                       MONTO
           10 HV-MONTO             PIC S9(8)V9(2) USAGE COMP-3.
      *                       FECHA
           10 HV-FECHA             PIC X(26).
      *                       USUARIO_REL
           10 HV-USUARIO-REL       PIC X(8).

       01   SW-CONTROL.
          05 SW-FIN-PROG         PIC X(1) VALUE 'N'.
             88 FIN-PROG         VALUE 'S'.

        01 WS-TXT-CONFIRMACION.
          05 FILLER          PIC X(22) VALUE "¿Confirma transferir $".
          05 WS-MONTO-TXT    PIC Z(7)9.99.
          05 FILLER          PIC X(3)  VALUE " a ".
          05 WS-USR-DEST-TXT PIC X(8).
          05 FILLER          PIC X(1)  VALUE "?".

       01 WS-VARIABLES-TRABAJO.
          05 WS-SALDO-ORIGEN     PIC S9(8)V99 COMP-3.
          05 WS-SALDO-DESTINO    PIC S9(8)V99 COMP-3.
          05 WS-SALDO-NUEVO        PIC S9(11)V99.
          05 WS-SALDO-CALCULO      PIC 9(10)V99.
          05 WS-USER-DESTINO       PIC X(8).
          05 WS-USER-ORIGEN        PIC X(8).

      *--- INDICADORES DE COMUNICACION CON DDS ---*
       01 INDICADORES-PANTALLA.
          05 IND03               PIC 1 INDIC 03.
             88 IND-SALIR        VALUE B"1".

       LINKAGE SECTION.
      *================================================================
      * AREA DE COMUNICACION GLOBAL (Recibida de PBNKM)
      *================================================================
       01 COMMAREA-GLOBAL.
          03 CG-CONTEXTO-USUARIO.
             05 CG-M-USER               PIC X(08).
          03 CG-NAVEGACION.
             05 CH-TRANS-RETORNO        PIC X(04).
             05 CH-PROGRAMA-RETORNO     PIC X(08).
             05 CH-XCTL                 PIC X(08).
          03 CG-ESTADOS.
             05 CG-ENTRADA-INCORRECTA   PIC X(01).
                88 ESTADO-ERROR-LOGN    VALUE 'E'.
                88 ESTADO-NORMAL        VALUE 'N'.
             05 SW-CONFIRMACION         PIC X(01).
                88 CONFIRMACION-PENDIENTE VALUE 'S'.
      *    03 CG-HISTORIAL.
      *       05 CG-H-OPER               PIC X(01).
      *       05 CG-H-ORDEN              PIC X(01).
      *       05 CG-H-ID1                PIC S9(9) COMP.
      *       05 CG-H-ID4                PIC S9(9) COMP.
      *       05 CG-H-UP-MORE            PIC X(01).
      *       05 CG-H-DW-MORE            PIC X(01).



       PROCEDURE DIVISION USING COMMAREA-GLOBAL.
       0000-MAIN-LOGIC.

           EXEC SQL
               SET OPTION COMMIT = *CHG
           END-EXEC.


           OPEN I-O PANTALLA-TRANSF.

           MOVE 'N' TO SW-FIN-PROG.

           INITIALIZE WS-TRANST-I.
           INITIALIZE WS-TRANST-O.
           INITIALIZE WS-CONFIRM-I.
           INITIALIZE WS-CONFIRM-O.


           MOVE CG-M-USER TO NOMBREUS OF BNKMAPT-O.
           PERFORM 7000-LEER-SALDO-ACTUAL.

           PERFORM UNTIL FIN-PROG
               WRITE TRANSF-RECORD FROM WS-TRANST-O
                FORMAT  IS "BNKMAPT"
                INDICATORS ARE INDICADORES-PANTALLA
               END-WRITE

               READ PANTALLA-TRANSF RECORD INTO WS-TRANST-I
                FORMAT IS "BNKMAPT"
                INDICATORS ARE INDICADORES-PANTALLA
                AT END CONTINUE
               END-READ

               IF IND-SALIR
                  SET FIN-PROG TO TRUE
               ELSE
                  PERFORM 7000-LEER-SALDO-ACTUAL
                  PERFORM 1000-PROCESAR-TRANSFERENCIA
               END-IF
           END-PERFORM.

           CLOSE PANTALLA-TRANSF.
           GOBACK.

       1000-PROCESAR-TRANSFERENCIA.
           MOVE SPACES TO MSGO OF BNKMAPT-O.

           IF USRDEST OF BNKMAPT-I = SPACES OR
              USRDEST OF BNKMAPT-I = CG-M-USER
              MOVE "USUARIO DESTINO INVALIDO." TO MSGO OF BNKMAPT-O
              EXIT PARAGRAPH
           END-IF.



           IF MONTOI OF BNKMAPT-I <= 0
              MOVE "EL MONTO DEBE SER MAYOR A CERO." TO MSGO
               OF BNKMAPT-O
              EXIT PARAGRAPH
           END-IF.

           PERFORM 7000-LEER-SALDO-ORIGEN.
           IF MONTOI OF BNKMAPT-I > WS-SALDO-ORIGEN
              MOVE "FONDOS INSUFICIENTES." TO MSGO OF BNKMAPT-O
              EXIT PARAGRAPH
           END-IF.



           PERFORM 7300-VALIDAR-DESTINO-DB2.
           IF SQLCODE = 0
               MOVE MONTOI OF BNKMAPT-I TO WS-SALDO-CALCULO
               COMPUTE WS-SALDO-NUEVO = HV-SALDO + WS-SALDO-CALCULO
               IF WS-SALDO-NUEVO > 99999999.99
                   MOVE
                   'ERROR: DESTINATARIO NO PUEDE RECIBIR TANTO MONTO'
                   TO MSGO OF BNKMAPT-O
                   EXIT PARAGRAPH
               END-IF
           ELSE
               MOVE "EL USUARIO DESTINO NO EXISTE EN EL SISTEMA."
                 TO MSGO OF BNKMAPT-O
               EXIT PARAGRAPH
           END-IF.

           MOVE MONTOI OF BNKMAPT-I  TO WS-MONTO-TXT.
           MOVE USRDEST OF BNKMAPT-I TO WS-USR-DEST-TXT.
           MOVE WS-TXT-CONFIRMACION  TO MSGCONF OF WCONFTR-O.

           WRITE TRANSF-RECORD FROM WS-CONFIRM-O
              FORMAT IS "WCONFTR"
              INDICATORS ARE INDICADORES-PANTALLA
           END-WRITE.

           READ PANTALLA-TRANSF RECORD INTO WS-CONFIRM-I
              FORMAT IS "WCONFTR"
              INDICATORS ARE INDICADORES-PANTALLA
              AT END CONTINUE
           END-READ.

           IF CONFIRMA OF WCONFTR-I NOT = 'S' AND
              CONFIRMA OF WCONFTR-I NOT = 's'
              MOVE "TRANSFERENCIA CANCELADA POR EL USUARIO."
                TO MSGO OF BNKMAPT-O
              EXIT PARAGRAPH
           END-IF.

           PERFORM 3000-PERSISTENCIA-DATOS.


       3000-PERSISTENCIA-DATOS.
           MOVE MONTOI OF BNKMAPT-I TO WS-SALDO-CALCULO
           SUBTRACT WS-SALDO-CALCULO FROM WS-SALDO-ORIGEN
               GIVING WS-SALDO-NUEVO.

           PERFORM 7100-UPDATE-SALDO-ORIGEN.

           IF SQLCODE = 0
               PERFORM 7400-UPDATE-SALDO-DESTINO
               IF SQLCODE = 0

                   INITIALIZE DCLMOVIMIENTOS
                   MOVE 'T'              TO HV-TIPO-OPER
                   MOVE WS-SALDO-CALCULO TO HV-MONTO
                   MOVE CG-M-USER        TO HV-USUARIO-MOV
                   MOVE WS-USER-DESTINO  TO HV-USUARIO-REL
                   PERFORM 7200-INSERTAR-HISTORIAL

                   IF SQLCODE = 0

                       INITIALIZE DCLMOVIMIENTOS
                       MOVE 'R'              TO HV-TIPO-OPER
                       MOVE WS-SALDO-CALCULO TO HV-MONTO
                       MOVE WS-USER-DESTINO  TO HV-USUARIO-MOV
                       MOVE CG-M-USER        TO HV-USUARIO-REL

                       PERFORM 7200-INSERTAR-HISTORIAL

                       IF SQLCODE = 0
                           EXEC SQL COMMIT END-EXEC
                           INITIALIZE WS-TRANST-I
                           INITIALIZE WS-CONFIRM-I
                           INITIALIZE WS-VARIABLES-TRABAJO
                           PERFORM 7000-LEER-SALDO-ACTUAL
                           MOVE "TRANSFERENCIA EXITOSA"
                            TO MSGO OF BNKMAPT-O
                       ELSE
                           EXEC SQL ROLLBACK END-EXEC
                           MOVE ' ERROR HISTORIAL DESTINO' TO MSGO
                           OF BNKMAPT-O
                       END-IF
                   ELSE
                       EXEC SQL ROLLBACK END-EXEC
                       MOVE ' ERROR HISTORIAL ORIGEN' TO MSGO
                       OF BNKMAPT-O
                   END-IF
               ELSE
                   EXEC SQL ROLLBACK END-EXEC
                   MOVE ' ERROR AL ACREDITAR DESTINO' TO MSGO
                   OF BNKMAPT-O
               END-IF
           ELSE
               EXEC SQL ROLLBACK END-EXEC
               MOVE ' ERROR AL DEBITAR ORIGEN' TO MSGO
               OF BNKMAPT-O
           END-IF.

       7000-LEER-SALDO-ACTUAL.
           INITIALIZE DCLCLIEN
           EXEC SQL
                SELECT SALDO INTO :HV-SALDO
                FROM CLIENTES
                WHERE USUARIO = :CG-M-USER
           END-EXEC.
           IF SQLCODE = 0
              MOVE HV-SALDO TO SALDO OF BNKMAPT-O
           END-IF.


       7000-LEER-SALDO-ORIGEN.
           INITIALIZE DCLCLIEN.
           MOVE CG-M-USER TO WS-USER-ORIGEN.
           EXEC SQL SELECT SALDO INTO :HV-SALDO
               FROM CLIENTES WHERE USUARIO = :WS-USER-ORIGEN
           END-EXEC.
           MOVE HV-SALDO TO WS-SALDO-ORIGEN.

       7100-UPDATE-SALDO-ORIGEN.
           MOVE WS-SALDO-NUEVO TO HV-SALDO.
           EXEC SQL UPDATE CLIENTES SET SALDO = :HV-SALDO
               WHERE USUARIO = :WS-USER-ORIGEN
           END-EXEC.

       7200-INSERTAR-HISTORIAL.
           EXEC SQL INSERT INTO MOVIMIENTOS
               (USUARIO, TIPO_OPER, MONTO, FECHA, USUARIO_REL)
               VALUES (:HV-USUARIO-MOV, :HV-TIPO-OPER, :HV-MONTO,
                CURRENT TIMESTAMP, :HV-USUARIO-REL)
           END-EXEC.

       7300-VALIDAR-DESTINO-DB2.
           INITIALIZE DCLCLIEN.
           MOVE USRDEST OF BNKMAPT-I TO WS-USER-DESTINO.
           EXEC SQL SELECT SALDO INTO :HV-SALDO
               FROM CLIENTES WHERE USUARIO = :WS-USER-DESTINO
           END-EXEC.
           MOVE HV-SALDO TO WS-SALDO-DESTINO.

       7400-UPDATE-SALDO-DESTINO.
           MOVE WS-SALDO-CALCULO TO HV-MONTO.
           EXEC SQL UPDATE CLIENTES
               SET SALDO = SALDO + :HV-MONTO
               WHERE USUARIO = :WS-USER-DESTINO
           END-EXEC.


