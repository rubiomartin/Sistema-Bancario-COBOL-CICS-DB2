       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBNKX.
       AUTHOR. MARTIN RUBIO.
      *================================================================*
      * TITULO   : TRANSACCIONES (DEPOSITOS Y RETIROS)                 *
      * ENTORNO  : IBM i (OS/400) - ILE COBOL                          *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS CRT.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PANTALLA-TRANSX ASSIGN TO WORKSTATION-BNKXMP-SI
                  ORGANIZATION IS TRANSACTION
                  ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD PANTALLA-TRANSX.
       01 TRANSX-RECORD PIC X(200).

       WORKING-STORAGE SECTION.
       01 WS-TRANSX-I.
          COPY DDS-BNKMAPX-I OF BNKXMP.

       01 WS-TRANSX-O.
          COPY DDS-BNKMAPX-O OF BNKXMP.

       01 WS-CONFIRM-I.
          COPY DDS-WCONFIRM-I OF BNKXMP.

       01 WS-CONFIRM-O.
          COPY DDS-WCONFIRM-O OF BNKXMP.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01 WS-VARIABLES-TRABAJO.
          05 SW-FIN-PROG         PIC X(1) VALUE 'N'.
             88 FIN-PROG         VALUE 'S'.
          05 WS-NUEVO-SALDO      PIC S9(8)V9(2) USAGE COMP-3.
          05 WS-SALDO-ACTUAL     PIC S9(8)V9(2) USAGE COMP-3.

       01 WS-DEBUG-MSG.
          05 FILLER         PIC X(2) VALUE "S:".
          05 WS-DBG-SALDO   PIC Z(7)9.99.
          05 FILLER         PIC X(4) VALUE " +M:".
          05 WS-DBG-MONTO   PIC Z(7)9.99.
          05 FILLER         PIC X(3) VALUE " = ".
          05 WS-DBG-NUEVO   PIC Z(7)9.99.

       01 WS-TXT-CONFIRMACION.
          05 FILLER         PIC X(18) VALUE "Seguro que quiere ".
          05 WS-ACCION      PIC X(9).
          05 FILLER         PIC X(2)  VALUE " $".
          05 WS-MONTO-TXT   PIC Z(7)9.99.
          05 FILLER         PIC X(1)  VALUE "?".

       01 INDICADORES-PANTALLA.
          05 IND03               PIC 1 INDIC 03.
             88 IND-SALIR        VALUE B"1".

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
      *   03 CG-HISTORIAL.
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

           OPEN I-O PANTALLA-TRANSX.
           MOVE 'N' TO SW-FIN-PROG.

           INITIALIZE WS-TRANSX-I.
           INITIALIZE WS-TRANSX-O.
           INITIALIZE WS-CONFIRM-I.
           INITIALIZE WS-CONFIRM-O.

           MOVE ZERO TO SALDO OF BNKMAPX-O.
           MOVE ZERO TO MONTOI OF BNKMAPX-I.

           MOVE CG-M-USER TO NOMBREUS OF BNKMAPX-O.
           PERFORM 7000-LEER-SALDO-ACTUAL.

           PERFORM UNTIL FIN-PROG

               WRITE TRANSX-RECORD FROM WS-TRANSX-O
                FORMAT  IS "BNKMAPX"
                INDICATORS ARE INDICADORES-PANTALLA
               END-WRITE

               READ PANTALLA-TRANSX RECORD INTO WS-TRANSX-I
                FORMAT IS "BNKMAPX"
                INDICATORS ARE INDICADORES-PANTALLA
                AT END CONTINUE
               END-READ

               MOVE SPACES TO MSGO OF BNKMAPX-O

               IF IND-SALIR
                  SET FIN-PROG TO TRUE
               ELSE
                  PERFORM 7000-LEER-SALDO-ACTUAL
                  PERFORM 1000-PROCESAR-OPERACION
               END-IF
           END-PERFORM.

           CLOSE PANTALLA-TRANSX.
           GOBACK.

       1000-PROCESAR-OPERACION.
           IF TIPOOPER OF BNKMAPX-I NOT = 'D' AND
              TIPOOPER OF BNKMAPX-I NOT = 'R'
              MOVE "TIPO DE OPERACION INVALIDA. USE D O R."
                TO MSGO OF BNKMAPX-O
              EXIT PARAGRAPH
           END-IF.

           IF MONTOI OF BNKMAPX-I <= 0
              MOVE "EL MONTO DEBE SER MAYOR A CERO."
                TO MSGO OF BNKMAPX-O
              EXIT PARAGRAPH
           END-IF.

           IF TIPOOPER OF BNKMAPX-I = 'R' AND
              MONTOI OF BNKMAPX-I >  WS-SALDO-ACTUAL
              MOVE "FONDOS INSUFICIENTES PARA EL RETIRO."
                TO MSGO OF BNKMAPX-O
              EXIT PARAGRAPH
           END-IF.

           IF TIPOOPER OF BNKMAPX-I = 'D'
              MOVE "DEPOSITAR" TO WS-ACCION
           ELSE
              MOVE "RETIRAR  " TO WS-ACCION
           END-IF.

           MOVE MONTOI OF BNKMAPX-I TO WS-MONTO-TXT.
           MOVE WS-TXT-CONFIRMACION TO MSGCONF OF WCONFIRM-O.

           WRITE TRANSX-RECORD FROM WS-CONFIRM-O
              FORMAT IS "WCONFIRM"
              INDICATORS ARE INDICADORES-PANTALLA
           END-WRITE.

           READ PANTALLA-TRANSX RECORD INTO WS-CONFIRM-I
              FORMAT IS "WCONFIRM"
              INDICATORS ARE INDICADORES-PANTALLA
              AT END CONTINUE
           END-READ.

           IF CONFIRMA OF WCONFIRM-I NOT = 'S' AND
              CONFIRMA OF WCONFIRM-I NOT = 's'
              MOVE "OPERACION CANCELADA."
                TO MSGO OF BNKMAPX-O
              EXIT PARAGRAPH
           END-IF.

           PERFORM 8000-EJECUTAR-TRANSACCION.

       7000-LEER-SALDO-ACTUAL.
           INITIALIZE DCLCLIEN
           MOVE ZEROES TO WS-SALDO-ACTUAL
           EXEC SQL
                SELECT SALDO INTO :HV-SALDO
                FROM CLIENTES
                WHERE USUARIO = :CG-M-USER
           END-EXEC.
           IF SQLCODE = 0
              MOVE HV-SALDO TO SALDO OF BNKMAPX-O
              MOVE HV-SALDO TO WS-SALDO-ACTUAL
           ELSE
              MOVE "ERROR AL RECUPERAR EL SALDO DEL USUARIO."
                TO MSGO OF BNKMAPX-O
           END-IF.

       8000-EJECUTAR-TRANSACCION.
           IF TIPOOPER OF BNKMAPX-I = 'D'
              COMPUTE WS-NUEVO-SALDO = WS-SALDO-ACTUAL +
                                       MONTOI OF BNKMAPX-I
           ELSE
              COMPUTE WS-NUEVO-SALDO = WS-SALDO-ACTUAL -
                                       MONTOI OF BNKMAPX-I
           END-IF.

           PERFORM 7100-UPDATE-SALDO

           IF SQLCODE = 0
              PERFORM 7200-INSERTAR-HISTORIAL
              IF SQLCODE = 0

                 EXEC SQL COMMIT END-EXEC

                 MOVE SPACE TO TIPOOPER OF BNKMAPX-I
                 MOVE ZERO  TO MONTOI OF BNKMAPX-I
                 PERFORM 7000-LEER-SALDO-ACTUAL
              ELSE
                 EXEC SQL ROLLBACK END-EXEC
                 MOVE "ERROR AL INSERTAR EN EL HISTORIAL"
                   TO MSGO OF BNKMAPX-O
              END-IF
           ELSE
              EXEC SQL ROLLBACK END-EXEC
              MOVE "ERROR AL ACTUALIZAR EL SALDO" TO MSGO OF BNKMAPX-O
           END-IF.



       7100-UPDATE-SALDO.
           INITIALIZE DCLCLIEN.
           MOVE WS-NUEVO-SALDO TO HV-SALDO.
           EXEC SQL UPDATE CLIENTES SET SALDO = :HV-SALDO
               WHERE USUARIO = :CG-M-USER
           END-EXEC.



       7200-INSERTAR-HISTORIAL.

           INITIALIZE DCLMOVIMIENTOS.

           IF TIPOOPER OF BNKMAPX-I = 'R'
               MOVE 'Z'          TO HV-TIPO-OPER
           ELSE
               MOVE TIPOOPER OF BNKMAPX-I   TO HV-TIPO-OPER
           END-IF
           MOVE MONTOI OF BNKMAPX-I TO HV-MONTO.
           MOVE CG-M-USER TO HV-USUARIO-MOV.
           MOVE SPACES TO HV-USUARIO-REL
           EXEC SQL INSERT INTO MOVIMIENTOS
               (USUARIO, TIPO_OPER, MONTO, FECHA, USUARIO_REL)
               VALUES (:HV-USUARIO-MOV, :HV-TIPO-OPER, :HV-MONTO,
                CURRENT TIMESTAMP, :HV-USUARIO-REL)
           END-EXEC.



