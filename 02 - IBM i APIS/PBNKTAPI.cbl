       PROCESS QUOTE.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBNKTAPI.
       AUTHOR. MARTIN RUBIO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.

       01  INDICADORES-PROCESO.
           05 ESTADO-TRX               PIC X(01) VALUE 'P'.
              88 TRX-EN-PROCESO        VALUE 'P'.
              88 TRX-OK                VALUE 'O'.
              88 TRX-IDEMPOTENTE       VALUE 'I'.
              88 TRX-DEADLOCK          VALUE 'D'.
              88 TRX-ERROR             VALUE 'E'.
           05 TIPO-RED-FLAG            PIC X(01).
              88 RED-INTERNA           VALUE 'I'.

       01  HV-VARIABLES.
           05 HV-USER-ORIGEN           PIC X(8).
           05 HV-USER-DESTINO          PIC X(8).
           05 HV-MONTO                 PIC S9(11)V9(2) USAGE COMP-3.
           05 HV-COMISION              PIC S9(11)V9(2) USAGE COMP-3.
           05 HV-IMPUESTO              PIC S9(11)V9(2) USAGE COMP-3.
           05 HV-TOTAL-DEBITO          PIC S9(11)V9(2) USAGE COMP-3.
           05 HV-MONTO-NEG             PIC S9(11)V9(2) USAGE COMP-3.
           05 HV-SALDO-ORIGEN          PIC S9(11)V9(2) USAGE COMP-3.
           05 HV-SQLSTATE              PIC X(5).
           05 HV-SYS-FEE               PIC X(8) VALUE "SYS_FEE ".
           05 HV-SYS-TAX               PIC X(8) VALUE "SYS_TAX ".
           05 HV-TIPO-TRAN             PIC X(4) VALUE "TRAN".

       LINKAGE SECTION.
       01  LK-INPUT.
           05 LK-ID-OPERACION          PIC X(36).
           05 LK-ID-IDEMP              PIC X(36).
           05 LK-ID-COELSA             PIC X(22).
           05 LK-CAE-AFIP              PIC X(14).
           05 LK-USER-ORIGEN           PIC X(8).
           05 LK-IDENT-DESTINO         PIC X(22).
           05 LK-TIPO-RED              PIC X(1).
           05 LK-MONTO                 PIC 9(11)V9(2).
           05 LK-COMISION              PIC 9(11)V9(2).
           05 LK-IMPUESTO              PIC 9(11)V9(2).

       01  LK-OUTPUT.
           05 LK-CODE                  PIC X(2).
           05 LK-MSG                   PIC X(40).
           05 LK-SALDO-OUT             PIC 9(11)V9(2).

       PROCEDURE DIVISION USING LK-INPUT LK-OUTPUT.
       COMIENZO.
           PERFORM 1000-INICIALIZAR    THRU 1000-FIN
           PERFORM 2000-REGISTRAR-OPER THRU 2000-FIN
           
           IF TRX-EN-PROCESO
              PERFORM 3000-VALIDAR-DEST THRU 3000-FIN
           END-IF
           
           IF TRX-EN-PROCESO
              PERFORM 4000-VALIDAR-FOND THRU 4000-FIN
           END-IF
           
           IF TRX-EN-PROCESO
              PERFORM 5000-ACTUALIZ-SALD THRU 5000-FIN
              PERFORM 6000-GRABAR-MOVIM  THRU 6000-FIN
           END-IF
           
           PERFORM 9000-FINALIZAR        THRU 9000-FIN
           GOBACK.

      *----------------------------------------------------------------*
       1000-INICIALIZAR.
           EXEC SQL
               SET OPTION COMMIT = *CHG, DATFMT = *ISO
           END-EXEC.

           INITIALIZE LK-OUTPUT
           SET TRX-EN-PROCESO  TO TRUE
           MOVE LK-TIPO-RED    TO TIPO-RED-FLAG

           MOVE LK-USER-ORIGEN TO HV-USER-ORIGEN
           MOVE LK-MONTO       TO HV-MONTO
           MOVE LK-COMISION    TO HV-COMISION
           MOVE LK-IMPUESTO    TO HV-IMPUESTO

           COMPUTE HV-TOTAL-DEBITO = HV-MONTO + HV-COMISION
                                     + HV-IMPUESTO.
       1000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2000-REGISTRAR-OPER.
           EXEC SQL
               INSERT INTO OPERACION
                 (ID_OPERACION, TIPO_OPERACION,
                  ID_IDEMPOTENCIA, ID_COELSA, CAE_AFIP)
               VALUES
                 (:LK-ID-OPERACION, :HV-TIPO-TRAN, :LK-ID-IDEMP,
                  :LK-ID-COELSA, :LK-CAE-AFIP)
           END-EXEC.

           EXEC SQL GET DIAGNOSTICS CONDITION 1
               :HV-SQLSTATE = RETURNED_SQLSTATE
           END-EXEC.

           IF HV-SQLSTATE = "23505"
              SET TRX-IDEMPOTENTE TO TRUE
              MOVE "OK" TO LK-CODE
              MOVE "IDEMPOTENCIA: TRX YA PROCESADA" TO LK-MSG
           ELSE
              IF SQLCODE NOT = 0
                 PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
                 MOVE "ERROR AL GRABAR CABECERA OPERACION" TO LK-MSG
              END-IF
           END-IF.
       2000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       3000-VALIDAR-DEST.
           IF RED-INTERNA
              MOVE LK-IDENT-DESTINO(1:8) TO HV-USER-DESTINO

              EXEC SQL
                  SELECT ID_USUARIO INTO :HV-USER-DESTINO
                  FROM USUARIOS
                  WHERE ID_USUARIO = :HV-USER-DESTINO
              END-EXEC

              IF SQLCODE NOT = 0
                 PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
                 MOVE "EL USUARIO DESTINO NO EXISTE" TO LK-MSG
              END-IF
           ELSE
              MOVE "COELSA  " TO HV-USER-DESTINO
           END-IF.
       3000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       4000-VALIDAR-FOND.
           EXEC SQL
               SELECT SALDO_ACTUAL INTO :HV-SALDO-ORIGEN
               FROM USUARIOS
               WHERE ID_USUARIO = :HV-USER-ORIGEN
           END-EXEC.

           IF SQLCODE NOT = 0
              PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
              MOVE "ERROR LEYENDO SALDO ORIGEN" TO LK-MSG
              GO TO 4000-FIN
           END-IF.

           IF HV-TOTAL-DEBITO > HV-SALDO-ORIGEN
              PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
              MOVE "FONDOS INSUFICIENTES" TO LK-MSG
           END-IF.
       4000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       5000-ACTUALIZ-SALD.
           EXEC SQL
               UPDATE USUARIOS
               SET SALDO_ACTUAL = SALDO_ACTUAL - :HV-TOTAL-DEBITO
               WHERE ID_USUARIO = :HV-USER-ORIGEN
           END-EXEC.

           IF SQLCODE NOT = 0
              PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
              MOVE "ERROR ACTUALIZANDO SALDO ORIGEN" TO LK-MSG
              GO TO 5000-FIN
           END-IF.

           EXEC SQL
               UPDATE USUARIOS
               SET SALDO_ACTUAL = SALDO_ACTUAL + :HV-MONTO
               WHERE ID_USUARIO = :HV-USER-DESTINO
           END-EXEC.

           IF SQLCODE NOT = 0
              PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
              MOVE "ERROR ACTUALIZANDO SALDO DESTINO" TO LK-MSG
              GO TO 5000-FIN
           END-IF.

           IF HV-COMISION > 0
              EXEC SQL
                  UPDATE USUARIOS
                  SET SALDO_ACTUAL = SALDO_ACTUAL + :HV-COMISION
                  WHERE ID_USUARIO = :HV-SYS-FEE
              END-EXEC
              IF SQLCODE NOT = 0
                 PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
                 MOVE "ERROR ACTUALIZANDO CUENTA COMISIONES" TO LK-MSG
                 GO TO 5000-FIN
              END-IF
           END-IF.

           IF HV-IMPUESTO > 0
              EXEC SQL
                  UPDATE USUARIOS
                  SET SALDO_ACTUAL = SALDO_ACTUAL + :HV-IMPUESTO
                  WHERE ID_USUARIO = :HV-SYS-TAX
              END-EXEC
              IF SQLCODE NOT = 0
                 PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
                 MOVE "ERROR ACTUALIZANDO CUENTA IMPUESTOS" TO LK-MSG
                 GO TO 5000-FIN
              END-IF
           END-IF.
       5000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       6000-GRABAR-MOVIM.
           COMPUTE HV-MONTO-NEG = HV-TOTAL-DEBITO * -1.

           EXEC SQL
               INSERT INTO MOVIMIENTO
                 (ID_MOVIMIENTO, ID_OPERACION, ID_USUARIO, MONTO)
               VALUES
                 (HEX(GENERATE_UNIQUE()), :LK-ID-OPERACION,
                  :HV-USER-ORIGEN, :HV-MONTO-NEG)
           END-EXEC.

           IF SQLCODE NOT = 0
              PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
              MOVE "ERROR GRABANDO MOVIMIENTO ORIGEN" TO LK-MSG
              GO TO 6000-FIN
           END-IF.

           EXEC SQL
               INSERT INTO MOVIMIENTO
                 (ID_MOVIMIENTO, ID_OPERACION, ID_USUARIO, MONTO)
               VALUES
                 (HEX(GENERATE_UNIQUE()), :LK-ID-OPERACION,
                  :HV-USER-DESTINO, :HV-MONTO)
           END-EXEC.

           IF SQLCODE NOT = 0
              PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
              MOVE "ERROR GRABANDO MOVIMIENTO DESTINO" TO LK-MSG
              GO TO 6000-FIN
           END-IF.

           IF HV-COMISION > 0
              EXEC SQL
                  INSERT INTO MOVIMIENTO
                    (ID_MOVIMIENTO, ID_OPERACION, ID_USUARIO, MONTO)
                  VALUES
                    (HEX(GENERATE_UNIQUE()), :LK-ID-OPERACION,
                     :HV-SYS-FEE, :HV-COMISION)
              END-EXEC
              IF SQLCODE NOT = 0
                 PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
                 MOVE "ERROR GRABANDO MOVIMIENTO COMISION" TO LK-MSG
                 GO TO 6000-FIN
              END-IF
           END-IF.

           IF HV-IMPUESTO > 0
              EXEC SQL
                  INSERT INTO MOVIMIENTO
                    (ID_MOVIMIENTO, ID_OPERACION, ID_USUARIO, MONTO)
                  VALUES
                    (HEX(GENERATE_UNIQUE()), :LK-ID-OPERACION,
                     :HV-SYS-TAX, :HV-IMPUESTO)
              END-EXEC
              IF SQLCODE NOT = 0
                 PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
                 MOVE "ERROR GRABANDO MOVIMIENTO IMPUESTO" TO LK-MSG
                 GO TO 6000-FIN
              END-IF
           END-IF.

           EXEC SQL COMMIT END-EXEC.
           SET TRX-OK TO TRUE.
       6000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       9000-FINALIZAR.
           EVALUATE TRUE
              WHEN TRX-OK
                   MOVE "OK" TO LK-CODE
                   MOVE "TRANSACCION PROCESADA EXITOSAMENTE" TO LK-MSG
                   COMPUTE LK-SALDO-OUT = HV-SALDO-ORIGEN 
                                          - HV-TOTAL-DEBITO
              WHEN TRX-IDEMPOTENTE
                   CONTINUE
              WHEN TRX-DEADLOCK
                   PERFORM 9950-ROLLBACK THRU 9950-FIN
                   MOVE "RE" TO LK-CODE
                   MOVE "DEADLOCK DB2 (-911): REINTENTE OPERACION" 
                     TO LK-MSG
              WHEN OTHER
                   PERFORM 9950-ROLLBACK THRU 9950-FIN
                   IF LK-CODE NOT = "ER"
                      MOVE "ER" TO LK-CODE
                      MOVE "ERROR GENERAL EN PROCESAMIENTO" TO LK-MSG
                   END-IF
           END-EVALUATE.
       9000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       9900-TRATAR-ERROR.
           IF SQLCODE = -911
              SET TRX-DEADLOCK TO TRUE
           ELSE
              SET TRX-ERROR TO TRUE
              MOVE "ER"     TO LK-CODE
           END-IF.
       9900-FIN.
           EXIT.

      *----------------------------------------------------------------*
       9950-ROLLBACK.
           EXEC SQL ROLLBACK END-EXEC.
       9950-FIN.
           EXIT.