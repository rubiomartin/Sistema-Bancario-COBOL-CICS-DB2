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

       01  HV-VARIABLES.
           05 HV-USER-ORIGEN   PIC X(8).
           05 HV-USER-DESTINO  PIC X(8).
           05 HV-MONTO         PIC S9(11)V9(2) USAGE COMP-3.
           05 HV-COMISION      PIC S9(11)V9(2) USAGE COMP-3.
           05 HV-IMPUESTO      PIC S9(11)V9(2) USAGE COMP-3.
           05 HV-TOTAL-DEBITO  PIC S9(11)V9(2) USAGE COMP-3.
           05 HV-SALDO-ORIGEN  PIC S9(11)V9(2) USAGE COMP-3.
           05 HV-SQLSTATE      PIC X(5).
           05 HV-SYS-FEE       PIC X(8) VALUE "SYS_FEE ".
           05 HV-SYS-TAX       PIC X(8) VALUE "SYS_TAX ".
           05 HV-TIPO-TRAN     PIC X(4) VALUE "TRAN".

       LINKAGE SECTION.
       01  LK-INPUT.
           05 LK-ID-OPERACION  PIC X(36).
           05 LK-ID-IDEMP      PIC X(36).
           05 LK-ID-COELSA     PIC X(22).
           05 LK-CAE-AFIP      PIC X(14).
           05 LK-USER-ORIGEN   PIC X(8).
           05 LK-IDENT-DESTINO PIC X(22).
           05 LK-TIPO-RED      PIC X(1).
           05 LK-MONTO         PIC 9(11)V9(2).
           05 LK-COMISION      PIC 9(11)V9(2).
           05 LK-IMPUESTO      PIC 9(11)V9(2).

       01  LK-OUTPUT.
           05 LK-CODE          PIC X(2).
           05 LK-MSG           PIC X(40).
           05 LK-SALDO-OUT     PIC 9(11)V9(2).

       PROCEDURE DIVISION USING LK-INPUT LK-OUTPUT.
       MAIN-PROCESS.
           EXEC SQL
               SET OPTION COMMIT = *CHG, DATFMT = *ISO
           END-EXEC.

           INITIALIZE LK-OUTPUT.
           MOVE LK-USER-ORIGEN TO HV-USER-ORIGEN.
           MOVE LK-MONTO       TO HV-MONTO.
           MOVE LK-COMISION    TO HV-COMISION.
           MOVE LK-IMPUESTO    TO HV-IMPUESTO.

           COMPUTE HV-TOTAL-DEBITO = HV-MONTO + HV-COMISION
                                   + HV-IMPUESTO.

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
               MOVE "OK" TO LK-CODE
               MOVE "IDEMPOTENCIA: TRX YA PROCESADA" TO LK-MSG
               GOBACK
           ELSE
               IF SQLCODE NOT = 0
                   MOVE "ER" TO LK-CODE
                   MOVE "ERROR AL GRABAR OPERACION" TO LK-MSG
                   GOBACK
               END-IF
           END-IF.

           IF LK-TIPO-RED = "I"
               MOVE LK-IDENT-DESTINO(1:8) TO HV-USER-DESTINO

               EXEC SQL
                   SELECT ID_USUARIO INTO :HV-USER-DESTINO
                   FROM USUARIOS
                   WHERE ID_USUARIO = :HV-USER-DESTINO
               END-EXEC

               IF SQLCODE NOT = 0
                   EXEC SQL ROLLBACK END-EXEC
                   MOVE "ER" TO LK-CODE
                   MOVE "EL USUARIO DESTINO NO EXISTE" TO LK-MSG
                   GOBACK
               END-IF
           ELSE
               MOVE "COELSA  " TO HV-USER-DESTINO
           END-IF.

           EXEC SQL
               SELECT SALDO_ACTUAL INTO :HV-SALDO-ORIGEN
               FROM USUARIOS
               WHERE ID_USUARIO = :HV-USER-ORIGEN
           END-EXEC.

           IF SQLCODE NOT = 0
               EXEC SQL ROLLBACK END-EXEC
               MOVE "ER" TO LK-CODE
               MOVE "ERROR LEYENDO SALDO ORIGEN" TO LK-MSG
               GOBACK
           END-IF.

           IF HV-TOTAL-DEBITO > HV-SALDO-ORIGEN
               EXEC SQL ROLLBACK END-EXEC
               MOVE "ER" TO LK-CODE
               MOVE "FONDOS INSUFICIENTES" TO LK-MSG
               GOBACK
           END-IF.

           EXEC SQL
               UPDATE USUARIOS
               SET SALDO_ACTUAL = SALDO_ACTUAL - :HV-TOTAL-DEBITO
               WHERE ID_USUARIO = :HV-USER-ORIGEN
           END-EXEC.

           EXEC SQL
               UPDATE USUARIOS
               SET SALDO_ACTUAL = SALDO_ACTUAL + :HV-MONTO
               WHERE ID_USUARIO = :HV-USER-DESTINO
           END-EXEC.

           IF HV-COMISION > 0
               EXEC SQL
                   UPDATE USUARIOS
                   SET SALDO_ACTUAL = SALDO_ACTUAL + :HV-COMISION
                   WHERE ID_USUARIO = :HV-SYS-FEE
               END-EXEC
           END-IF.

           IF HV-IMPUESTO > 0
               EXEC SQL
                   UPDATE USUARIOS
                   SET SALDO_ACTUAL = SALDO_ACTUAL + :HV-IMPUESTO
                   WHERE ID_USUARIO = :HV-SYS-TAX
               END-EXEC
           END-IF.

           COMPUTE HV-TOTAL-DEBITO = HV-TOTAL-DEBITO * -1.

           EXEC SQL
               INSERT INTO MOVIMIENTO
                 (ID_MOVIMIENTO, ID_OPERACION, ID_USUARIO, MONTO)
               VALUES
                 (HEX(GENERATE_UNIQUE()), :LK-ID-OPERACION,
                  :HV-USER-ORIGEN, :HV-TOTAL-DEBITO)
           END-EXEC.

           EXEC SQL
               INSERT INTO MOVIMIENTO
                 (ID_MOVIMIENTO, ID_OPERACION, ID_USUARIO, MONTO)
               VALUES
                 (HEX(GENERATE_UNIQUE()), :LK-ID-OPERACION,
                  :HV-USER-DESTINO, :HV-MONTO)
           END-EXEC.

           IF HV-COMISION > 0
               EXEC SQL
                   INSERT INTO MOVIMIENTO
                     (ID_MOVIMIENTO, ID_OPERACION, ID_USUARIO, MONTO)
                   VALUES
                     (HEX(GENERATE_UNIQUE()), :LK-ID-OPERACION,
                      :HV-SYS-FEE, :HV-COMISION)
               END-EXEC
           END-IF.

           IF HV-IMPUESTO > 0
               EXEC SQL
                   INSERT INTO MOVIMIENTO
                     (ID_MOVIMIENTO, ID_OPERACION, ID_USUARIO, MONTO)
                   VALUES
                     (HEX(GENERATE_UNIQUE()), :LK-ID-OPERACION,
                      :HV-SYS-TAX, :HV-IMPUESTO)
               END-EXEC
           END-IF.

           EXEC SQL COMMIT END-EXEC.

           MOVE "OK" TO LK-CODE
           MOVE "TRANSACCION PROCESADA EXITOSAMENTE" TO LK-MSG

           COMPUTE LK-SALDO-OUT = HV-SALDO-ORIGEN
                                + HV-TOTAL-DEBITO.

           GOBACK.