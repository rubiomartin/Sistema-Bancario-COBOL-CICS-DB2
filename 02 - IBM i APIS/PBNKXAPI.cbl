       PROCESS QUOTE.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBNKXAPI.
       AUTHOR. MARTIN RUBIO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.

       01  HV-VARIABLES.
           05 HV-USUARIO        PIC X(8).
           05 HV-TIPO-CAJA      PIC X(1).
           05 HV-MONTO          PIC S9(10)V9(2) USAGE COMP-3.
           05 HV-SALDO-ACTUAL   PIC S9(10)V9(2) USAGE COMP-3.
           05 HV-NUEVO-SALDO    PIC S9(10)V9(2) USAGE COMP-3.
           05 HV-ID-OPERACION   PIC X(36).
           05 HV-SQLSTATE       PIC X(5).

       LINKAGE SECTION.
       01  LK-INPUT.
           05 LK-USUARIO        PIC X(8).
           05 LK-OPERACION      PIC X(1).
           05 LK-MONTO          PIC 9(10).

       01  LK-OUTPUT.
           05 LK-CODE           PIC X(2).
           05 LK-MSG            PIC X(40).
           05 LK-SALDO-OUT      PIC 9(10).

       PROCEDURE DIVISION USING LK-INPUT LK-OUTPUT.
       MAIN-PROCESS.
           EXEC SQL
               SET OPTION COMMIT = *CHG, DATFMT = *ISO
           END-EXEC.

           INITIALIZE LK-OUTPUT.
           MOVE LK-USUARIO   TO HV-USUARIO.
           MOVE LK-OPERACION TO HV-TIPO-CAJA.
           COMPUTE HV-MONTO = LK-MONTO / 100.

           EXEC SQL
               SELECT SALDO_ACTUAL INTO :HV-SALDO-ACTUAL
               FROM USUARIOS
               WHERE ID_USUARIO = :HV-USUARIO
           END-EXEC.

           IF SQLCODE NOT = 0
               EXEC SQL ROLLBACK END-EXEC
               MOVE "ER" TO LK-CODE
               MOVE "USUARIO INEXISTENTE EN DB2" TO LK-MSG
               GOBACK
           END-IF.

           IF HV-TIPO-CAJA = "C"
               MOVE "OK" TO LK-CODE
               MOVE "CONSULTA DE SALDO EXITOSA" TO LK-MSG
               COMPUTE LK-SALDO-OUT = HV-SALDO-ACTUAL * 100
               GOBACK
           END-IF.

           IF HV-TIPO-CAJA = "D"
               COMPUTE HV-NUEVO-SALDO = HV-SALDO-ACTUAL + HV-MONTO
           ELSE
               IF HV-TIPO-CAJA = "R"
                   IF HV-MONTO > HV-SALDO-ACTUAL
                       EXEC SQL ROLLBACK END-EXEC
                       MOVE "ER" TO LK-CODE
                       MOVE "FONDOS INSUFICIENTES PARA RETIRO" TO LK-MSG
                       GOBACK
                   END-IF
                   COMPUTE HV-NUEVO-SALDO = HV-SALDO-ACTUAL - HV-MONTO
                   COMPUTE HV-MONTO = HV-MONTO * -1
               ELSE
                   EXEC SQL ROLLBACK END-EXEC
                   MOVE "ER" TO LK-CODE
                   MOVE "TIPO DE OPERACION INVALIDO" TO LK-MSG
                   GOBACK
               END-IF
           END-IF.

           EXEC SQL
               UPDATE USUARIOS
               SET SALDO_ACTUAL = :HV-NUEVO-SALDO
               WHERE ID_USUARIO = :HV-USUARIO
           END-EXEC.

           IF SQLCODE NOT = 0
               EXEC SQL ROLLBACK END-EXEC
               MOVE "ER" TO LK-CODE
               MOVE "ERROR AL ACTUALIZAR SALDO EN DB2" TO LK-MSG
               GOBACK
           END-IF.

           EXEC SQL
               VALUES CHAR(CURRENT TIMESTAMP) INTO :HV-ID-OPERACION
           END-EXEC.

           EXEC SQL
               INSERT INTO OPERACION
                 (ID_OPERACION, TIPO_OPERACION, ID_IDEMPOTENCIA)
               VALUES
                 (:HV-ID-OPERACION, :HV-TIPO-CAJA, :HV-ID-OPERACION)
           END-EXEC.

           IF SQLCODE NOT = 0
               EXEC SQL ROLLBACK END-EXEC
               MOVE "ER" TO LK-CODE
               MOVE "ERROR REGISTRANDO OPERACION" TO LK-MSG
               GOBACK
           END-IF.

           EXEC SQL
               INSERT INTO MOVIMIENTO
                 (ID_MOVIMIENTO, ID_OPERACION, ID_USUARIO, MONTO)
               VALUES
                 (HEX(GENERATE_UNIQUE()), :HV-ID-OPERACION,
                  :HV-USUARIO, :HV-MONTO)
           END-EXEC.

           IF SQLCODE NOT = 0
               EXEC SQL ROLLBACK END-EXEC
               MOVE "ER" TO LK-CODE
               MOVE "ERROR REGISTRANDO ASIENTO" TO LK-MSG
               GOBACK
           END-IF.

           EXEC SQL COMMIT END-EXEC.

           MOVE "OK" TO LK-CODE
           MOVE "OPERACION REALIZADA CON EXITO" TO LK-MSG
           COMPUTE LK-SALDO-OUT = HV-NUEVO-SALDO * 100.

           GOBACK.