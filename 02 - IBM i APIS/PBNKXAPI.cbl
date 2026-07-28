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

       01  INDICADORES-PROCESO.
           05 ESTADO-TRX               PIC X(01) VALUE 'P'.
              88 TRX-EN-PROCESO        VALUE 'P'.
              88 TRX-OK                VALUE 'O'.
              88 TRX-CONSULTA-OK       VALUE 'C'.
              88 TRX-DEADLOCK          VALUE 'D'.
              88 TRX-ERROR             VALUE 'E'.
           05 TIPO-OPERACION-FLAG      PIC X(01).
              88 OP-CONSULTA           VALUE 'C'.
              88 OP-DEPOSITO           VALUE 'D'.
              88 OP-RETIRO             VALUE 'R'.

       01  HV-VARIABLES.
           05 HV-USUARIO               PIC X(8).
           05 HV-TIPO-CAJA             PIC X(1).
           05 HV-MONTO                 PIC S9(10)V9(2) USAGE COMP-3.
           05 HV-MONTO-MOV             PIC S9(10)V9(2) USAGE COMP-3.
           05 HV-SALDO-ACTUAL          PIC S9(10)V9(2) USAGE COMP-3.
           05 HV-NUEVO-SALDO           PIC S9(10)V9(2) USAGE COMP-3.
           05 HV-ID-OPERACION          PIC X(36).
           05 HV-SQLSTATE              PIC X(5).

       LINKAGE SECTION.
       01  LK-INPUT.
           05 LK-USUARIO               PIC X(8).
           05 LK-OPERACION             PIC X(1).
           05 LK-MONTO                 PIC 9(10).

       01  LK-OUTPUT.
           05 LK-CODE                  PIC X(2).
           05 LK-MSG                   PIC X(40).
           05 LK-SALDO-OUT             PIC 9(10).

       PROCEDURE DIVISION USING LK-INPUT LK-OUTPUT.
       COMIENZO.
           PERFORM 1000-INICIALIZAR       THRU 1000-FIN
           PERFORM 2000-LEER-SALDO        THRU 2000-FIN
           
           IF TRX-EN-PROCESO
              PERFORM 3000-PROCESAR-OPER  THRU 3000-FIN
           END-IF
           
           IF TRX-EN-PROCESO AND NOT OP-CONSULTA
              PERFORM 4000-ACTUALIZ-SALD  THRU 4000-FIN
           END-IF

           IF TRX-EN-PROCESO AND NOT OP-CONSULTA
              PERFORM 5000-REGISTRAR-OPER THRU 5000-FIN
           END-IF

           IF TRX-EN-PROCESO AND NOT OP-CONSULTA
              PERFORM 6000-REGISTRAR-MOV  THRU 6000-FIN
           END-IF
           
           PERFORM 9000-FINALIZAR         THRU 9000-FIN
           GOBACK.

      *----------------------------------------------------------------*
       1000-INICIALIZAR.
           EXEC SQL
               SET OPTION COMMIT = *CHG, DATFMT = *ISO
           END-EXEC.

           INITIALIZE LK-OUTPUT
           SET TRX-EN-PROCESO  TO TRUE
           
           MOVE LK-USUARIO     TO HV-USUARIO
           MOVE LK-OPERACION   TO HV-TIPO-CAJA
           MOVE LK-OPERACION   TO TIPO-OPERACION-FLAG
           
           COMPUTE HV-MONTO = LK-MONTO / 100.
       1000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2000-LEER-SALDO.
           IF OP-CONSULTA
              EXEC SQL
                  SELECT SALDO_ACTUAL INTO :HV-SALDO-ACTUAL
                  FROM USUARIOS WHERE ID_USUARIO = :HV-USUARIO
              END-EXEC
           ELSE
              EXEC SQL
                  SELECT SALDO_ACTUAL INTO :HV-SALDO-ACTUAL
                  FROM USUARIOS WHERE ID_USUARIO = :HV-USUARIO
                  WITH RS USE AND KEEP EXCLUSIVE LOCKS
              END-EXEC
           END-IF.

           IF SQLCODE NOT = 0
              PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
              MOVE "USUARIO INEXISTENTE O BLOQUEADO" TO LK-MSG
           END-IF.
       2000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       3000-PROCESAR-OPER.
           EVALUATE TRUE
              WHEN OP-CONSULTA
                   SET TRX-CONSULTA-OK TO TRUE
                   
              WHEN OP-DEPOSITO
                   COMPUTE HV-NUEVO-SALDO = HV-SALDO-ACTUAL + HV-MONTO
                   MOVE HV-MONTO TO HV-MONTO-MOV
                   
              WHEN OP-RETIRO
                   IF HV-MONTO > HV-SALDO-ACTUAL
                      PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
                      MOVE "FONDOS INSUFICIENTES PARA RETIRO" TO LK-MSG
                   ELSE
                      COMPUTE HV-NUEVO-SALDO = HV-SALDO-ACTUAL 
                                             - HV-MONTO
                      COMPUTE HV-MONTO-MOV   = HV-MONTO * -1
                   END-IF
                   
              WHEN OTHER
                   PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
                   MOVE "TIPO DE OPERACION INVALIDO" TO LK-MSG
           END-EVALUATE.
       3000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       4000-ACTUALIZ-SALD.
           EXEC SQL
               UPDATE USUARIOS
               SET SALDO_ACTUAL = :HV-NUEVO-SALDO
               WHERE ID_USUARIO = :HV-USUARIO
           END-EXEC.

           IF SQLCODE NOT = 0
              PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
              MOVE "ERROR AL ACTUALIZAR SALDO EN DB2" TO LK-MSG
              GO TO 4000-FIN
           END-IF.
       4000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       5000-REGISTRAR-OPER.
           EXEC SQL
               VALUES HEX(GENERATE_UNIQUE()) INTO :HV-ID-OPERACION
           END-EXEC.

           EXEC SQL
               INSERT INTO OPERACION
                 (ID_OPERACION, TIPO_OPERACION, ID_IDEMPOTENCIA)
               VALUES
                 (:HV-ID-OPERACION, :HV-TIPO-CAJA, :HV-ID-OPERACION)
           END-EXEC.

           IF SQLCODE NOT = 0
              PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
              MOVE "ERROR REGISTRANDO OPERACION" TO LK-MSG
              GO TO 5000-FIN
           END-IF.
       5000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       6000-REGISTRAR-MOV.
           EXEC SQL
               INSERT INTO MOVIMIENTO
                 (ID_MOVIMIENTO, ID_OPERACION, ID_USUARIO, MONTO)
               VALUES
                 (HEX(GENERATE_UNIQUE()), :HV-ID-OPERACION,
                  :HV-USUARIO, :HV-MONTO-MOV)
           END-EXEC.

           IF SQLCODE = 0
              EXEC SQL COMMIT END-EXEC
              SET TRX-OK TO TRUE
           ELSE
              PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
              MOVE "ERROR REGISTRANDO ASIENTO CONTABLE" TO LK-MSG
           END-IF.
       6000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       9000-FINALIZAR.
           EVALUATE TRUE
              WHEN TRX-OK
                   MOVE "OK" TO LK-CODE
                   MOVE "OPERACION REALIZADA CON EXITO" TO LK-MSG
                   COMPUTE LK-SALDO-OUT = HV-NUEVO-SALDO * 100
                   
              WHEN TRX-CONSULTA-OK
                   MOVE "OK" TO LK-CODE
                   MOVE "CONSULTA DE SALDO EXITOSA" TO LK-MSG
                   COMPUTE LK-SALDO-OUT = HV-SALDO-ACTUAL * 100
                   
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