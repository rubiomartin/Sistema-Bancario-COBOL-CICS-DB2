       PROCESS QUOTE.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBNKHAPI.
       AUTHOR. MARTIN RUBIO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.

       01  HV-VARIABLES.
           05 HV-USER             PIC X(8).
           05 HV-TIPO-OPER-FULL   PIC X(4).
           05 HV-TIPO-OPER-OUT    PIC X(1).
           05 HV-MONTO            PIC S9(11)V9(2) USAGE COMP-3.
           05 HV-FECHA-CHAR       PIC X(10).
           05 HV-USUARIO-REL      PIC X(8).
           05 HV-BLANCOS          PIC X(8) VALUE SPACES.
           05 HV-DATE-FROM        PIC X(10).
           05 HV-DATE-TO          PIC X(10).
           05 HV-OFFSET           PIC S9(9) COMP.
           05 HV-SYS-FEE          PIC X(8) VALUE "SYS_FEE ".
           05 HV-SYS-TAX          PIC X(8) VALUE "SYS_TAX ".

       01  WS-COUNTERS.
           05 WS-IDX              PIC 9(2).

       LINKAGE SECTION.
       01  LK-INPUT.
           05 LK-USER          PIC X(8).
           05 LK-DATE-FROM     PIC X(10).
           05 LK-DATE-TO       PIC X(10).
           05 LK-OFFSET        PIC 9(4).

       01  LK-OUTPUT.
           05 LK-CODE          PIC X(2).
           05 LK-MSG           PIC X(40).
           05 LK-COUNT         PIC 9(2).
           05 LK-RECORDS.
              10 LK-ROW OCCURS 10 TIMES.
                 15 LK-FECHA   PIC X(10).
                 15 LK-TIPO    PIC X(1).
                 15 LK-MONTO   PIC 9(8)V9(2).
                 15 LK-REL     PIC X(8).

       PROCEDURE DIVISION USING LK-INPUT LK-OUTPUT.
       MAIN-PROCESS.
           EXEC SQL SET OPTION DATFMT = *ISO END-EXEC.

           EXEC SQL
             DECLARE CURHISTORIAL CURSOR FOR
             SELECT
               CHAR(DATE(O.FECHA_CREACION), ISO),
               O.TIPO_OPERACION,
               M.MONTO,
               COALESCE((SELECT MAX(M2.ID_USUARIO)
                         FROM MOVIMIENTO M2
                         WHERE M2.ID_OPERACION = M.ID_OPERACION
                           AND M2.ID_USUARIO NOT IN
                           (:HV-USER, :HV-SYS-FEE, :HV-SYS-TAX)
                        ), :HV-BLANCOS)
             FROM MOVIMIENTO M
             JOIN OPERACION O
               ON M.ID_OPERACION = O.ID_OPERACION
             WHERE M.ID_USUARIO = :HV-USER
               AND DATE(O.FECHA_CREACION) BETWEEN
                   :HV-DATE-FROM AND :HV-DATE-TO
             ORDER BY O.FECHA_CREACION DESC
           END-EXEC.

           INITIALIZE LK-OUTPUT.
           MOVE LK-USER TO HV-USER.
           MOVE LK-DATE-FROM TO HV-DATE-FROM.
           MOVE LK-DATE-TO TO HV-DATE-TO.
           MOVE LK-OFFSET TO HV-OFFSET.
           MOVE 1 TO WS-IDX.
           MOVE 0 TO LK-COUNT.

           EXEC SQL OPEN CURHISTORIAL END-EXEC.

           PERFORM UNTIL HV-OFFSET <= 0
               EXEC SQL
                   FETCH CURHISTORIAL
                   INTO :HV-FECHA-CHAR, :HV-TIPO-OPER-FULL,
                        :HV-MONTO, :HV-USUARIO-REL
               END-EXEC

               IF SQLCODE = 0
                   SUBTRACT 1 FROM HV-OFFSET
               ELSE
                   EXEC SQL CLOSE CURHISTORIAL END-EXEC
                   MOVE "OK" TO LK-CODE
                   MOVE "HISTORIAL RECUPERADO" TO LK-MSG
                   GOBACK
               END-IF
           END-PERFORM.

           PERFORM UNTIL WS-IDX > 10
               EXEC SQL
                   FETCH CURHISTORIAL
                   INTO :HV-FECHA-CHAR, :HV-TIPO-OPER-FULL,
                        :HV-MONTO, :HV-USUARIO-REL
               END-EXEC

               IF SQLCODE = 0
                   IF HV-TIPO-OPER-FULL = "CAJA"
                       IF HV-MONTO > 0
                           MOVE "D" TO HV-TIPO-OPER-OUT
                       ELSE
                           MOVE "R" TO HV-TIPO-OPER-OUT
                       END-IF
                   ELSE
                       IF HV-TIPO-OPER-FULL = "TRAN"
                           MOVE "T" TO HV-TIPO-OPER-OUT
                       ELSE
                           MOVE "O" TO HV-TIPO-OPER-OUT
                       END-IF
                   END-IF

                   IF HV-MONTO < 0
                       COMPUTE HV-MONTO = HV-MONTO * -1
                   END-IF

                   MOVE HV-FECHA-CHAR    TO LK-FECHA(WS-IDX)
                   MOVE HV-TIPO-OPER-OUT TO LK-TIPO(WS-IDX)
                   MOVE HV-MONTO         TO LK-MONTO(WS-IDX)
                   MOVE HV-USUARIO-REL   TO LK-REL(WS-IDX)

                   ADD 1 TO LK-COUNT
                   ADD 1 TO WS-IDX
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

           EXEC SQL CLOSE CURHISTORIAL END-EXEC.

           MOVE "OK" TO LK-CODE.
           MOVE "HISTORIAL CARGADO EXITOSAMENTE" TO LK-MSG.

           GOBACK.