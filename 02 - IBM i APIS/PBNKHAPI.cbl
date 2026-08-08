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

       01  INDICADORES-PROCESO.
           05 ESTADO-CURSOR               PIC X(01) VALUE 'C'.
              88 CURSOR-ABIERTO           VALUE 'A'.
              88 CURSOR-CERRADO           VALUE 'C'.
           05 ESTADO-SQL                  PIC X(01) VALUE 'P'.
              88 FETCH-OK                 VALUE 'O'.
              88 FETCH-EOF                VALUE 'F'.
              88 FETCH-ERROR              VALUE 'E'.

       01  HV-VARIABLES.
           05 HV-USER                     PIC X(8).
           05 HV-TIPO-OPER-FULL           PIC X(4).
              88 ES-CAJA                  VALUE
                                           'CAJA' 'D   ' 'R   ' 'D' 'R'.
              88 ES-TRAN                  VALUE 'TRAN'.
           05 HV-TIPO-OPER-OUT            PIC X(1).
           05 HV-MONTO                    PIC S9(11)V9(2) USAGE COMP-3.
           05 HV-FECHA-CHAR               PIC X(10).
           05 HV-USUARIO-REL              PIC X(8).
           05 HV-BLANCOS                  PIC X(8) VALUE SPACES.
           05 HV-DATE-FROM                PIC X(10).
           05 HV-DATE-TO                  PIC X(10).
           05 HV-OFFSET                   PIC S9(9) COMP.
           05 HV-SYS-FEE                  PIC X(8) VALUE "SYS_FEE ".
           05 HV-SYS-TAX                  PIC X(8) VALUE "SYS_TAX ".

       01  WS-COUNTERS.
           05 WS-IDX                      PIC 9(2).

       LINKAGE SECTION.
       01  LK-INPUT.
           05 LK-USER                     PIC X(8).
           05 LK-DATE-FROM                PIC X(10).
           05 LK-DATE-TO                  PIC X(10).
           05 LK-OFFSET                   PIC 9(4).

       01  LK-OUTPUT.
           05 LK-CODE                     PIC X(2).
           05 LK-MSG                      PIC X(40).
           05 LK-COUNT                    PIC 9(2).
           05 LK-RECORDS.
              10 LK-ROW OCCURS 10 TIMES.
                 15 LK-FECHA              PIC X(10).
                 15 LK-TIPO               PIC X(1).
                 15 LK-MONTO              PIC 9(8)V9(2).
                 15 LK-REL                PIC X(8).

       PROCEDURE DIVISION USING LK-INPUT LK-OUTPUT.
       COMIENZO.
           PERFORM 1000-INICIALIZAR       THRU 1000-FIN
           PERFORM 2000-ABRIR-CURSOR      THRU 2000-FIN

           IF CURSOR-ABIERTO
              PERFORM 3000-SALTAR-OFFSET  THRU 3000-FIN
           END-IF

           IF CURSOR-ABIERTO AND FETCH-OK
              PERFORM 4000-CARGAR-PAGINA  THRU 4000-FIN
           END-IF

           IF CURSOR-ABIERTO
              PERFORM 5000-CERRAR-CURSOR  THRU 5000-FIN
           END-IF

           PERFORM 9000-FINALIZAR         THRU 9000-FIN
           GOBACK.

      *----------------------------------------------------------------*
       1000-INICIALIZAR.
           EXEC SQL SET OPTION DATFMT = *ISO END-EXEC.

           INITIALIZE LK-OUTPUT
           MOVE LK-USER      TO HV-USER
           MOVE LK-DATE-FROM TO HV-DATE-FROM
           MOVE LK-DATE-TO   TO HV-DATE-TO
           MOVE LK-OFFSET    TO HV-OFFSET

           MOVE 1            TO WS-IDX
           MOVE 0            TO LK-COUNT
           SET FETCH-OK      TO TRUE

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
       1000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2000-ABRIR-CURSOR.
           EXEC SQL OPEN CURHISTORIAL END-EXEC.

           IF SQLCODE = 0
              SET CURSOR-ABIERTO TO TRUE
           ELSE
              PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
              MOVE "ERROR AL ABRIR CURSOR HISTORIAL" TO LK-MSG
           END-IF.
       2000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       3000-SALTAR-OFFSET.
           PERFORM UNTIL HV-OFFSET <= 0 OR FETCH-EOF OR FETCH-ERROR
               PERFORM 8000-LEER-CURSOR THRU 8000-FIN
               IF FETCH-OK
                  SUBTRACT 1 FROM HV-OFFSET
               END-IF
           END-PERFORM.

           IF FETCH-EOF
              SET FETCH-OK TO TRUE
              MOVE 0       TO HV-OFFSET
           END-IF.
       3000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       4000-CARGAR-PAGINA.
           PERFORM UNTIL WS-IDX > 10 OR FETCH-EOF OR FETCH-ERROR
               PERFORM 8000-LEER-CURSOR THRU 8000-FIN

               IF FETCH-OK
                  PERFORM 4100-MAPEAR-REGISTRO THRU 4100-FIN
                  ADD 1 TO LK-COUNT
                  ADD 1 TO WS-IDX
               END-IF
           END-PERFORM.

           IF FETCH-EOF
              SET FETCH-OK TO TRUE
           END-IF.
       4000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       4100-MAPEAR-REGISTRO.
           EVALUATE TRUE
              WHEN ES-CAJA
                   IF HV-MONTO > 0
                      MOVE "D" TO HV-TIPO-OPER-OUT
                   ELSE
                      MOVE "R" TO HV-TIPO-OPER-OUT
                   END-IF
              WHEN ES-TRAN
                   IF HV-MONTO > 0
                      MOVE "C" TO HV-TIPO-OPER-OUT
                   ELSE
                      MOVE "T" TO HV-TIPO-OPER-OUT
                   END-IF
              WHEN OTHER
                   MOVE "O" TO HV-TIPO-OPER-OUT
           END-EVALUATE.

           IF HV-MONTO < 0
              COMPUTE HV-MONTO = HV-MONTO * -1
           END-IF.

           INITIALIZE LK-ROW(WS-IDX)
           MOVE HV-FECHA-CHAR    TO LK-FECHA(WS-IDX)
           MOVE HV-TIPO-OPER-OUT TO LK-TIPO(WS-IDX)
           MOVE HV-MONTO         TO LK-MONTO(WS-IDX)
           MOVE HV-USUARIO-REL   TO LK-REL(WS-IDX).
       4100-FIN.
           EXIT.

      *----------------------------------------------------------------*
       5000-CERRAR-CURSOR.
           EXEC SQL CLOSE CURHISTORIAL END-EXEC.
           SET CURSOR-CERRADO TO TRUE.
       5000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       8000-LEER-CURSOR.
           EXEC SQL
               FETCH CURHISTORIAL
               INTO :HV-FECHA-CHAR, :HV-TIPO-OPER-FULL,
                    :HV-MONTO, :HV-USUARIO-REL
           END-EXEC.

           EVALUATE SQLCODE
              WHEN 0
                   SET FETCH-OK TO TRUE
              WHEN 100
                   SET FETCH-EOF TO TRUE
              WHEN OTHER
                   PERFORM 9900-TRATAR-ERROR THRU 9900-FIN
                   MOVE "ERROR DE LECTURA EN CURSOR" TO LK-MSG
           END-EVALUATE.
       8000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       9000-FINALIZAR.
           IF FETCH-OK AND LK-CODE = SPACES
              MOVE "OK" TO LK-CODE
              IF LK-COUNT = 0
                 MOVE "NO SE ENCONTRARON MOVIMIENTOS" TO LK-MSG
              ELSE
                 MOVE "HISTORIAL CARGADO EXITOSAMENTE" TO LK-MSG
              END-IF
           END-IF.
       9000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       9900-TRATAR-ERROR.
           SET FETCH-ERROR TO TRUE
           MOVE "ER"       TO LK-CODE.
       9900-FIN.
           EXIT.
