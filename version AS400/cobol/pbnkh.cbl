       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBNKH.
       AUTHOR. MARTIN RUBIO.
      *================================================================*
      * TITULO   : HISTORIAL DE MOVIMIENTOS (ARQUITECTURA 3 FORMATOS)  *
      * ENTORNO  : IBM i (OS/400) - ILE COBOL                          *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS CRT.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PANTALLA-HIST ASSIGN TO WORKSTATION-BNKHMP-SI
                  ORGANIZATION IS TRANSACTION
                  ACCESS IS DYNAMIC
                  RELATIVE KEY IS WS-RRN.

       DATA DIVISION.
       FILE SECTION.
       FD PANTALLA-HIST.
       01 HIST-RECORD PIC X(200).

       WORKING-STORAGE SECTION.
           EXEC SQL SET OPTION COMMIT = *CHG END-EXEC.

       01 WS-SFL-O.
          COPY DDS-BNKMAPHS-O OF BNKHMP.

       01 WS-CTL-O.
          COPY DDS-BNKMAPHC-O OF BNKHMP.


       01 WS-FTR-O.
          COPY DDS-BNKMAPHF-O OF BNKHMP.

           EXEC SQL INCLUDE SQLCA END-EXEC.

       01 WS-VARIABLES-TRABAJO.
          05 SW-FIN-PROG         PIC X(1) VALUE "N".
             88 FIN-PROG         VALUE "S".
          05 WS-RRN              PIC 9(4) VALUE ZERO.
          05 SW-FIN-CURSOR       PIC X(1) VALUE "N".
             88 FIN-CURSOR       VALUE "S".
          05 WS-USER-CURSOR      PIC X(8).

       01 INDICADORES-PANTALLA.
          05 IND03               PIC 1 INDIC 03.
             88 IND-SALIR        VALUE B"1".
          05 IND40               PIC 1 INDIC 40.
             88 SFLDSP-ON        VALUE B"1".
             88 SFLDSP-OFF       VALUE B"0".
          05 IND41               PIC 1 INDIC 41.
             88 SFLDSPCTL-ON     VALUE B"1".
             88 SFLDSPCTL-OFF    VALUE B"0".
          05 IND42               PIC 1 INDIC 42.
             88 SFLCLR-ON        VALUE B"1".
             88 SFLCLR-OFF       VALUE B"0".
          05 IND43               PIC 1 INDIC 43.
             88 SFLEND-ON        VALUE B"1".
             88 SFLEND-OFF       VALUE B"0".

       01  DCLCLIEN.
           10 HV-SALDO             PIC S9(8)V9(2) USAGE COMP-3.

       01  DCLMOVIMIENTOS.
           10 HV-TIPO-OPER         PIC X(1).
           10 HV-MONTO             PIC S9(8)V9(2) USAGE COMP-3.
           10 HV-FECHA-CHAR        PIC X(10).
           10 HV-USUARIO-REL       PIC X(8).

           EXEC SQL
               DECLARE CURHISTORIAL CURSOR FOR
               SELECT CHAR(DATE(FECHA), ISO), TIPO_OPER, MONTO,
                      USUARIO_REL
               FROM MOVIMIENTOS
               WHERE USUARIO = :WS-USER-CURSOR
               ORDER BY FECHA DESC
           END-EXEC.

       LINKAGE SECTION.
       01 COMMAREA-GLOBAL.
          03 CG-CONTEXTO-USUARIO.
             05 CG-M-USER               PIC X(08).
          03 CG-NAVEGACION.
             05 CH-TRANS-RETORNO        PIC X(04).
             05 CH-PROGRAMA-RETORNO     PIC X(08).
             05 CH-XCTL                 PIC X(08).
          03 CG-ESTADOS.
             05 CG-ENTRADA-INCORRECTA   PIC X(01).
                88 ESTADO-ERROR-LOGN    VALUE "E".
                88 ESTADO-NORMAL        VALUE "N".

       PROCEDURE DIVISION USING COMMAREA-GLOBAL.
       0000-MAIN-LOGIC.
           OPEN I-O PANTALLA-HIST.
           MOVE 'N' TO SW-FIN-PROG.

           INITIALIZE WS-CTL-O.
           INITIALIZE WS-FTR-O.

           MOVE CG-M-USER TO NOMBREUS OF BNKMAPHC-O.
           PERFORM 7000-LEER-SALDO-ACTUAL.

           PERFORM UNTIL FIN-PROG
               PERFORM 1000-LIMPIAR-SUBFILE
               PERFORM 2000-CARGAR-SUBFILE
               PERFORM 3000-MOSTRAR-PANTALLA

               IF IND-SALIR
                  SET FIN-PROG TO TRUE
               END-IF
           END-PERFORM.

           CLOSE PANTALLA-HIST.
           GOBACK.

       1000-LIMPIAR-SUBFILE.
           SET SFLDSP-OFF    TO TRUE
           SET SFLDSPCTL-OFF TO TRUE
           SET SFLCLR-ON     TO TRUE
           SET SFLEND-OFF    TO TRUE
           MOVE ZERO TO WS-RRN.

           WRITE HIST-RECORD FROM WS-CTL-O
                 FORMAT IS "BNKMAPHC"
                 INDICATORS ARE INDICADORES-PANTALLA
           END-WRITE.

           SET SFLCLR-OFF TO TRUE.

       2000-CARGAR-SUBFILE.
           MOVE "N" TO SW-FIN-CURSOR.
           MOVE CG-M-USER TO WS-USER-CURSOR.
           EXEC SQL OPEN CURHISTORIAL END-EXEC.

           PERFORM UNTIL FIN-CURSOR
               INITIALIZE DCLMOVIMIENTOS
               EXEC SQL
                   FETCH CURHISTORIAL
                   INTO :HV-FECHA-CHAR, :HV-TIPO-OPER,
                        :HV-MONTO, :HV-USUARIO-REL
               END-EXEC

               IF SQLCODE = 0
                  ADD 1 TO WS-RRN
                  INITIALIZE WS-SFL-O
                  MOVE HV-FECHA-CHAR  TO FECHAD OF BNKMAPHS-O
                  MOVE HV-TIPO-OPER   TO TIPOD OF BNKMAPHS-O
                  MOVE HV-MONTO       TO MONTOD OF BNKMAPHS-O
                  MOVE HV-USUARIO-REL TO USRRELD OF BNKMAPHS-O

                  WRITE SUBFILE HIST-RECORD FROM WS-SFL-O
                        FORMAT IS "BNKMAPHS"
                        INDICATORS ARE INDICADORES-PANTALLA
                        INVALID KEY CONTINUE
                  END-WRITE
               ELSE
                  SET FIN-CURSOR TO TRUE
               END-IF
           END-PERFORM.

           EXEC SQL CLOSE CURHISTORIAL END-EXEC.

       3000-MOSTRAR-PANTALLA.
           IF WS-RRN > 0
              SET SFLDSP-ON    TO TRUE
              SET SFLDSPCTL-ON TO TRUE
              SET SFLEND-ON    TO TRUE
              MOVE SPACES TO MSGO OF BNKMAPHF-O
           ELSE
              SET SFLDSP-OFF   TO TRUE
              SET SFLDSPCTL-ON TO TRUE
              MOVE "NO EXISTEN MOVIMIENTOS PARA MOSTRAR."
                TO MSGO OF BNKMAPHF-O
           END-IF.

           WRITE HIST-RECORD FROM WS-CTL-O
                 FORMAT IS "BNKMAPHC"
                 INDICATORS ARE INDICADORES-PANTALLA
           END-WRITE.

           WRITE HIST-RECORD FROM WS-FTR-O
                 FORMAT IS "BNKMAPHF"
                 INDICATORS ARE INDICADORES-PANTALLA
           END-WRITE.

           READ PANTALLA-HIST RECORD
                 FORMAT IS "BNKMAPHC"
                 INDICATORS ARE INDICADORES-PANTALLA
                 AT END CONTINUE
           END-READ.

       7000-LEER-SALDO-ACTUAL.
           INITIALIZE DCLCLIEN.
           EXEC SQL
                SELECT SALDO INTO :HV-SALDO
                FROM CLIENTES
                WHERE USUARIO = :CG-M-USER
           END-EXEC.
           IF SQLCODE = 0
              MOVE HV-SALDO TO SALDO OF BNKMAPHC-O
           END-IF.
