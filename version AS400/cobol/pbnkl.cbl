       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBNKL.
       AUTHOR. MARTIN RUBIO.
      *================================================================*
      * TITULO   : CONTROL DE ACCESO (LOGIN)                           *
      * ENTORNO  : IBM i (OS/400) - ILE COBOL                          *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS CRT.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PANTALLA-LOGIN ASSIGN TO WORKSTATION-BNKLMP-SI
                  ORGANIZATION IS TRANSACTION
                  ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD PANTALLA-LOGIN.
       01 LOGIN-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-PANTALLA-LOGIN.
          COPY DDS-ALL-FORMATS OF BNKLMP.

      *--- AREA DE COMUNICACION GLOBAL ---*
      *   COPY WSCOMM.

       01  DCLCLIEN.
      *                       USUARIO
           10 HV-USUARIO           PIC X(8).
      *                       PASSWORD
           10 HV-PASSWORD          PIC X(8).
      *                       NOMBRE
           10 HV-NOMBRE            PIC X(20).
      *                       SALDO
           10 HV-SALDO             PIC S9(8)V9(2) USAGE COMP-3.

      *================================================================
      * AREA DE COMUNICACION GLOBAL (Reemplazo de COMMAREA CICS)
      *================================================================
       01 COMMAREA-GLOBAL.
          03 CG-CONTEXTO-USUARIO.
             05 CG-M-USER               PIC X(08) VALUE SPACES.
          03 CG-NAVEGACION.
             05 CH-TRANS-RETORNO        PIC X(04) VALUE SPACES.
             05 CH-PROGRAMA-RETORNO     PIC X(08) VALUE SPACES.
             05 CH-XCTL                 PIC X(08) VALUE SPACES.
          03 CG-ESTADOS.
             05 CG-ENTRADA-INCORRECTA   PIC X(01) VALUE 'N'.
                88 ESTADO-ERROR-LOGN    VALUE 'E'.
                88 ESTADO-NORMAL        VALUE 'N'.
             05 SW-CONFIRMACION         PIC X(01) VALUE 'N'.
                88 CONFIRMACION-PENDIENTE VALUE 'S'.
      *    03 CG-HISTORIAL.
      *       05 CG-H-OPER               PIC X(01) VALUE SPACE.
      *       05 CG-H-ORDEN              PIC X(01) VALUE 'D'.
      *       05 CG-H-ID1                PIC S9(9) COMP.
      *       05 CG-H-ID4                PIC S9(9) COMP.
      *       05 CG-H-UP-MORE            PIC X(01) VALUE '-'.
      *       05 CG-H-DW-MORE            PIC X(01) VALUE '+'.

       01 WS-VARIABLES-TRABAJO.
          05 WS-USUARIO-INPUT    PIC X(8).
          05 WS-PASSWORD-INPUT   PIC X(8).
          05 SW-FIN-SISTEMA      PIC X(1) VALUE 'N'.
             88 FIN-SISTEMA      VALUE 'S'.

       01  INDICADORES-PANTALLA.
           05  IND03                    PIC 1 INDIC 03.
               88  IND-SALIR                VALUE B"1".
           05  IND10                    PIC 1 INDIC 10.
               88  USER-NO-REG              VALUE B"1".
               88  USER-REG                 VALUE B"0".
           05  IND20                    PIC 1 INDIC 20.
               88  PASSWORD-MAL              VALUE B"1".
               88  PASSWORD-BIEN             VALUE B"0".
           05  IND30                    PIC 1 INDIC 30.
               88  ERROR-DB2              VALUE B"1".
               88  DB2-OK                 VALUE B"0".

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       PROCEDURE DIVISION.
       0000-MAIN-LOGIC.
           exec sql
               set option commit = *NONE
           end-exec
           OPEN I-O PANTALLA-LOGIN.

           INITIALIZE WS-PANTALLA-LOGIN

           PERFORM 1000-VALIDAR-LOGIN UNTIL FIN-SISTEMA

           CLOSE PANTALLA-LOGIN.
           GOBACK.

       1000-VALIDAR-LOGIN.
           WRITE LOGIN-RECORD FROM WS-PANTALLA-LOGIN
                 FORMAT IS "BNKMAPL"
                 INDICATORS ARE INDICADORES-PANTALLA
           END-WRITE.
           READ PANTALLA-LOGIN RECORD INTO WS-PANTALLA-LOGIN
                FORMAT IS "BNKMAPL"
                INDICATORS ARE INDICADORES-PANTALLA
                AT END CONTINUE
           END-READ.
           SET USER-REG TO TRUE
           SET PASSWORD-BIEN TO TRUE
           SET DB2-OK TO TRUE
           IF IND-SALIR
                  SET FIN-SISTEMA TO TRUE
           ELSE
                  MOVE USERI OF BNKMAPL-I TO WS-USUARIO-INPUT
                  MOVE PASSI OF BNKMAPL-I TO WS-PASSWORD-INPUT
           END-IF
           EXEC SQL
                SELECT PASSWRD INTO :HV-PASSWORD
                FROM CLIENTES
                WHERE USUARIO = :WS-USUARIO-INPUT
           END-EXEC.

           EVALUATE SQLCODE
               WHEN 0
                  IF HV-PASSWORD = WS-PASSWORD-INPUT
                      MOVE WS-USUARIO-INPUT TO CG-M-USER
                      CALL "PBNKM" USING COMMAREA-GLOBAL
                      INITIALIZE WS-PANTALLA-LOGIN
                      INITIALIZE WS-VARIABLES-TRABAJO
                  ELSE
                     SET PASSWORD-MAL TO TRUE
                  END-IF
               WHEN 100
                  SET USER-NO-REG TO TRUE
               WHEN OTHER
                  SET ERROR-DB2 TO TRUE
           END-EVALUATE.
