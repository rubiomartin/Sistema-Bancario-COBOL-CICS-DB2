       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBNKM.
       AUTHOR. MARTIN RUBIO.
      *================================================================*
      * TITULO   : MENU PRINCIPAL DEL SISTEMA                          *
      * ENTORNO  : IBM i (OS/400) - ILE COBOL                          *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS CRT.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PANTALLA-MENU ASSIGN TO WORKSTATION-BNKMMP-SI
                  ORGANIZATION IS TRANSACTION
                  ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD PANTALLA-MENU.
       01 MENU-RECORD PIC X(150).

       WORKING-STORAGE SECTION.
       01 WS-PANTALLA-MENU.
          COPY DDS-ALL-FORMATS OF BNKMMP.

       01 WS-VARIABLES-TRABAJO.
          05 SW-FIN-MENU         PIC X(1) VALUE 'N'.
             88 FIN-MENU         VALUE 'S'.

       01 INDICADORES-PANTALLA.
          05 IND03               PIC 1 INDIC 03.
             88 IND-SALIR        VALUE B"1".
          05 IND12               PIC 1 INDIC 12.
             88 IND-VOLVER       VALUE B"1".

       LINKAGE SECTION.
      *================================================================
      * AREA DE COMUNICACION GLOBAL (Recibida de PBNKL)
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
           IF CG-M-USER = SPACES
              SET ESTADO-ERROR-LOGN TO TRUE
              GOBACK
           END-IF.

           OPEN I-O PANTALLA-MENU.
           MOVE 'N' TO SW-FIN-MENU.

           INITIALIZE WS-PANTALLA-MENU.
           MOVE CG-M-USER TO NOMBREUS OF BNKMAPM-O.

           PERFORM UNTIL FIN-MENU
               MOVE SPACES TO MSGO OF BNKMAPM-O
               MOVE SPACE  TO OPCIONI OF BNKMAPM-I

               WRITE MENU-RECORD FROM WS-PANTALLA-MENU
                FORMAT IS "BNKMAPM"
                INDICATORS ARE INDICADORES-PANTALLA
               END-WRITE

               READ PANTALLA-MENU RECORD INTO WS-PANTALLA-MENU
                 FORMAT IS "BNKMAPM"
                 INDICATORS ARE INDICADORES-PANTALLA
                 AT END CONTINUE
               END-READ

               IF IND-SALIR OR IND-VOLVER
                  SET FIN-MENU TO TRUE
               ELSE
                  PERFORM 1000-EVALUAR-OPCION
               END-IF
           END-PERFORM.

           CLOSE PANTALLA-MENU.
           GOBACK.

       1000-EVALUAR-OPCION.
           EVALUATE OPCIONI OF BNKMAPM-I
               WHEN '1'
                    CALL "PBNKX" USING COMMAREA-GLOBAL

               WHEN '2'
                    CALL "PBNKT" USING COMMAREA-GLOBAL

               WHEN '3'
                    CALL "PBNKH" USING COMMAREA-GLOBAL
               WHEN OTHER
                  MOVE "OPCION INVALIDA. INGRESE 1, 2 O 3."
                    TO MSGO OF BNKMAPM-O
           END-EVALUATE.
