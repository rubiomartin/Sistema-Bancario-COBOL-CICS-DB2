       PROCESS QUOTE.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBNKLAPI.
       AUTHOR. MARTIN RUBIO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.

       01  INDICADORES-PROCESO.
           05 ESTADO-SQL               PIC X(01) VALUE 'P'.
              88 USUARIO-ENCONTRADO    VALUE 'O'.
              88 USUARIO-INEXISTENTE   VALUE 'N'.
              88 ERROR-DB              VALUE 'E'.
           05 ESTADO-LOGIN             PIC X(01) VALUE 'P'.
              88 LOGIN-OK              VALUE 'O'.
              88 LOGIN-PASS-INVALIDA   VALUE 'I'.
              88 LOGIN-USR-INVALIDO    VALUE 'U'.
              88 LOGIN-ERROR-SISTEMA   VALUE 'S'.

       01  HV-VARIABLES.
           05 HV-USUARIO               PIC X(8).
           05 HV-PASSWORD              PIC X(50).

       LINKAGE SECTION.
       01  LK-INPUT.
           05 LK-USER                  PIC X(8).
           05 LK-PASS                  PIC X(50).

       01  LK-OUTPUT.
           05 LK-CODE                  PIC X(02).
           05 LK-MSG                   PIC X(40).

       PROCEDURE DIVISION USING LK-INPUT LK-OUTPUT.
       COMIENZO.
           PERFORM 1000-INICIALIZAR       THRU 1000-FIN
           PERFORM 2000-CONSULTAR-USUARIO THRU 2000-FIN
           
           IF USUARIO-ENCONTRADO
              PERFORM 3000-VALIDAR-PASS   THRU 3000-FIN
           END-IF
           
           PERFORM 9000-FINALIZAR         THRU 9000-FIN
           GOBACK.

      *----------------------------------------------------------------*
       1000-INICIALIZAR.
           EXEC SQL
               SET OPTION COMMIT = *NONE
           END-EXEC.

           INITIALIZE LK-OUTPUT
           MOVE LK-USER TO HV-USUARIO.
       1000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2000-CONSULTAR-USUARIO.
           EXEC SQL
               SELECT PASSWORD
               INTO :HV-PASSWORD
               FROM USUARIOS
               WHERE ID_USUARIO = :HV-USUARIO
           END-EXEC.

           EVALUATE SQLCODE
              WHEN 0
                   SET USUARIO-ENCONTRADO  TO TRUE
              WHEN 100
                   SET USUARIO-INEXISTENTE TO TRUE
                   SET LOGIN-USR-INVALIDO  TO TRUE
              WHEN OTHER
                   SET ERROR-DB            TO TRUE
                   SET LOGIN-ERROR-SISTEMA TO TRUE
           END-EVALUATE.
       2000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       3000-VALIDAR-PASS.
           IF HV-PASSWORD = LK-PASS
              SET LOGIN-OK            TO TRUE
           ELSE
              SET LOGIN-PASS-INVALIDA TO TRUE
           END-IF.
       3000-FIN.
           EXIT.

      *----------------------------------------------------------------*
       9000-FINALIZAR.
           EVALUATE TRUE
              WHEN LOGIN-OK
                   MOVE "OK" TO LK-CODE
                   MOVE "AUTENTICACION EXITOSA" TO LK-MSG
                   
              WHEN LOGIN-PASS-INVALIDA
                   MOVE "WP" TO LK-CODE
                   MOVE "CONTRASENA INCORRECTA" TO LK-MSG
                   
              WHEN LOGIN-USR-INVALIDO
                   MOVE "NE" TO LK-CODE
                   MOVE "EL USUARIO NO EXISTE" TO LK-MSG
                   
              WHEN OTHER
                   MOVE "ER" TO LK-CODE
                   MOVE "ERROR CRITICO EN LA BASE DE DATOS" TO LK-MSG
           END-EVALUATE.
       9000-FIN.
           EXIT.