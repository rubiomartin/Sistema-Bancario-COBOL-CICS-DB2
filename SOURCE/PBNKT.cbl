       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PBNKT.
       AUTHOR. IBMUSER.
      *
      *****************************************************************
      ** PROGRAMA .........: PBNKT                                  **
      ** TITULO ...........: TRANSACCION DE TRANSFERENCIAS          **
      ** **
      ** TIPO .............: ONLINE                                 **
      ** - LENGUAJE ...............: COBOL                          **
      ** - ENTORNO ................: CICS                           **
      ** - BASE DE DATOS ..........: DB2                            **
      ** **
      ** DESCRIPCION ......:                                        **
      ** **
      ** - Permite realizar transferencias de fondos entre          **
      ** clientes (Origen a Destino).                               **
      ** **
      ** Actualiza la tabla de CLIENTES (Debita al usuario origen   **
      ** y acredita al usuario destino) y genera registros de       **
      ** auditoria en la tabla de MOVIMIENTOS para los dos usuarios.**
      ** **
      ** Incluye validacion de existencia de destino y limites      **
      ** de saldo (Anti-Desbordamiento).                            **
      ** Incluye doble verificacion (Confirmacion de usuario).      **
      ** Utiliza COMMIT/ROLLBACK (SYNCPOINT) para integridad.       **
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * COPIES DE MAPAS, COMMAREA Y UTILIDADES                         *
      *----------------------------------------------------------------*
       COPY BNKTMP.
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY WSCOMM.
       COPY CPYVALWD.
       COPY CICSATTR.

      *----------------------------------------------------------------*
      * DEFINICIONES DB2
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLCLIEN END-EXEC.
           EXEC SQL INCLUDE DCLMOVIM END-EXEC.
       01  WS-MONTO-EDITADO      PIC Z.ZZZ.ZZZ.ZZ9,99.
       01  WA-RESPUESTA-CICS        PIC S9(8) COMP.

      * VARIABLES DE TRABAJO
       01  WS-VARIABLES-TRABAJO.
           05 WS-MSG-EXITO          PIC X(60).
           05 WS-MONTO-DECIMAL      PIC 9(10)V99.
           05 WS-SALDO-ACTUAL       PIC 9(10)V99.
           05 WS-SALDO-NUEVO        PIC S9(11)V99.
           05 WS-USER-ORIGEN        PIC X(8).
           05 WS-USER-DESTINO       PIC X(8).
      * SWITCHES
       01  WS-CONTROL.
           03 SW-ENVIO-MAPA         PIC X     VALUE '0'.
              88 ENVIO-ERASE                  VALUE '1'.
              88 ENVIO-DATAONLY               VALUE '2'.

           03 SW-ERRORES            PIC X     VALUE 'N'.
              88 HAY-ERROR-VALIDACION         VALUE 'S'.
              88 NO-HAY-ERRORES               VALUE 'N'.

           03 SW-RESULTADO          PIC X     VALUE 'N'.
              88 OPERACION-EXITOSA            VALUE 'S'.
              88 OPERACION-FALLIDA            VALUE 'N'.

           03 SW-SALDO-LEIDO        PIC X     VALUE 'N'.

       01  WS-CONSTANTES.
           05 WS-MENSAJE-LOGN       PIC X(25)
               VALUE 'DEBE INGRESAR POR LOGN'.

       01  WC-CONSTANTES.
           03  WC-PROGRAMA          PIC X(8)  VALUE 'PBNKT'.
           03  WC-TRANSACCION       PIC X(4)  VALUE 'BNKT'.
           03  WC-MAP               PIC X(8)  VALUE 'BNKMAPT'.
           03  WC-MAPSET            PIC X(8)  VALUE 'BNKTMP'.

       LINKAGE SECTION.
       01  DFHCOMMAREA              PIC X(74).

       PROCEDURE DIVISION.

      *================================================================*
      * 0000 - DRIVER PRINCIPAL                                        *
      *================================================================*
       0000-PROCESO-TAREA.

           IF EIBCALEN > 0
               MOVE DFHCOMMAREA TO COMMAREA-GLOBAL
           END-IF.

           EVALUATE TRUE
               WHEN EIBCALEN = 0
                   SET ESTADO-ERROR-LOGN TO TRUE
                   PERFORM 9200-ENVIAR-AVISO-TEXTO

               WHEN EIBCALEN > 0 AND ESTADO-ERROR-LOGN
                   PERFORM 9100-SALIR-A-LOGN

               WHEN EIBTRNID NOT = WC-TRANSACCION
                   PERFORM 1000-PREPARAR-DATOS

               WHEN OTHER
                   PERFORM 2000-PROCESAR-INTERACCION
           END-EVALUATE.

           PERFORM 9999-RETORNO-CICS.

      *================================================================*
      * 1000 - INICIALIZACION                                          *
      *================================================================*
       1000-PREPARAR-DATOS.
           MOVE LOW-VALUES TO BNKMAPTO.

      * Leemos nuestro saldo para mostrar en pantalla
           PERFORM 7000-LEER-SALDO-ORIGEN.

           IF SQLCODE = 0
               MOVE HV-SALDO TO SALDOO
               MOVE HV-SALDO TO WS-SALDO-ACTUAL
               MOVE 'S'      TO SW-SALDO-LEIDO
           ELSE
               MOVE 0 TO WS-SALDO-ACTUAL
               MOVE ' ADVERTENCIA: ERROR LEYENDO SALDO' TO MSGO
               MOVE ATTR-RED TO MSGC
           END-IF.

           SET ENVIO-ERASE TO TRUE.
           PERFORM 4000-ENVIO-MAPA.

      *================================================================*
      * 2000 - LOGICA DE NEGOCIO                                     *
      *================================================================*
       2000-PROCESAR-INTERACCION.
           PERFORM 4100-RECIBIR-MAPA.
           SET ENVIO-DATAONLY TO TRUE.


           EVALUATE TRUE
               WHEN EIBAID = DFHENTER
                   PERFORM 2100-ACCION-MAPA

               WHEN EIBAID = DFHPF3
                   PERFORM 2200-TRATAR-SALIDA

               WHEN OTHER
                   MOVE ' TECLA INVALIDA' TO MSGO
           END-EVALUATE.

           PERFORM 4000-ENVIO-MAPA.

       2100-ACCION-MAPA.
           PERFORM 2300-VALIDAR-CAMPOS.

           IF NO-HAY-ERRORES
               IF CONFIRMACION-PENDIENTE
                   PERFORM 2500-EJECUTAR-NEGOCIO
               ELSE
                   PERFORM 2400-PREPARAR-CONFIRMACION
               END-IF
           END-IF.

       2200-TRATAR-SALIDA.
           IF CONFIRMACION-PENDIENTE
               MOVE 'N' TO SW-CONFIRMACION
               MOVE ' OPERACION CANCELADA' TO MSGO
               MOVE SPACES TO CONFRMO
               MOVE ATTR-RED TO MSGC
               PERFORM 4200-DESBLOQUEAR-CAMPOS
           ELSE
               PERFORM 9000-VOLVER-AL-MENU
           END-IF.

       2300-VALIDAR-CAMPOS.
           MOVE 'N' TO SW-ERRORES.

           MOVE FUNCTION UPPER-CASE(USRDESTI) TO USRDESTI.
           MOVE MONTOI TO WS-VAL-ENTRADA.

           PERFORM 9900-RUTINA-VALIDAR-NUMERO.

           IF NO-HAY-ERRORES
               MOVE WS-VAL-SALIDA-V TO WS-MONTO-DECIMAL
           END-IF.

           PERFORM 7300-VALIDAR-DESTINO-DB2.

           EVALUATE TRUE
               WHEN USRDESTI = SPACES OR LOW-VALUES
                   MOVE ' ERROR: INGRESE USUARIO DESTINO' TO MSGO
                   SET HAY-ERROR-VALIDACION TO TRUE

               WHEN SQLCODE NOT = 0
                   MOVE ' ERROR: USUARIO DESTINO NO EXISTE' TO MSGO
                   SET HAY-ERROR-VALIDACION TO TRUE

               WHEN USRDESTI = CG-M-USER
                   MOVE ' ERROR: NO PUEDE TRANSFERIRSE A SI MISMO'
                   TO MSGO
                   SET HAY-ERROR-VALIDACION TO TRUE

               WHEN VAL-HAY-ERROR
                   MOVE ' ERROR: MONTO INVALIDO (FORMATO)' TO MSGO
                   SET HAY-ERROR-VALIDACION TO TRUE

               WHEN WS-VAL-SALIDA <= 0
                   MOVE ' ERROR: EL MONTO DEBE SER MAYOR A 0' TO MSGO
                   SET HAY-ERROR-VALIDACION TO TRUE


           END-EVALUATE.

           IF HAY-ERROR-VALIDACION
               MOVE 'N' TO SW-CONFIRMACION
           END-IF.

       2400-PREPARAR-CONFIRMACION.
           MOVE ' CONFIRME: ENTER=SI PF3=CANCELAR' TO MSGO.
           MOVE ATTR-YELLOW TO MSGC.

           MOVE WS-MONTO-DECIMAL TO WS-MONTO-EDITADO.
           MOVE WS-MONTO-EDITADO TO WS-TRIM-STR-IN.
           MOVE 16               TO WS-TRIM-MAX-LEN.
           PERFORM 9950-ELIMINAR-ESPACIOS-IZQ.

           INITIALIZE CONFRMO.
           STRING ' ¿SEGURO QUIERE TRANSFERIRLE $' DELIMITED BY SIZE
           WS-TRIM-STR-OUT                DELIMITED BY SPACES
           ' A '                          DELIMITED BY SIZE
           USRDESTI                       DELIMITED BY SPACE
           '?'                            DELIMITED BY SIZE
           INTO CONFRMO.

           MOVE 'S' TO SW-CONFIRMACION.
           MOVE ATTR-PROT-MDT TO USRDESTA.
           MOVE ATTR-PROT-MDT TO MONTOA.

       2500-EJECUTAR-NEGOCIO.
      * Inicializamos estado como fallido por defecto
           SET OPERACION-FALLIDA TO TRUE.
           MOVE SPACES TO WS-MSG-EXITO.

           MOVE USRDESTI TO WS-USER-DESTINO.

      * 1. RE-LECTURA para evitar usar un saldo incorrecto
           PERFORM 7000-LEER-SALDO-ORIGEN.

           IF SQLCODE NOT = 0
               MOVE ' ERROR CRITICO LECTURA SALDO' TO MSGO
               MOVE 'N' TO SW-CONFIRMACION
               PERFORM 4200-DESBLOQUEAR-CAMPOS
           ELSE
               MOVE HV-SALDO TO WS-SALDO-ACTUAL
               MOVE 'S'      TO SW-SALDO-LEIDO

               IF WS-MONTO-DECIMAL = 0
                   SET HAY-ERROR-VALIDACION TO TRUE
                   MOVE ' ERROR: MONTO LLEGO EN CERO' TO MSGO
               END-IF

               IF WS-SALDO-ACTUAL < WS-MONTO-DECIMAL
                   MOVE ' FONDOS INSUFICIENTES' TO MSGO
                   MOVE ATTR-RED TO MSGC
                   MOVE 'N' TO SW-CONFIRMACION
                   PERFORM 4200-DESBLOQUEAR-CAMPOS
                   SET HAY-ERROR-VALIDACION TO TRUE
               ELSE
                   PERFORM 2600-VERIFICAR-LIMITE-DESTINO
               END-IF

               IF NO-HAY-ERRORES
                   PERFORM 3000-PERSISTENCIA-DATOS
               END-IF

               IF OPERACION-EXITOSA
                   MOVE LOW-VALUES   TO BNKMAPTO
                   MOVE SPACES       TO CONFRMO
                   MOVE WS-MSG-EXITO TO MSGO
                   PERFORM 4200-DESBLOQUEAR-CAMPOS
                   MOVE SPACES       TO MONTOO
                   MOVE SPACES       TO USRDESTO
                   MOVE WS-SALDO-NUEVO TO WS-SALDO-ACTUAL
                   SET ENVIO-ERASE   TO TRUE
                   MOVE 'N'          TO SW-CONFIRMACION
               END-IF
           END-IF.

      *================================================================*
      * 2600 - VERIFICACION LIMITE DESTINO                             *
      *================================================================*
       2600-VERIFICAR-LIMITE-DESTINO.
           PERFORM 7300-VALIDAR-DESTINO-DB2.

           COMPUTE WS-SALDO-NUEVO = HV-SALDO + WS-MONTO-DECIMAL

           IF WS-SALDO-NUEVO > 99999999,99
               MOVE ' ERROR: DESTINATARIO NO PUEDE RECIBIR TANTO MONTO'
                  TO MSGO
               MOVE ATTR-RED TO MSGC
               MOVE 'N' TO SW-CONFIRMACION
               SET HAY-ERROR-VALIDACION TO TRUE
               PERFORM 4200-DESBLOQUEAR-CAMPOS
           END-IF.

      *================================================================*
      * 3000 - PERSISTENCIA (ACID)                                     *
      *================================================================*
       3000-PERSISTENCIA-DATOS.
           SUBTRACT WS-MONTO-DECIMAL FROM WS-SALDO-ACTUAL
               GIVING WS-SALDO-NUEVO.

           PERFORM 7100-UPDATE-SALDO-ORIGEN.

           IF SQLCODE = 0
               PERFORM 7400-UPDATE-SALDO-DESTINO
               IF SQLCODE = 0
      * ---------------------------------------------------------
      * 1. REGISTRO PARA EL REMITENTE (Salida de dinero)
      * ---------------------------------------------------------
                   MOVE 'T'              TO HV-TIPO-OPER
                   MOVE WS-MONTO-DECIMAL TO HV-MONTO
                   MOVE CG-M-USER        TO HV-USUARIO-MOV
                   MOVE WS-USER-DESTINO  TO HV-USUARIO-REL
                   PERFORM 7200-INSERTAR-HISTORIAL

                   IF SQLCODE = 0
      * ---------------------------------------------------------
      * 2. REGISTRO PARA EL DESTINATARIO (Entrada de dinero)
      * ---------------------------------------------------------
                       MOVE 'R'              TO HV-TIPO-OPER
                       MOVE WS-MONTO-DECIMAL TO HV-MONTO
                       MOVE WS-USER-DESTINO  TO HV-USUARIO-MOV
                       MOVE CG-M-USER        TO HV-USUARIO-REL

                       PERFORM 7200-INSERTAR-HISTORIAL

                       IF SQLCODE = 0
                           EXEC CICS SYNCPOINT END-EXEC
                           SET OPERACION-EXITOSA TO TRUE
                           MOVE ' TRANSFERENCIA EXITOSA' TO WS-MSG-EXITO
                       ELSE
                           EXEC CICS SYNCPOINT ROLLBACK END-EXEC
                           MOVE ' ERROR HISTORIAL DESTINO' TO MSGO
                       END-IF
                   ELSE
                       EXEC CICS SYNCPOINT ROLLBACK END-EXEC
                       MOVE ' ERROR HISTORIAL ORIGEN' TO MSGO
                   END-IF
               ELSE
                   EXEC CICS SYNCPOINT ROLLBACK END-EXEC
                   MOVE ' ERROR AL ACREDITAR DESTINO' TO MSGO
               END-IF
           ELSE
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
               MOVE ' ERROR AL DEBITAR ORIGEN' TO MSGO
           END-IF.

      *================================================================*
      * 4000 - MANEJO DE MAPAS                                         *
      *================================================================*
       4000-ENVIO-MAPA.

           IF SW-SALDO-LEIDO = 'N'
               PERFORM 7000-LEER-SALDO-ORIGEN
               IF SQLCODE = 0
                  MOVE HV-SALDO TO WS-SALDO-ACTUAL
               END-IF
           END-IF.

           MOVE CG-M-USER       TO NOMBREUSO.
           MOVE WS-SALDO-ACTUAL TO SALDOO.

           EVALUATE TRUE
               WHEN ENVIO-ERASE
                   EXEC CICS SEND MAP('BNKMAPT') MAPSET('BNKTMP')
                        FROM(BNKMAPTO) ERASE FREEKB END-EXEC
               WHEN ENVIO-DATAONLY
                   EXEC CICS SEND MAP('BNKMAPT') MAPSET('BNKTMP')
                        FROM(BNKMAPTO) DATAONLY FREEKB END-EXEC
           END-EVALUATE.

       4100-RECIBIR-MAPA.
           EXEC CICS RECEIVE MAP('BNKMAPT') MAPSET('BNKTMP')
               INTO(BNKMAPTI) RESP(WA-RESPUESTA-CICS) END-EXEC.

       4200-DESBLOQUEAR-CAMPOS.
           MOVE ATTR-UNPROT-MDT     TO USRDESTA.
           MOVE ATTR-UNPROT-NUM-MDT TO MONTOA.

      *================================================================*
      * 7000 - ACCESO A DATOS (DB2)                                    *
      *================================================================*
       7000-LEER-SALDO-ORIGEN.
           MOVE CG-M-USER TO WS-USER-ORIGEN.
           EXEC SQL SELECT SALDO INTO :HV-SALDO
               FROM IBMUSER.CLIENTES WHERE USUARIO = :WS-USER-ORIGEN
           END-EXEC.

       7100-UPDATE-SALDO-ORIGEN.
           MOVE WS-SALDO-NUEVO TO HV-SALDO.
           EXEC SQL UPDATE IBMUSER.CLIENTES SET SALDO = :HV-SALDO
               WHERE USUARIO = :WS-USER-ORIGEN
           END-EXEC.

       7200-INSERTAR-HISTORIAL.
           EXEC SQL INSERT INTO IBMUSER.MOVIMIENTOS
               (USUARIO, TIPO_OPER, MONTO, FECHA, USUARIO_REL)
               VALUES (:HV-USUARIO-MOV, :HV-TIPO-OPER, :HV-MONTO,
                CURRENT TIMESTAMP, :HV-USUARIO-REL)
           END-EXEC.

       7300-VALIDAR-DESTINO-DB2.
           MOVE USRDESTI TO WS-USER-DESTINO.
           EXEC SQL SELECT SALDO INTO :HV-SALDO
               FROM IBMUSER.CLIENTES WHERE USUARIO = :WS-USER-DESTINO
           END-EXEC.

       7400-UPDATE-SALDO-DESTINO.
           MOVE WS-MONTO-DECIMAL TO HV-MONTO.
           EXEC SQL UPDATE IBMUSER.CLIENTES
               SET SALDO = SALDO + :HV-MONTO
               WHERE USUARIO = :WS-USER-DESTINO
           END-EXEC.

       9000-VOLVER-AL-MENU.
           INITIALIZE CH-COMUN.
           MOVE 'BNKT' TO CH-TRANS-RETORNO.
           EXEC CICS XCTL PROGRAM(CS-PGM-MENU)
               COMMAREA(COMMAREA-GLOBAL) RESP(WA-RESPUESTA-CICS)
           END-EXEC.

       9100-SALIR-A-LOGN.
           EXEC CICS XCTL PROGRAM (CS-PGM-LOGIN) END-EXEC.

       9200-ENVIAR-AVISO-TEXTO.
           EXEC CICS SEND TEXT FROM (WS-MENSAJE-LOGN)
               LENGTH (LENGTH OF WS-MENSAJE-LOGN)
               ERASE FREEKB END-EXEC.

       9999-RETORNO-CICS.
           EXEC CICS RETURN TRANSID(WC-TRANSACCION)
               COMMAREA(COMMAREA-GLOBAL)
           END-EXEC.

       COPY CPYVALPD.
