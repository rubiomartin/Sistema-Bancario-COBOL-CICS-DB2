       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PBNKX.
       AUTHOR. IBMUSER.
      *
      *****************************************************************
      ** PROGRAMA .........: PBNKX                                  **
      ** TITULO ...........: TRANSACCION DE DEPOSITOS Y RETIROS     **
      ** **
      ** TIPO .............: ONLINE                                 **
      ** - LENGUAJE ...............: COBOL                          **
      ** - ENTORNO ................: CICS                           **
      ** - BASE DE DATOS ..........: DB2                            **
      ** **
      ** DESCRIPCION ......:                                        **
      ** **
      ** - Permite realizar operaciones de Caja.                    **
      ** Tipos de operacion:                                        **
      ** 'D' -> Deposito (Suma al saldo).                           **
      ** 'R' -> Retiro   (Resta al saldo si hay fondos).            **
      ** **
      ** Actualiza la tabla de CLIENTES y genera un registro        **
      ** en la tabla de MOVIMIENTOS.                                **
      ** Incluye doble verificacion (Confirmacion de usuario)       **
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
       COPY BNKXMP.
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

       01  WA-RESPUESTA-CICS        PIC S9(8) COMP.
       01  WS-MONTO-EDITADO         PIC Z.ZZZ.ZZZ.ZZ9,99.
      * VARIABLES DE CONTROL Y MENSAJES
       01  WS-VARIABLES-TRABAJO.
           05 WS-MSG-EXITO          PIC X(60).
           05 WS-MONTO-DECIMAL      PIC 9(10)V99.
           05 WS-SALDO-ACTUAL       PIC 9(10)V99.
           05 WS-SALDO-NUEVO        PIC 9(10)V99.
      * Variable puente para el usuario en DB2
           05 WS-USER-DB2           PIC X(8).

       01  WS-CONTROL.
           05 SW-ENVIO-MAPA PIC X.
              88 ENVIO-ERASE                  VALUE '1'.
              88 ENVIO-DATAONLY               VALUE '2'.
      * SWITCH DE VALIDACION
           05 SW-ERRORES PIC X.
              88 HAY-ERROR-VALIDACION         VALUE 'S'.
              88 NO-HAY-ERRORES               VALUE 'N'.

           05 SW-RESULTADO          PIC X     VALUE 'N'.
              88 OPERACION-EXITOSA            VALUE 'S'.
              88 OPERACION-FALLIDA            VALUE 'N'.

           05 SW-SALDO-LEIDO        PIC X     VALUE 'N'.

       01  WS-CONSTANTES.
           05 WS-MENSAJE-LOGN       PIC X(25)
              VALUE 'DEBE INGRESAR POR LOGN'.

       01  WC-CONSTANTES.
           03  WC-PROGRAMA          PIC X(8)  VALUE 'PBNKX'.
           03  WC-TRANSACCION       PIC X(4)  VALUE 'BNKX'.
           03  WC-MAP               PIC X(8)  VALUE 'BNKMAPX'.
           03  WC-MAPSET            PIC X(8)  VALUE 'BNKXMP'.

       LINKAGE SECTION.
       01  DFHCOMMAREA              PIC X(126).

       PROCEDURE DIVISION.

      *================================================================*
      * 0000 - CONTROL PRINCIPAL                                       *
      *================================================================*
       0000-PROCESO-TAREA.

           IF EIBCALEN > 0
               MOVE DFHCOMMAREA TO COMMAREA-GLOBAL
           END-IF.

           EVALUATE TRUE
               WHEN EIBCALEN = 0
                   SET ESTADO-ERROR-LOGN TO TRUE
                   PERFORM 9200-ENVIAR-AVISO-TEXTO

      * 2. REDIRECCION: Usuario presiono Enter tras el error
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
           MOVE LOW-VALUES TO BNKMAPXO.

      * Al entrar, buscamos el saldo para mostrarlo en pantalla
           PERFORM 7000-LEER-SALDO-DB2.

           IF SQLCODE = 0
               MOVE HV-SALDO TO SALDOO
               MOVE HV-SALDO TO WS-SALDO-ACTUAL
               MOVE 'S'      TO SW-SALDO-LEIDO
           ELSE

               MOVE 0 TO WS-SALDO-ACTUAL
               MOVE ' ADVERTENCIA: NO SE PUDO LEER SALDO INICIAL'
               TO MSGO
               MOVE DFHRED TO MSGC
           END-IF.
           SET ENVIO-ERASE         TO TRUE
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
                   MOVE SPACES TO MSGO
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
      * Cancelar: Apagamos switch, desbloqueamos y avisamos
               MOVE 'N' TO SW-CONFIRMACION
               MOVE SPACES TO MSGO
               MOVE SPACES TO CONFRMO
               MOVE ' OPERACION CANCELADA' TO MSGO
               PERFORM 4200-DESBLOQUEAR-CAMPOS
           ELSE
               PERFORM 9000-VOLVER-AL-MENU
           END-IF.

       2300-VALIDAR-CAMPOS.
           MOVE 'N' TO SW-ERRORES.
           MOVE FUNCTION UPPER-CASE(TIPOOPERI) TO TIPOOPERI.

           MOVE MONTOI TO WS-VAL-ENTRADA.
           PERFORM 9900-RUTINA-VALIDAR-NUMERO.

           IF NO-HAY-ERRORES
               MOVE WS-VAL-SALIDA-V TO WS-MONTO-DECIMAL
           END-IF.

           MOVE SPACES TO MSGO.

           EVALUATE TRUE
               WHEN VAL-HAY-ERROR
                   MOVE ' ERROR: MONTO INVALIDO' TO MSGO
                   SET HAY-ERROR-VALIDACION TO TRUE

               WHEN WS-VAL-SALIDA <= 0
                   MOVE ' ERROR: MONTO DEBE SER > 0' TO MSGO
                   SET HAY-ERROR-VALIDACION TO TRUE

               WHEN TIPOOPERI NOT = 'D' AND TIPOOPERI NOT = 'R'
                   MOVE ' ERROR: OPERACION INVALIDA' TO MSGO
                   SET HAY-ERROR-VALIDACION TO TRUE
           END-EVALUATE.

           IF HAY-ERROR-VALIDACION
               MOVE 'N' TO SW-CONFIRMACION
           END-IF.

       2400-PREPARAR-CONFIRMACION.
           MOVE SPACES TO MSGO
           MOVE ' CONFIRME: ENTER=SI PF3=CANCELAR' TO MSGO.
           MOVE ATTR-YELLOW TO MSGC.

           MOVE WS-MONTO-DECIMAL TO WS-MONTO-EDITADO.
           MOVE WS-MONTO-EDITADO TO WS-TRIM-STR-IN.
           MOVE 16               TO WS-TRIM-MAX-LEN.
           PERFORM 9950-ELIMINAR-ESPACIOS-IZQ.
           INITIALIZE CONFRMO.

           IF TIPOOPERI = 'D'
               STRING ' ¿SEGURO QUIERE DEPOSITAR $' DELIMITED BY SIZE
               WS-TRIM-STR-OUT                DELIMITED BY SPACES
               '?'                            DELIMITED BY SIZE
               INTO CONFRMO
           ELSE
               STRING ' ¿SEGURO QUIERE RETIRAR $' DELIMITED BY SIZE
               WS-TRIM-STR-OUT                DELIMITED BY SPACES
               '?'                            DELIMITED BY SIZE
               INTO CONFRMO
           END-IF.

      * Encendemos el switch que se guardara en Commarea
           MOVE 'S' TO SW-CONFIRMACION.

      * Bloqueamos campos
           MOVE ATTR-PROT TO TIPOOPERA.
           MOVE ATTR-PROT TO MONTOA.

       2500-EJECUTAR-NEGOCIO.
      * Inicializamos estado como fallido por defecto
           SET OPERACION-FALLIDA TO TRUE.
           MOVE SPACES TO WS-MSG-EXITO.

      * Re-lectura para consistencia
           PERFORM 7000-LEER-SALDO-DB2.

           IF SQLCODE NOT = 0
               MOVE 'ERROR CRITICO LECTURA SALDO' TO MSGO
               MOVE 'N' TO SW-CONFIRMACION
               PERFORM 4200-DESBLOQUEAR-CAMPOS
           ELSE
               MOVE HV-SALDO TO WS-SALDO-ACTUAL
               MOVE 'S'      TO SW-SALDO-LEIDO
               MOVE SPACES   TO MSGO

               EVALUATE TIPOOPERI
                   WHEN 'D'
                       ADD WS-MONTO-DECIMAL TO WS-SALDO-ACTUAL
                           GIVING WS-SALDO-NUEVO
                       IF WS-SALDO-NUEVO > 99999999,99
                           MOVE ' ERROR: SALDO SUPERA LIMITE MAXIMO'
                             TO MSGO
                           MOVE ATTR-RED TO MSGC
                           SET HAY-ERROR-VALIDACION TO TRUE
                       END-IF

                   WHEN 'R'
                       IF WS-SALDO-ACTUAL < WS-MONTO-DECIMAL
                           MOVE ' FONDOS INSUFICIENTES' TO MSGO
                           MOVE ATTR-RED TO MSGC
                           SET HAY-ERROR-VALIDACION TO TRUE
                       ELSE
                           SUBTRACT WS-MONTO-DECIMAL
                            FROM WS-SALDO-ACTUAL GIVING WS-SALDO-NUEVO
                       END-IF
               END-EVALUATE

               IF HAY-ERROR-VALIDACION
                   MOVE 'N' TO SW-CONFIRMACION
                   PERFORM 4200-DESBLOQUEAR-CAMPOS
               ELSE
      * Si los calculos estan bien, buscamos el commit en db2
                   PERFORM 3000-PERSISTENCIA-DATOS
               END-IF


               IF OPERACION-EXITOSA
                   MOVE LOW-VALUES   TO BNKMAPXO
                   MOVE SPACES       TO CONFRMO
                   MOVE WS-MSG-EXITO TO MSGO
                   MOVE ATTR-GREEN   TO MSGC
                   PERFORM 4200-DESBLOQUEAR-CAMPOS
                   MOVE SPACES       TO MONTOO
                   MOVE SPACES       TO TIPOOPERO
                   MOVE WS-SALDO-NUEVO TO WS-SALDO-ACTUAL
                   SET ENVIO-ERASE   TO TRUE
                   MOVE 'N'          TO SW-CONFIRMACION
               END-IF
           END-IF.

      *================================================================*
      * 3000 - PERSISTENCIA (ACID)                                     *
      *================================================================*
       3000-PERSISTENCIA-DATOS.
           PERFORM 7100-UPDATE-SALDO.
           IF SQLCODE = 0
               PERFORM 7200-INSERTAR-HISTORIAL
               IF SQLCODE = 0
                   EXEC CICS SYNCPOINT END-EXEC


                   SET OPERACION-EXITOSA TO TRUE
                   IF TIPOOPERI = 'D'
                       MOVE ' DEPOSITO EXITOSO' TO WS-MSG-EXITO
                   ELSE
                       MOVE ' RETIRO EXITOSO'   TO WS-MSG-EXITO
                   END-IF
               ELSE
                   EXEC CICS SYNCPOINT ROLLBACK END-EXEC
                   MOVE ' ERROR HISTORIAL' TO MSGO
               END-IF
           ELSE
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
               MOVE ' ERROR UPDATE' TO MSGO
           END-IF.

      *================================================================*
      * 4000 - MANEJO DE MAPAS                                         *
      *================================================================*
       4000-ENVIO-MAPA.
      * Este switch es para verificar si es necesario volver a leer
      * saldo, se puede quitar
           IF SW-SALDO-LEIDO = 'N'
               PERFORM 7000-LEER-SALDO-DB2
               IF SQLCODE = 0
                   MOVE HV-SALDO TO WS-SALDO-ACTUAL
               END-IF
           END-IF.

           MOVE CG-M-USER TO NOMBREUSO
           MOVE WS-SALDO-ACTUAL TO SALDOO.

           EVALUATE TRUE
               WHEN ENVIO-ERASE
                   EXEC CICS SEND MAP('BNKMAPX') MAPSET('BNKXMP')
                        FROM(BNKMAPXO) ERASE FREEKB END-EXEC
               WHEN ENVIO-DATAONLY
                   EXEC CICS SEND MAP('BNKMAPX') MAPSET('BNKXMP')
                        FROM(BNKMAPXO) DATAONLY FREEKB END-EXEC
           END-EVALUATE.

       4100-RECIBIR-MAPA.
           EXEC CICS RECEIVE MAP('BNKMAPX') MAPSET('BNKXMP')
               INTO(BNKMAPXI) RESP(WA-RESPUESTA-CICS) END-EXEC.

       4200-DESBLOQUEAR-CAMPOS.
           MOVE ATTR-UNPROT-MDT     TO TIPOOPERA.
           MOVE ATTR-UNPROT-NUM-MDT TO MONTOA.

      *================================================================*
      * 7000 - CONSULTAS E INSERCIONES A DB2                           *
      *================================================================*
       7000-LEER-SALDO-DB2.
           MOVE CG-M-USER TO WS-USER-DB2.
           EXEC SQL SELECT SALDO INTO :HV-SALDO
               FROM IBMUSER.CLIENTES WHERE USUARIO = :WS-USER-DB2
           END-EXEC.

       7100-UPDATE-SALDO.
           MOVE WS-SALDO-NUEVO TO HV-SALDO.
           EXEC SQL UPDATE IBMUSER.CLIENTES SET SALDO = :HV-SALDO
               WHERE USUARIO = :WS-USER-DB2
           END-EXEC.

       7200-INSERTAR-HISTORIAL.
           IF TIPOOPERI = 'R'
               MOVE 'Z'          TO HV-TIPO-OPER
           ELSE
               MOVE TIPOOPERI        TO HV-TIPO-OPER
           END-IF
           MOVE WS-MONTO-DECIMAL TO HV-MONTO.
           MOVE CG-M-USER        TO HV-USUARIO-MOV.
           EXEC SQL INSERT INTO IBMUSER.MOVIMIENTOS
               (USUARIO, TIPO_OPER, MONTO, FECHA)
               VALUES (:HV-USUARIO-MOV, :HV-TIPO-OPER, :HV-MONTO,
                CURRENT TIMESTAMP)
           END-EXEC.

      *================================================================*
      * 9000 - NAVEGACION Y SALIDA                                     *
      *================================================================*
       9000-VOLVER-AL-MENU.
           INITIALIZE CH-COMUN.
           MOVE 'BNKX' TO CH-TRANS-RETORNO.
           EXEC CICS XCTL PROGRAM(CS-PGM-MENU)
               COMMAREA(COMMAREA-GLOBAL) RESP(WA-RESPUESTA-CICS)
           END-EXEC.

       9100-SALIR-A-LOGN.
           EXEC CICS XCTL
               PROGRAM (CS-PGM-LOGIN)
           END-EXEC.

       9200-ENVIAR-AVISO-TEXTO.
           EXEC CICS SEND TEXT
               FROM (WS-MENSAJE-LOGN)
               LENGTH (LENGTH OF WS-MENSAJE-LOGN)
               ERASE
               FREEKB
           END-EXEC.

       9999-RETORNO-CICS.

           EXEC CICS RETURN TRANSID(WC-TRANSACCION)
               COMMAREA(COMMAREA-GLOBAL)
           END-EXEC.

       COPY CPYVALPD.
