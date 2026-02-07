      *================================================================*
      * COPYBOOK: WSCOMM                                               *
      * DESCRIPCION: AREA DE COMUNICACION GLOBAL (COMMAREA)            *
      *================================================================*
       
       01  COMMAREA-GLOBAL.
           03  CG-CONTEXTO-USUARIO.
               05  CG-M-USER              PIC X(08) VALUE SPACES.

           03  CG-NAVEGACION.
               05  CH-TRANS-RETORNO       PIC X(04) VALUE SPACES.
               05  CH-PROGRAMA-RETORNO    PIC X(08) VALUE SPACES.
               05  CH-XCTL                PIC X(08) VALUE SPACES.
      *----------------------------------------------------------------*
      * 3. BANDERAS DE ESTADO GLOBAL                                   *
      * Control de errores de acceso y logica transaccional            *
      *----------------------------------------------------------------*
           03  CG-ESTADOS.
      * Flag para detectar acceso directo sin pasar por Login
               05  CG-ENTRADA-INCORRECTA  PIC X(01) VALUE 'N'.
                   88 ESTADO-ERROR-LOGN             VALUE 'E'.
                   88 ESTADO-NORMAL                 VALUE 'N'.

      * Flag para transacciones de doble paso (PBNKT / PBNKX)
               05  SW-CONFIRMACION        PIC X(01) VALUE 'N'.
                   88 CONFIRMACION-PENDIENTE        VALUE 'S'.

           03  CG-HISTORIAL.
               05  CG-H-OPER              PIC X(01) VALUE SPACE.
               05  CG-H-ORDEN             PIC X(01) VALUE 'D'.
               05  CG-H-ID1               PIC S9(9) COMP.
               05  CG-H-ID4               PIC S9(9) COMP.
               05  CG-H-UP-MORE           PIC X(01) VALUE '-'.
               05  CG-H-DOWN-MORE         PIC X(01) VALUE '-'.

      *================================================================*
      * CONSTANTES DE PROGRAMAS                                        *
      *================================================================*
       01  CS-PROGRAMAS-BNK.
           03  CS-PGM-LOGIN               PIC X(08) VALUE 'PBNKL   '.
           03  CS-PGM-MENU                PIC X(08) VALUE 'PBNKM   '.
           03  CS-PGM-CONSULTA            PIC X(08) VALUE 'PBNKX   '.
           03  CS-PGM-TRANSFERIR          PIC X(08) VALUE 'PBNKT   '.
           03  CS-PGM-HISTORIAL           PIC X(08) VALUE 'PBNKH   '.