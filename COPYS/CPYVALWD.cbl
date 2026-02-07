      *----------------------------------------------------------------*

      * CPYUTLW: LIBRERIA DE VARIABLES (TRIM + VALIDACION AVANZADA)    *

      *----------------------------------------------------------------*



       01  WS-TRIM-CONTROLES.

           05 WS-TRIM-STR-IN          PIC X(50) VALUE SPACES.

           05 WS-TRIM-STR-OUT         PIC X(50) VALUE SPACES.

           05 WS-TRIM-IND             PIC 99    VALUE 0.

           05 WS-TRIM-INICIO          PIC 99    VALUE 0.

           05 WS-TRIM-LARGO           PIC 99    VALUE 0.

           05 WS-TRIM-MAX-LEN         PIC 99    VALUE 50.





       01  WS-AREA-VALIDACION-NUM.

           05 WS-VAL-ENTRADA       PIC X(12).

           05 WS-VAL-SALIDA        PIC 9(12).

      * REDEFINES para interpretar los centavos correctamente

           05 WS-VAL-SALIDA-V      REDEFINES WS-VAL-SALIDA

                                   PIC 9(10)V99.

           05 WS-VAL-INDICE-IN     PIC 9(02).

           05 WS-VAL-INDICE-OUT    PIC 9(02).

           05 WS-VAL-CHAR          PIC X(01).

           05 SW-VAL-ERROR         PIC X(01).

              88 VAL-HAY-ERROR             VALUE 'S'.

           05 WS-VAL-POS-COMA      PIC 9(02).

           05 WS-VAL-START-AUX     PIC 9(02).