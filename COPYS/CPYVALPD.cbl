      *----------------------------------------------------------------*
      * CPYUTLP: LIBRERIA DE LOGICA (TRIM + VALIDACION NUMERICA)       *
      *----------------------------------------------------------------*
       9950-ELIMINAR-ESPACIOS-IZQ.
           MOVE 0 TO WS-TRIM-IND.
           INSPECT WS-TRIM-STR-IN TALLYING WS-TRIM-IND
                   FOR LEADING SPACES.
           COMPUTE WS-TRIM-INICIO = WS-TRIM-IND + 1.
           COMPUTE WS-TRIM-LARGO = WS-TRIM-MAX-LEN - WS-TRIM-IND.
           INITIALIZE WS-TRIM-STR-OUT.
           IF WS-TRIM-LARGO > 0
               MOVE WS-TRIM-STR-IN(WS-TRIM-INICIO : WS-TRIM-LARGO)
                   TO WS-TRIM-STR-OUT
           END-IF.

       9900-RUTINA-VALIDAR-NUMERO.
           INITIALIZE WS-VAL-SALIDA.
           MOVE 0 TO WS-VAL-POS-COMA.
           INSPECT WS-VAL-ENTRADA REPLACING ALL LOW-VALUES BY SPACES.

      * PASO 1: LOCALIZAR LA COMA
           PERFORM VARYING WS-VAL-INDICE-IN FROM 1 BY 1
                   UNTIL WS-VAL-INDICE-IN > 12 OR WS-VAL-POS-COMA > 0
               IF WS-VAL-ENTRADA(WS-VAL-INDICE-IN:1) = ',' OR
                  WS-VAL-ENTRADA(WS-VAL-INDICE-IN:1) = '.'
                  MOVE WS-VAL-INDICE-IN TO WS-VAL-POS-COMA
               END-IF
           END-PERFORM.

      * PASO 2: PROCESAR SEGUN RESULTADO
           IF WS-VAL-POS-COMA > 0
      * --- SI HAY COMA: PROCESAR DECIMALES (MAXIMO 2) ---
              COMPUTE WS-VAL-START-AUX = WS-VAL-POS-COMA + 1
              IF WS-VAL-START-AUX <= 12 AND
                 WS-VAL-ENTRADA(WS-VAL-START-AUX:1) IS NUMERIC
                 MOVE WS-VAL-ENTRADA(WS-VAL-START-AUX:1)
                   TO WS-VAL-SALIDA(11:1)
              END-IF
              ADD 1 TO WS-VAL-START-AUX
              IF WS-VAL-START-AUX <= 12 AND
                 WS-VAL-ENTRADA(WS-VAL-START-AUX:1) IS NUMERIC
                 MOVE WS-VAL-ENTRADA(WS-VAL-START-AUX:1)
                   TO WS-VAL-SALIDA(12:1)
              END-IF
      * --- PROCESAR PARTE ENTERA ---
              MOVE 10 TO WS-VAL-INDICE-OUT
              COMPUTE WS-VAL-START-AUX = WS-VAL-POS-COMA - 1
              PERFORM VARYING WS-VAL-INDICE-IN FROM WS-VAL-START-AUX
                      BY -1 UNTIL WS-VAL-INDICE-IN < 1
                 IF WS-VAL-ENTRADA(WS-VAL-INDICE-IN:1) IS NUMERIC
                    MOVE WS-VAL-ENTRADA(WS-VAL-INDICE-IN:1)
                      TO WS-VAL-SALIDA(WS-VAL-INDICE-OUT:1)
                    SUBTRACT 1 FROM WS-VAL-INDICE-OUT
                 END-IF
              END-PERFORM
           ELSE
      * --- SI NO HAY COMA: TODO ES ENTERO ---
              MOVE 10 TO WS-VAL-INDICE-OUT
              PERFORM VARYING WS-VAL-INDICE-IN FROM 12 BY -1
                      UNTIL WS-VAL-INDICE-IN < 1
                 IF WS-VAL-ENTRADA(WS-VAL-INDICE-IN:1) IS NUMERIC
                    MOVE WS-VAL-ENTRADA(WS-VAL-INDICE-IN:1)
                      TO WS-VAL-SALIDA(WS-VAL-INDICE-OUT:1)
                    SUBTRACT 1 FROM WS-VAL-INDICE-OUT
                 END-IF
              END-PERFORM
           END-IF.