      ******************************************************************
      * DCLGEN TABLE(MOVIMIENTOS)                                      *
      *        LIBRARY(BANKPRJ.LIB.DCLGEN(DCLMOVIM))                   *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(HV-)                                              *
      *        STRUCTURE(DCLMOVIMIENTOS)                               *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MOVIMIENTOS TABLE
           ( ID_MOV                         INTEGER NOT NULL,
             USUARIO                        CHAR(8) NOT NULL,
             TIPO_OPER                      CHAR(1) NOT NULL,
             MONTO                          DECIMAL(10, 2) NOT NULL,
             FECHA                          TIMESTAMP NOT NULL,
             USUARIO_REL                    CHAR(8) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MOVIMIENTOS                        *
      ******************************************************************
       01  DCLMOVIMIENTOS.
      *                       ID_MOV
           10 HV-ID-MOV            PIC S9(9) USAGE COMP.
      *                       USUARIO
           10 HV-USUARIO-MOV       PIC X(8).
      *                       TIPO_OPER
           10 HV-TIPO-OPER         PIC X(1).
      *                       MONTO
           10 HV-MONTO             PIC S9(8)V9(2) USAGE COMP-3.
      *                       FECHA
           10 HV-FECHA             PIC X(26).
      *                       USUARIO_REL
           10 HV-USUARIO-REL       PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
