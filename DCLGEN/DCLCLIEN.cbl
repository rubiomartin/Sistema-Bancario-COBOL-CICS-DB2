      ******************************************************************
      * DCLGEN TABLE(IBMUSER.CLIENTES)                                 *
      *        LIBRARY(BANKPRJ.LIB.DCLGEN(DCLCLIEN))                   *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(HV-)                                              *
      *        STRUCTURE(DCLCLIENTES)                                  *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE IBMUSER.CLIENTES TABLE
           ( USUARIO                        CHAR(8) NOT NULL,
             PASSWORD                       CHAR(8) NOT NULL,
             NOMBRE                         CHAR(20) NOT NULL,
             SALDO                          DECIMAL(10, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE IBMUSER.CLIENTES                   *
      ******************************************************************
       01  DCLCLIEN.
      *                       USUARIO
           10 HV-USUARIO           PIC X(8).
      *                       PASSWORD
           10 HV-PASSWORD          PIC X(8).
      *                       NOMBRE
           10 HV-NOMBRE            PIC X(20).
      *                       SALDO
           10 HV-SALDO             PIC S9(8)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
