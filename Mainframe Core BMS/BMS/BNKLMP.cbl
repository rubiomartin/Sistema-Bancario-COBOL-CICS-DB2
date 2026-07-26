BNKLMP   DFHMSD TYPE=MAP,                                              X
               MODE=INOUT,                                             X
               TERM=ALL,                                               X
               LANG=COBOL,                                             X
               STORAGE=AUTO,                                           X
               TIOAPFX=YES,                                            X
               MAPATTS=(COLOR,HILIGHT),                                X
               DSATTS=(COLOR,HILIGHT),                                 X
               CTRL=(FREEKB,FRSET)
BNKMAPL  DFHMDI SIZE=(24,80),                                          X
               LINE=1,                                                 X
               COLUMN=1,                                               X
               COLOR=BLUE,                                             X
               MAPATTS=(COLOR,HILIGHT),                                X
               DSATTS=(COLOR,HILIGHT)
         DFHMDF POS=(01,30),                                           X
               LENGTH=20,                                              X
               ATTRB=(ASKIP,BRT),                                      X
               INITIAL='SISTEMA DE LOGIN'
         DFHMDF POS=(03,18),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=BLUE,INITIAL=' BBBBB '
         DFHMDF POS=(03,27),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=PINK,INITIAL='  AAA  '
         DFHMDF POS=(03,36),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=YELLOW,INITIAL='NN   NN'
         DFHMDF POS=(03,45),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=TURQUOISE,INITIAL=' CCCCC '
         DFHMDF POS=(03,54),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=GREEN,INITIAL=' OOOOO '
         DFHMDF POS=(04,18),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=BLUE,INITIAL='BB   BB'
         DFHMDF POS=(04,27),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=PINK,INITIAL='AA   AA'
         DFHMDF POS=(04,36),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=YELLOW,INITIAL='NNN  NN'
         DFHMDF POS=(04,45),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=TURQUOISE,INITIAL='CC     '
         DFHMDF POS=(04,54),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=GREEN,INITIAL='OO   OO'
         DFHMDF POS=(05,18),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=BLUE,INITIAL='BBBBBB '
         DFHMDF POS=(05,27),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=PINK,INITIAL='AA   AA'
         DFHMDF POS=(05,36),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=YELLOW,INITIAL='NN N NN'
         DFHMDF POS=(05,45),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=TURQUOISE,INITIAL='CC     '
         DFHMDF POS=(05,54),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=GREEN,INITIAL='OO   OO'
         DFHMDF POS=(06,18),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=BLUE,INITIAL='BB   BB'
         DFHMDF POS=(06,27),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=PINK,INITIAL='AAAAAAA'
         DFHMDF POS=(06,36),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=YELLOW,INITIAL='NN  NNN'
         DFHMDF POS=(06,45),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=TURQUOISE,INITIAL='CC     '
         DFHMDF POS=(06,54),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=GREEN,INITIAL='OO   OO'
         DFHMDF POS=(07,18),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=BLUE,INITIAL='BB   BB'
         DFHMDF POS=(07,27),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=PINK,INITIAL='AA   AA'
         DFHMDF POS=(07,36),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=YELLOW,INITIAL='NN   NN'
         DFHMDF POS=(07,45),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=TURQUOISE,INITIAL='CC     '
         DFHMDF POS=(07,54),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=GREEN,INITIAL='OO   OO'
         DFHMDF POS=(08,18),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=BLUE,INITIAL=' BBBBB '
         DFHMDF POS=(08,27),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=PINK,INITIAL='AA   AA'
         DFHMDF POS=(08,36),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=YELLOW,INITIAL='NN   NN'
         DFHMDF POS=(08,45),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=TURQUOISE,INITIAL=' CCCCC '
         DFHMDF POS=(08,54),LENGTH=07,ATTRB=(ASKIP,NORM),              X
               COLOR=GREEN,INITIAL=' OOOOO '
         DFHMDF POS=(11,20),                                           X
               LENGTH=08,                                              X
               ATTRB=(ASKIP),                                          X
               INITIAL='USUARIO:'
USERF    DFHMDF POS=(11,29),                                           X
               LENGTH=08,                                              X
               ATTRB=(UNPROT,IC)
         DFHMDF POS=(13,20),                                           X
               LENGTH=09,                                              X
               ATTRB=(ASKIP),                                          X
               INITIAL='PASSWORD:'
PASSF    DFHMDF POS=(13,30),                                           X
               LENGTH=08,                                              X
               ATTRB=(UNPROT,DRK)
MSGF     DFHMDF POS=(23,02),                                           X
               LENGTH=70,                                              X
               ATTRB=(ASKIP,BRT)
         DFHMDF POS=(24,20),                                           X
               LENGTH=29,                                              X
               ATTRB=(ASKIP),                                          X
               INITIAL='PRESIONE ENTER PARA CONTINUAR'
         DFHMSD TYPE=FINAL
         END
