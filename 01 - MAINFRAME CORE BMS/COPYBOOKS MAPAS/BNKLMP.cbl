       01  BNKMAPLI.
           02  FILLER PIC X(12).
           02  USERFL    COMP  PIC  S9(4).
           02  USERFF    PICTURE X.
           02  FILLER REDEFINES USERFF.
             03 USERFA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  USERFI  PIC X(8).
           02  PASSFL    COMP  PIC  S9(4).
           02  PASSFF    PICTURE X.
           02  FILLER REDEFINES PASSFF.
             03 PASSFA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PASSFI  PIC X(8).
           02  MSGFL    COMP  PIC  S9(4).
           02  MSGFF    PICTURE X.
           02  FILLER REDEFINES MSGFF.
             03 MSGFA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSGFI  PIC X(70).
       01  BNKMAPLO REDEFINES BNKMAPLI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  USERFC    PICTURE X.
           02  USERFH    PICTURE X.
           02  USERFO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  PASSFC    PICTURE X.
           02  PASSFH    PICTURE X.
           02  PASSFO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  MSGFC    PICTURE X.
           02  MSGFH    PICTURE X.
           02  MSGFO  PIC X(70).
