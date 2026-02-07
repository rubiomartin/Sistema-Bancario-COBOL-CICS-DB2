       01  BNKMAPTI.
           02  FILLER PIC X(12).
           02  TITULOL    COMP  PIC  S9(4).
           02  TITULOF    PICTURE X.
           02  FILLER REDEFINES TITULOF.
             03 TITULOA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TITULOI  PIC X(35).
           02  LBLUSERL    COMP  PIC  S9(4).
           02  LBLUSERF    PICTURE X.
           02  FILLER REDEFINES LBLUSERF.
             03 LBLUSERA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LBLUSERI  PIC X(18).
           02  NOMBREUSL    COMP  PIC  S9(4).
           02  NOMBREUSF    PICTURE X.
           02  FILLER REDEFINES NOMBREUSF.
             03 NOMBREUSA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NOMBREUSI  PIC X(10).
           02  LBLSALDOL    COMP  PIC  S9(4).
           02  LBLSALDOF    PICTURE X.
           02  FILLER REDEFINES LBLSALDOF.
             03 LBLSALDOA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LBLSALDOI  PIC X(15).
           02  SALDOL    COMP  PIC  S9(4).
           02  SALDOF    PICTURE X.
           02  FILLER REDEFINES SALDOF.
             03 SALDOA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SALDOI  PIC X(15).
           02  LINEA2L    COMP  PIC  S9(4).
           02  LINEA2F    PICTURE X.
           02  FILLER REDEFINES LINEA2F.
             03 LINEA2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LINEA2I  PIC X(80).
           02  LBLDESTL    COMP  PIC  S9(4).
           02  LBLDESTF    PICTURE X.
           02  FILLER REDEFINES LBLDESTF.
             03 LBLDESTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LBLDESTI  PIC X(20).
           02  USRDESTL    COMP  PIC  S9(4).
           02  USRDESTF    PICTURE X.
           02  FILLER REDEFINES USRDESTF.
             03 USRDESTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  USRDESTI  PIC X(8).
           02  LBLMONTOL    COMP  PIC  S9(4).
           02  LBLMONTOF    PICTURE X.
           02  FILLER REDEFINES LBLMONTOF.
             03 LBLMONTOA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LBLMONTOI  PIC X(20).
           02  MONTOL    COMP  PIC  S9(4).
           02  MONTOF    PICTURE X.
           02  FILLER REDEFINES MONTOF.
             03 MONTOA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MONTOI  PIC 999999999999.
           02  CONFRML    COMP  PIC  S9(4).
           02  CONFRMF    PICTURE X.
           02  FILLER REDEFINES CONFRMF.
             03 CONFRMA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CONFRMI  PIC X(70).
           02  MSGL    COMP  PIC  S9(4).
           02  MSGF    PICTURE X.
           02  FILLER REDEFINES MSGF.
             03 MSGA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSGI  PIC X(60).
           02  LINEA3L    COMP  PIC  S9(4).
           02  LINEA3F    PICTURE X.
           02  FILLER REDEFINES LINEA3F.
             03 LINEA3A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LINEA3I  PIC X(80).
           02  LEYENDAL    COMP  PIC  S9(4).
           02  LEYENDAF    PICTURE X.
           02  FILLER REDEFINES LEYENDAF.
             03 LEYENDAA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LEYENDAI  PIC X(60).
       01  BNKMAPTO REDEFINES BNKMAPTI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  TITULOC    PICTURE X.
           02  TITULOH    PICTURE X.
           02  TITULOO  PIC X(35).
           02  FILLER PICTURE X(3).
           02  LBLUSERC    PICTURE X.
           02  LBLUSERH    PICTURE X.
           02  LBLUSERO  PIC X(18).
           02  FILLER PICTURE X(3).
           02  NOMBREUSC    PICTURE X.
           02  NOMBREUSH    PICTURE X.
           02  NOMBREUSO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  LBLSALDOC    PICTURE X.
           02  LBLSALDOH    PICTURE X.
           02  LBLSALDOO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  SALDOC    PICTURE X.
           02  SALDOH    PICTURE X.
           02  SALDOO PIC $$$.$$$.$$9,99.
           02  FILLER PICTURE X(3).
           02  LINEA2C    PICTURE X.
           02  LINEA2H    PICTURE X.
           02  LINEA2O  PIC X(80).
           02  FILLER PICTURE X(3).
           02  LBLDESTC    PICTURE X.
           02  LBLDESTH    PICTURE X.
           02  LBLDESTO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  USRDESTC    PICTURE X.
           02  USRDESTH    PICTURE X.
           02  USRDESTO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  LBLMONTOC    PICTURE X.
           02  LBLMONTOH    PICTURE X.
           02  LBLMONTOO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  MONTOC    PICTURE X.
           02  MONTOH    PICTURE X.
           02  MONTOO  PIC X(12).
           02  FILLER PICTURE X(3).
           02  CONFRMC    PICTURE X.
           02  CONFRMH    PICTURE X.
           02  CONFRMO  PIC X(70).
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(60).
           02  FILLER PICTURE X(3).
           02  LINEA3C    PICTURE X.
           02  LINEA3H    PICTURE X.
           02  LINEA3O  PIC X(80).
           02  FILLER PICTURE X(3).
           02  LEYENDAC    PICTURE X.
           02  LEYENDAH    PICTURE X.
           02  LEYENDAO  PIC X(60).
