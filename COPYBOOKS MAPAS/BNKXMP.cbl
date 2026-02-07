       01  BNKMAPXI.
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
           02  TXTDEPL    COMP  PIC  S9(4).
           02  TXTDEPF    PICTURE X.
           02  FILLER REDEFINES TXTDEPF.
             03 TXTDEPA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TXTDEPI  PIC X(40).
           02  TXTRETL    COMP  PIC  S9(4).
           02  TXTRETF    PICTURE X.
           02  FILLER REDEFINES TXTRETF.
             03 TXTRETA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TXTRETI  PIC X(40).
           02  LBLOPERL    COMP  PIC  S9(4).
           02  LBLOPERF    PICTURE X.
           02  FILLER REDEFINES LBLOPERF.
             03 LBLOPERA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LBLOPERI  PIC X(20).
           02  TIPOOPERL    COMP  PIC  S9(4).
           02  TIPOOPERF    PICTURE X.
           02  FILLER REDEFINES TIPOOPERF.
             03 TIPOOPERA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TIPOOPERI  PIC X(1).
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
       01  BNKMAPXO REDEFINES BNKMAPXI.
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
           02  TXTDEPC    PICTURE X.
           02  TXTDEPH    PICTURE X.
           02  TXTDEPO  PIC X(40).
           02  FILLER PICTURE X(3).
           02  TXTRETC    PICTURE X.
           02  TXTRETH    PICTURE X.
           02  TXTRETO  PIC X(40).
           02  FILLER PICTURE X(3).
           02  LBLOPERC    PICTURE X.
           02  LBLOPERH    PICTURE X.
           02  LBLOPERO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  TIPOOPERC    PICTURE X.
           02  TIPOOPERH    PICTURE X.
           02  TIPOOPERO  PIC X(1).
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
