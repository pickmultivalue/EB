    SUBROUTINE EB_ADD(START,LCOL,LLEN,INS.MODE,NEW.CHARS,CURR.LINE)
* @(#) EB_ADD.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.ADD Ported to jBASE 16:15:13  27 JUL 2001
    EQU MAX TO 9999
    IF NOT(START) THEN START=LCOL
    LLEN=LEN(CURR.LINE)
    IF LLEN<START THEN
        CURR.LINE:=SPACE(START-LLEN)
    END
    IF INS.MODE THEN LLEN=0 ELSE LLEN=LEN(NEW.CHARS)
    CURR.LINE=(CURR.LINE[1,START-1]:NEW.CHARS:CURR.LINE[START+LLEN,MAX])
    START=0; NEW.CHARS=""
    RETURN
END
