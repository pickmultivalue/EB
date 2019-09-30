* @(#) CLEAN.SEC.I Ported to jBASE 15:13:19  17 APR 2000
    READV NAME FROM FG_SECURITY,PORT,6 ELSE NAME=''
    NAME=FIELD(NAME,' ',1)
    IF NAME#FG_TUSER THEN     ;! use this one
        DEL EB.SECURITY.REC<1,I>
        CRT PORT:' removed from ':FG_TUSER
    END
