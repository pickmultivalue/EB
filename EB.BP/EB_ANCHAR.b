    SUBROUTINE EB_ANCHAR(I,ST,FI,TYPE,STMP)
* @(#) EB_ANCHAR.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.ANCHAR Ported to jBASE 16:15:13  27 JUL 2001
    EQU PC TO 'P(1N);(1A)'
    INCR=1-2*(ST>FI)
    IF TYPE THEN
        FOR I=ST TO FI STEP INCR UNTIL NOT(ICONV(STMP[I,1],PC)#''); NEXT I
    END ELSE
        FOR I=ST TO FI STEP INCR UNTIL NOT(ICONV(STMP[I,1],PC)#''); NEXT I
    END
    RETURN
END
