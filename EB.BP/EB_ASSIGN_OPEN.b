    SUBROUTINE EB_ASSIGN_OPEN(FILE.NAME,POS,FILE.VAR,MUST.EXIST)
* @(#) EB_ASSIGN_OPEN.b Ported to jBASE 07:23:52  18 FEB 2010
!
    INCLUDE EB.EQUS EB.COMMONS
    INCLUDE EB.OS.INCLUDES OS.ERRORS
!
    CALL EB_OPEN_FILE(FILE.NAME,POS)
    IF POS THEN
        FILE.VAR=OPENED.FILES(POS)
    END ELSE
        IF MUST.EXIST THEN STOP OPER,FILE.NAME
    END
    RETURN
END
