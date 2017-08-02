    SUBROUTINE EB_REFINE(DISPLAY.LIST,HILINE,DIMMED,NBR.ATTS,ATTRS,KEY.LIST,NBR.KEYS)
* @(#) EB_REFINE.b Ported to jBASE 07:23:52  18 FEB 2010
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(100),EB.FILE.LIST
    COM RDSP(100), CHANGES(100)
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS SCREEN.PARAMS
    MAIN$:!
!
    MSG=FG$ERROR.MSGS<123>
    ICOL=LEN(MSG)+1; IROW=(PDEPTH-1)
    CRT MSG.CLR:MSG:SPC:
    STMP=FG$INPUT.CODES
    FG$INPUT.CODES=FG$REPLACE.CODES
    SEARCH.LIST=''
    CALL EB_UT_WP(SEARCH.LIST,'LIT',50,1,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',ESC)
    FG$INPUT.CODES=STMP
    IF FG$ACT.CODE THEN
        IF FG$ACT.CODE=1 THEN ;! don't abort
            FG$ACT.CODE=''
        END
        GOTO FINISH
    END
    IF SEARCH.LIST='' THEN RETURN
    MV=1
    IF SEARCH.LIST[1,1]='~' THEN SEARCH.LIST=SEARCH.LIST[2,99]; Reverse=1 ELSE Reverse=0
    LOOP
        LINE=''
        FOR J=1 TO NBR.ATTS
            ATTR=ATTRS<1,J>
            IF INDEX(ATTR,CTRL.F,1) THEN
                FILE=FIELD(ATTR,CTRL.F,1)
                IF FILE THEN
                    ATTR=ATTR[COL2()+1,99]
                    IF ATTR MATCHES "1N0N" THEN
                        READV STMP FROM OPENED.FILES(FILE),DISPLAY.LIST<J,MV>,ATTR ELSE STMP=''
                    END ELSE STMP=''
                END ELSE STMP=''
            END ELSE STMP=DISPLAY.LIST<J,MV>
            LINE:=STMP:' '
        NEXT J
    UNTIL TRIM(LINE)='' DO
        POS=(INDEX(LINE,SEARCH.LIST,1)#0)
        IF POS-Reverse THEN MV+=1 ELSE
            FOR J=1 TO NBR.ATTS
                DEL DISPLAY.LIST<J,MV>
            NEXT J
            FOR J=1 TO NBR.KEYS
                DEL KEY.LIST<J,MV>
            NEXT J
            DEL HILINE<1,MV>
            DEL DIMMED<1,MV>
        END
    REPEAT
FINISH:   !
    CRT @(0,22):CLEOL:
    IF FG$VALID THEN CRT @(0,23):CLEOL:
    RETURN
END