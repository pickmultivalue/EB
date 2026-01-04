    SUBROUTINE EB_REFINE(cache, INP.LENGTH, DISPLAY.LIST,HILINE,DIMMED,NBR.ATTS,ATTRS,KEY.LIST,NBR.KEYS, auto_complete)
    $option jabba
    INCLUDE EB.EQUS EB.COMMON
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS SCREEN.PARAMS
    MAIN$:!
!
    last_filter = cache->last_filter
    MSG=FG_ERROR.MSGS<123>
    ICOL=LEN(MSG)+1; IROW=(PDEPTH-1)
    STMP=FG_INPUT.CODES
    IF auto_complete THEN
        abort_string = ''
    END ELSE
        abort_string = ESC
        FG_INPUT.CODES=FG_REPLACE.CODES
        CRT MSG.CLR:MSG:SPC:
    END
    type = 'LIT'
    type<3> = LEN(last_filter) + 1
    CALL EB_UT_WP(last_filter,type,INP.LENGTH,1,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',abort_string)
    IF auto_complete AND FG_TIMEDOUT THEN FG_ACT.CODE = 0
    FG_INPUT.CODES=STMP
    IF FG_ACT.CODE THEN
        GOTO FINISH
    END
    cache->last_filter = last_filter
    IF last_filter='' THEN RETURN
    IF cache->$hasproperty(last_filter) then
        obj = cache->@last_filter
        DISPLAY.LIST = obj->DISPLAY.LIST
        KEY.LIST = obj->KEY.LIST
        HILINE = obj->HILINE
        DIMMED = obj->DIMMED
        RETURN
    END ELSE
        obj = new object()
    END
    MV=1
    filter_string = last_filter
    IF filter_string[1,1]='~' THEN filter_string=filter_string[2,99]; Reverse=1 ELSE Reverse=0
    LOOP
        LINE=''
        FOR J=1 TO NBR.ATTS
            ATTR=ATTRS<1,J>
            IF INDEX(ATTR,CTRL.F,1) THEN
                fn=FIELD(ATTR,CTRL.F,1)
                IF fn THEN
                    ATTR=ATTR[COL2()+1,99]
                    IF ATTR MATCHES "1N0N" THEN
                        READV STMP FROM OPENED.FILES(fn),DISPLAY.LIST<J,MV>,ATTR ELSE STMP=''
                    END ELSE STMP=''
                END ELSE STMP=''
            END ELSE STMP=DISPLAY.LIST<J,MV>
            LINE:=STMP:' '
        NEXT J
    UNTIL TRIM(LINE)='' DO
        POS=(INDEX(LINE,filter_string,1) NE 0)
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
    obj->DISPLAY.LIST = DISPLAY.LIST
    obj->KEY.LIST = KEY.LIST
    obj->HILINE = HILINE
    obj->DIMMED = DIMMED
    cache->@last_filter = obj
FINISH:   !
!    CRT @(0,22):CLEOL:
!    IF FG_VALID THEN CRT @(0,23):CLEOL:
    RETURN
END
