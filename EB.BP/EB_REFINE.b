    SUBROUTINE EB_REFINE(in_filter_string, INP.LENGTH, DISPLAY.LIST,HILINE,DIMMED,NBR.ATTS,ATTRS,KEY.LIST,NBR.KEYS, auto_complete)
    INCLUDE EB.EQUS EB.COMMON
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS SCREEN.PARAMS
    MAIN$:!
!
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
    type<3> = LEN(in_filter_string) + 1
    CALL EB_UT_WP(in_filter_string,type,INP.LENGTH,1,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',abort_string)
    IF auto_complete AND FG_TIMEDOUT THEN FG_ACT.CODE = 0
    FG_INPUT.CODES=STMP
    IF FG_ACT.CODE THEN
!        IF FG_ACT.CODE=1 THEN ;! don't abort
!            FG_ACT.CODE=''
!        END
        GOTO FINISH
    END
    IF in_filter_string='' THEN RETURN
    MV=1
    filter_string = in_filter_string
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
FINISH:   !
!    CRT @(0,22):CLEOL:
!    IF FG_VALID THEN CRT @(0,23):CLEOL:
    RETURN
END
