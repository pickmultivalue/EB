    SUBROUTINE EB_GETMOUSE(FG$TYPEAHEAD.BUFF,EVENT,C,R)
* @(#) EB_GETMOUSE.b Ported to jBASE 07:23:52  18 FEB 2010
    ECHO OFF
    EVENT=FG$TYPEAHEAD.BUFF
    cleft=3-LEN(EVENT)
    FG$TYPEAHEAD.BUFF=''
    nc=SYSTEM(14)
    IF nc THEN
        IF nc>cleft THEN nc=cleft
        INPUT E,nc:
        EVENT:=E
        cleft-=nc
        IF cleft THEN
            nc=SYSTEM(14)
            IF nc THEN
                IF nc>cleft THEN nc=cleft
                INPUT E,nc:
                EVENT:=E
                cleft-=nc
                IF cleft THEN
                    nc=SYSTEM(14)
                    INPUT E,nc:
                    EVENT:=E
                END
            END
        END
    END
    C=EVENT[2,1]
    R=EVENT[3,1]
    EVENT=EVENT[1,1]
    IF R#'' THEN
        C=SEQ(C)-33
        R=SEQ(R)-33
    END
    ECHO ON
    RETURN
