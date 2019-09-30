    SUBROUTINE EB_GETMOUSE(FG_TYPEAHEAD.BUFF,EVENT,C,R)
!    ECHO OFF
    EVENT=FG_TYPEAHEAD.BUFF
    BTN=EVENT[1,1]
    EVENT=FIELD(EVENT,'[',2)
    R = FIELD(EVENT,';',1)-1
    C = FIELD(EVENT[COL2()+1,9],'R',1)-1
    FG_TYPEAHEAD.BUFF=''
    EVENT=(' "')[BTN,1]
    RETURN
    cleft=3-LEN(EVENT)
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
