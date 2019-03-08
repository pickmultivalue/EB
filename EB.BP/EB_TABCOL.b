    SUBROUTINE EB_TABCOL(CRTLN,DCOL,TCOL,CHGLCOL)
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    INCLUDE EB.EQUS EB.EQUS
!
    EQU TABCH TO CHAR(9)
    Indent=ITAB<ITABPOS>
    Nbr.Tabs=COUNT(CRTLN[1,TCOL],TABCH)
    IF CHGLCOL THEN
        IF TAB.MODE THEN
            TMPCOL=0
            TDCOL=DCOL+OFFSET-4
            TCOL=0
            LOOP
                TCOL+=1
            UNTIL TMPCOL>=TDCOL DO
                C=CRTLN[TCOL,1]
                IF C=TABCH THEN
                    TMPCOL=(((TMPCOL)/Indent+.5) "0")*Indent
                END ELSE TMPCOL+=1
            REPEAT
            TCOL-=1
        END ELSE
            TCOL=DCOL-4+OFFSET
        END
    END ELSE
        GOSUB SETCOL
    END
    RETURN
!
SETCOL:
    IF TAB.MODE THEN
        NCOL = 0
        TC = TCOL-(CRTLN[TCOL,1] # TABCH)
        FOR C = 1 TO TC
            IF CRTLN[C,1] EQ TABCH THEN
                R = MOD(NCOL,Indent)
                NCOL += Indent -R
            END ELSE NCOL++
        NEXT C
        NCOL++
!        NEWLN=CRTLN[1,TCOL]
!        CALL EB_TABS(NEWLN,PWIDTH)
!        NCOL=LEN(NEWLN)
    END ELSE NCOL=TCOL
    DCOL=NCOL-OFFSET+4
    IF DCOL<0 THEN DCOL+=OFFSET
    RETURN
