    SUBROUTINE EB_INTEGRATE
    INCLUDE JBC.h
    INCLUDE EB.EQUS EB.COMMON
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS SCREEN.PARAMS
    DIM merge(3)
    EQU m.X TO merge(1)
    EQU m.Y TO merge(2)
    EQU m.Z TO merge(3)
    DIM m.Hs(2)
    m.Hs(1) = '<<<<<<< HEAD'
    m.Hs(2) = '<<<<<<< Updated upstream'
    FG_TIMEOUT = 0
    FOR m = 1 TO 2
        m.H = m.Hs(m)
        IF INDEX(REC, m.H, 1) THEN GO RETRY
    NEXT m
    m = 4
    GO MISSING
RETRY:
    m.X='<<<<<<<'
    m.Y='======='
    m.Z='>>>>>>>'
    I=INDROW+ROW
    CHECK.LINE=REC<I>
    IF CHECK.LINE NE m.H THEN GOSUB FINDHEAD
    YNC=COL; YNR=ROW
    FOR m = 1 TO 3
        IF CHECK.LINE[1, LEN(merge(m))] EQ merge(m) THEN BREAK
    NEXT m
MISSING:
    IF m GT 3 THEN
        CRT MSG.CLR:'No instance of ':m.H:' found':
        RQM
        FG_ACT.CODE = FALSE
        RETURN
    END
    cmds = "<O>riginal,<Y>ours,<T>heirs,<C>ompare ?"
    IDC1 = ITNM:'.COMP1'
    AREC = ''
    C = 1
    S = I+1
    LOOP
        LINE = REC<S>
        S++
    UNTIL LINE EQ m.Y DO
        AREC<C> = LINE
        C++
    REPEAT
    IDC2 = ITNM:'.COMP2'
    BREC = ''
    C = 1
    LOOP
        LINE = REC<S>
        S++
    UNTIL LINE[1,LEN(m.Z)] EQ m.Z DO
        BREC<C> = LINE
        C++
    REPEAT
    IF AREC EQ BREC THEN
        Y = 'Y'
    END ELSE
REPROMPT:
        CRT MSG.CLR:CHANGE(CHANGE(cmds,'<',RVON),'>',RVOFF)
        YNCHRS='.':VM:'C':VM:'O':VM:'Y':VM:'T'; YNL=1; GOSUB GET.CHAR
        CRT MSG.DSP:
        IF FG_ACT.CODE THEN RETURN
        Y = UPCASE(Y)
    END
    IF Y EQ 'C' THEN
        WRITE AREC ON FIL,IDC1
        WRITE BREC ON FIL,IDC2
        ID1 = FLNM:DIR_DELIM_CH:IDC1
        ID2 = FLNM:DIR_DELIM_CH:IDC2
        EXECUTE 'COMPARE_ITEM ':ID1:' ':ID2
        DELETE FIL,IDC1
        DELETE FIL,IDC2
        SCR.UD=TRUE
        CALL EB_REFRESH
        GO REPROMPT
    END
    CRT MSG.AKN:
    FTYP=INDEX('OYT',Y,1)
! Find the ORIG line if we're not there
    LOOP
        IF CHECK.LINE[1, LEN(m.X)] EQ m.X THEN BREAK
    UNTIL I=1 DO
        I-=1
        CHECK.LINE=REC<I>
    REPEAT
    Y=I
    ROW=I-INDROW; SCRL=ROW
! We found ORIG so now delete up to the section we want
    STMP = m.Y
    m.Y = ''
    LOOP
        DEL REC<I>
        CHECK.LINE=REC<I>
        IF CHECK.LINE[1, LEN(STMP)] EQ STMP THEN BREAK
        m.Y<-1> = CHECK.LINE
    REPEAT
! Now found the end of this bit
    STMP = m.Z
    m.Z = ''
    LOOP
        DEL REC<I>
        CHECK.LINE=REC<I>
        IF CHECK.LINE[1, LEN(STMP)] EQ STMP THEN BREAK
        m.Z<-1> = CHECK.LINE
    REPEAT
    REC<I> = merge(FTYP)
    CHANGED=TRUE; SCR.UD=TRUE
    CALL EB_REFRESH
    GO RETRY
GET.CHAR: !
    CALL EB_UT_INPUT_ZERO(Y,MAT EB_CHARS,FG_ACT.CODE,YNC,YNR,FG_INPUT.CODES,YNCHRS,YNL,FG_TIMEOUT:AM:FG_MONITOR.SECS)
    ECHO ON
    RETURN
FINDHEAD: !
    FG_ACT.CODE=FG_MULTI.CODE
    PSSTR = m.H
    CALL EB_SEARCH
    I=INDROW+ROW
    CHECK.LINE=REC<I>
    IF CHECK.LINE NE m.H THEN I--
    CHECK.LINE=REC<I>
    IF CHECK.LINE EQ m.H THEN
        INDROW = I+1
        ROW = 1
        PSSTR = m.Z
        CALL EB_SEARCH
        ROW = 2
        INDROW = I-2
        CHANGED=TRUE; SCR.UD=TRUE
        CALL EB_REFRESH
    END
    RETURN
