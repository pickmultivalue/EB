    SUBROUTINE EB_INTEGRATE
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(100),EB.FILE.LIST
    COM RDSP(100),CHANGES(100)
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS SCREEN.PARAMS
    DIM merge(3)
    EQU m.X TO merge(1)
    EQU m.Y TO merge(2)
    EQU m.Z TO merge(3)
    m.X='<<<<<<<'
    m.Y='======='
    m.Z='>>>>>>>'
    STMP=m.Z
    I=INDROW+ROW
    CHECK.LINE=REC<I>
    FOR m = 1 TO 3
        IF CHECK.LINE[1, LEN(merge(m))] EQ merge(m) THEN BREAK
    NEXT m
    IF m GT 3 THEN
        CRT MSG.CLR:'Position cursor on a merge line (e.g. <<<<)':
        GOSUB GET.CHAR
        RETURN
    END
    CRT MSG.CLR:"<O>rigonal,<Y>ours,<T>heirs,<C>ompare ?":
    YNC=COL; YNR=ROW; YNCHRS='.':VM:'C':VM:'O':VM:'Y':VM:'T'; YNL=1; GOSUB GET.CHAR
    CRT MSG.DSP:
    IF FG$ACT.CODE THEN RETURN
    CRT MSG.AKN:
    FTYP=INDEX('OYT',OCONV(Y,"MCU"),1)
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
    CHANGED=FALSE; SCR.UD=TRUE
    RETURN
GET.CHAR: !
    CALL EB_UT_INPUT_ZERO(Y,MAT EB$CHARS,FG$ACT.CODE,YNC,YNR,FG$INPUT.CODES,YNCHRS,YNL,FG$TIMEOUT:AM:FG$MONITOR.SECS)
    RETURN
