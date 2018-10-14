    SUBROUTINE EB_INTEGRATE
    INCLUDE JBC.h
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
    YNC=COL; YNR=ROW
    FOR m = 1 TO 3
        IF CHECK.LINE[1, LEN(merge(m))] EQ merge(m) THEN BREAK
    NEXT m
    IF m GT 3 THEN
        CRT MSG.CLR:'Position cursor on a merge line (e.g. <<<<)':
        YNCHRS='';YNL=0
        GOSUB GET.CHAR
        RETURN
    END
    cmds = "<O>rigonal,<Y>ours,<T>heirs,<C>ompare ?"
    CRT MSG.CLR:CHANGE(CHANGE(cmds,'<',RVON),'>',RVOFF)
    YNCHRS='.':VM:'C':VM:'O':VM:'Y':VM:'T'; YNL=1; GOSUB GET.CHAR
    CRT MSG.DSP:
    IF FG$ACT.CODE THEN RETURN
    Y = UPCASE(Y)
    IF Y EQ 'C' THEN
!        CALL EB_COMPARE(MAT RDSP,FIL,REC,CHANGED,MREC,POS,READ.AGAIN,LCOL,LROW,ROW,INDROW,PR,MSG.CLR,MSG.AKN,FLNM,MFLNM,ITNM,MITNM,DCT,MDCT)
!        SCR.UD=TRUE ;!SCRL=0
!        MREC=''
!        CRT MSG.DSP:
!        GO STRT
        IDC = ITNM:'.COMP1'
        WRITE REC ON FIL,IDC
        ID = FLNM:DIR_DELIM_CH:IDC
        EXECUTE 'COMPARE_ITEM ':ID:' ':ID
        READ MREC FROM FIL,IDC ELSE NULL
        DELETE FIL,IDC
        CHANGED=(MREC NE REC); SCR.UD=TRUE
        REC = MREC
        RETURN
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
    CHANGED=FALSE; SCR.UD=TRUE
    RETURN
GET.CHAR: !
    CALL EB_UT_INPUT_ZERO(Y,MAT EB$CHARS,FG$ACT.CODE,YNC,YNR,FG$INPUT.CODES,YNCHRS,YNL,FG$TIMEOUT:AM:FG$MONITOR.SECS)
    ECHO ON
    RETURN
