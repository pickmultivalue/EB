    SUBROUTINE EB_INPUT_ZERO(CHR,COL,ROW,CODES,SUBCODES,MAX.SUB)
* @(#) EB_INPUT_ZERO.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.INPUT.ZERO Ported to jBASE 16:15:15  27 JUL 2001
    INCLUDE EB.EQUS EB.COMMONS
    GO MAIN$
!
!=============================================
! GALA 4GL
!
! Copyright (C) GENERAL AUTOMATION AUSTRALASIA Pty. Ltd.
!
! Written by Peter Falson - February 1992
!
!=========== Program's Purpose ===============
!
! No echo input prompt to return a value or
! a FG$ACT.CODE.
!
!=============================================
!
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS CRT.PARAMS
    INCLUDE EB.EQUS EB.CHARS
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.OS.INCLUDES TODAYS.DATE
!INCLUDE jutil.h
    MAIN$:!
    DEFC INT JBASEEmulateGETINT(INT, INT)
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    vm_start=IF_COMPILED_PRIME
!
!===================================================
    IF FG$TIMEDOUT THEN CHR=''; FG$ACT.CODE=FG$ABT.CODE; RETURN
!===================================================
!
    EQU RTN.VAL TO 13
    Timeout = 300
!
    SUB.CODES=SUBCODES
    NO.ECHO=SUB.CODES<4>
    DFLT.CHR=SUB.CODES<3>
    MATCH.SET=SUB.CODES<2>; SUB.CODES=SUB.CODES<1>
!
    IF SUB.CODES MATCHES "1X'-'1X" THEN
        SUB.CODE1=SEQ(FIELD(SUB.CODES,'-',1))
        SUB.CODE2=SEQ(SUB.CODES[COL2()+1,1])
        MAX.SUB=0
        SUB.JUST='R#1'
    END ELSE
        SUB.CODE1=-1; SUB.CODE2=-1
        IF NOT(MAX.SUB) THEN
            NBR.CODES=DCOUNT(SUB.CODES,VM)
            FOR S=1 TO NBR.CODES
                L=LEN(SUB.CODES<1,S>)
                IF L>MAX.SUB THEN MAX.SUB=L
            NEXT S
        END
        ALPHAS=SWAP(SUB.CODES,VM,'')
        ALPHAS=OCONV(ALPHAS,'MCA')
        CONV=(ALPHAS=OCONV(ALPHAS,'MCU'))
        SUB.JUST='R#':MAX.SUB
    END
    MAX.JUST='R#':FG$MAX.CHARS
    IF FG$MAX.CHARS>=MAX.SUB THEN MAX.LEN=FG$MAX.CHARS ELSE MAX.LEN=MAX.SUB
!
    IF FG$ACT.CODE THEN FG$LAST.ACT.CODE=FG$ACT.CODE; FG$ACT.CODE=FALSE
    SAVE.CODES=FG$INPUT.CODES
    FG$INPUT.CODES=CODES
    IF (COL:ROW)#'' THEN CRT @(COL,ROW):DFLT.CHR:
!INCLUDE EB.INCLUDES GET.CHR
    INCLUDE EB.INCLUDES INPUT.ZERO
    IF CHR.NBR=13 THEN CHR=DFLT.CHR
    ECHO ON
    FG$INPUT.CODES=SAVE.CODES
    RETURN
END
