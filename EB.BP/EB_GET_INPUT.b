    SUBROUTINE EB_GET_INPUT(CHR, CHR.NBR)
    INCLUDE EB.EQUS EB.COMMONS
    GO MAIN$
!=========== Program's Purpose ===============
!
! No echo input prompt to return a value or
! a FG_ACT.CODE.
!
!=============================================
!
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS EB.CHARS
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS STD.EQUS
MAIN$:!
    DEFC INT JBASEEmulateGETINT(INT)
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30)
    am_start=IF_COMPILED_PRIME
    vm_start=IF_COMPILED_PRIME
!
!===================================================
    IF FG_TIMEDOUT THEN CHR=''; FG_ACT.CODE=FG_ABT.CODE; RETURN
!===================================================
!
    EQU RTN.VAL TO 13
    time_out = 300
!
    ECHO OFF
    MAX.LEN=FG_MAX.CHARS
    PREV.CHARS=''
    FG_ACT.CODE=FALSE
    SYSTEM.14 = 0
!
! If FG_TYPEAHEAD was not null, process any "normal" characters as input
!
    IF LEN(FG_TYPEAHEAD.BUFF)>0 THEN
        CHR=FG_TYPEAHEAD.BUFF[1,1]; FG_TYPEAHEAD.BUFF=FG_TYPEAHEAD.BUFF[2,999]
        CHR.NBR=SEQ(CHR)
    END ELSE
        INCLUDE EB.OS.INCLUDES UT.INPUT.TIMEOUT
        INCLUDE EB.OS.INCLUDES SYSTEM.14
    END

    IF FG_TIMEDOUT THEN RETURN
    IF CHR.NBR=RTN.VAL OR (CHR.NBR>=FIRST.ASCII AND CHR.NBR<=LAST.ASCII) THEN RETURN

    IF COUNT(FG_INPUT.CODES,CHR)=0 THEN
        RETURN
    END
!
! following code tries to trap all characters from single key-stroke
!
    PREV.CHARS=CHR
    IF SYSTEM.14 = 0 THEN
        INCLUDE EB.OS.INCLUDES INPUT.DELAY
        INCLUDE EB.OS.INCLUDES SYSTEM.14
    END
    IF SYSTEM.14 > 0 THEN
        POS=1
        LOOP
        WHILE SYSTEM.14 AND POS<MAX.LEN DO
            FOR IC=1 TO SYSTEM.14
                INCLUDE EB.OS.INCLUDES INPUT.ZERO
                FG_TYPEAHEAD.BUFF:=CHR
                POS++
            NEXT IC
            INCLUDE EB.OS.INCLUDES INPUT.DELAY
            INCLUDE EB.OS.INCLUDES SYSTEM.14
        REPEAT
    END
    PREV.CHARS := FG_TYPEAHEAD.BUFF
    l = MAX.LEN
    LOOP
        CHR1=PREV.CHARS[1,l]
        LOCATE(CHR1,FG_INPUT.CODES,1;FG_ACT.CODE) ELSE FG_ACT.CODE=FALSE
    UNTIL FG_ACT.CODE OR l=1 DO l-- REPEAT
    IF NOT(FG_ACT.CODE) THEN l=1
    FG_TYPEAHEAD.BUFF = PREV.CHARS[l+1, LEN(PREV.CHARS)]
    CHR = ''; CHR.NBR=-1
    ECHO ON
    RETURN
