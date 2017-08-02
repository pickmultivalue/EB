    SUBROUTINE EB_GET_INPUT(CHR, CHR.NBR)
* @(#) EB_GET_INPUT.b Ported to jBASE 07:23:52  18 FEB 2010
    INCLUDE EB.EQUS EB.COMMONS
    GO MAIN$
!=========== Program's Purpose ===============
!
! No echo input prompt to return a value or
! a FG$ACT.CODE.
!
!=============================================
!
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS EB.CHARS
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS STD.EQUS
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
    ECHO OFF
    MAX.LEN=FG$MAX.CHARS
    PREV.CHARS=''
    FG$ACT.CODE=FALSE
    SYSTEM.14 = 0
!
! If FG$TYPEAHEAD was not null, process any "normal" characters as input
!
    IF LEN(FG$TYPEAHEAD.BUFF)>0 THEN
    CHR=FG$TYPEAHEAD.BUFF[1,1]; FG$TYPEAHEAD.BUFF=FG$TYPEAHEAD.BUFF[2,999]
    CHR.NBR=SEQ(CHR)
END ELSE
INCLUDE EB.OS.INCLUDES UT.INPUT.TIMEOUT
INCLUDE EB.OS.INCLUDES SYSTEM.14
END

IF FG$TIMEDOUT THEN RETURN
IF CHR.NBR=RTN.VAL OR (CHR.NBR>=FIRST.ASCII AND CHR.NBR<=LAST.ASCII) THEN RETURN

IF COUNT(FG$INPUT.CODES,CHR)=0 THEN
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
    FG$TYPEAHEAD.BUFF:=CHR
    POS++
NEXT IC
INCLUDE EB.OS.INCLUDES INPUT.DELAY
INCLUDE EB.OS.INCLUDES SYSTEM.14
REPEAT
END
PREV.CHARS := FG$TYPEAHEAD.BUFF
l = MAX.LEN
LOOP
CHR1=PREV.CHARS[1,l]
LOCATE(CHR1,FG$INPUT.CODES,1;FG$ACT.CODE) ELSE FG$ACT.CODE=FALSE
UNTIL FG$ACT.CODE OR l=1 DO l-- REPEAT
IF NOT(FG$ACT.CODE) THEN l=1
FG$TYPEAHEAD.BUFF = PREV.CHARS[l+1, LEN(PREV.CHARS)]
CHR = ''; CHR.NBR=-1
ECHO ON
RETURN