    SUBROUTINE EB_UT_INPUT_ZERO(CHR,MAT EB$CHARS,FG$ACT.CODE,COL,ROW,CODES,SUBCODES,MAX.SUB,FG$TIMEOUT)
!
    DIM EB$CHARS(100)
    INCLUDE EB.EQUS EB.CHARS
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE JBC.h
    DEFC INT JBASEEmulateGETINT(INT, INT)
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    vm_start=IF_COMPILED_PRIME
!INCLUDE jutil.h
!
    CHR = ''
    FG$TIMEDOUT=FALSE
    FG$TYPEAHEAD.BUFF=''
    SUB.CODES=SUBCODES
    DFLT.CHR=SUB.CODES<3>
    MATCH.SET=SUB.CODES<2>; SUB.CODES=SUB.CODES<1>
    FG$MONITOR.SECS=FG$TIMEOUT<2>; FG$TIMEOUT=FG$TIMEOUT<1>; IF FG$MONITOR.SECS='' THEN FG$MONITOR.SECS=FG$TIMEOUT
    IF SUB.CODES MATCHES "1X'-'1X" THEN
        SUB.CODE1=SEQ(FIELD(SUB.CODES,'-',1))
        SUB.CODE2=SEQ(SUB.CODES[COL2()+1,1])
        MAX.SUB=0
        SUB.JUST='R#1'
    END ELSE
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
!
    ECHO OFF
    PREV.CHARS=''
    PREV.SUBS=''
    FG$ACT.CODE=FALSE
    CODE=FALSE
    CRT @(COL,ROW):DFLT.CHR:
    LOOP
        CRT @(COL,ROW):
        INCLUDE EB.OS.INCLUDES UT.INPUT.TIMEOUT
        IF FG$TIMEDOUT THEN RETURN
        IF (CHR.NBR<FIRST.ASCII OR CHR.NBR>LAST.ASCII) AND COUNT(CODES,CHR)>1 THEN
            INCLUDE EB.OS.INCLUDES INPUT.DELAY
            INCLUDE EB.OS.INCLUDES SYSTEM.12
            LOOP
                INCLUDE EB.OS.INCLUDES INPUT.DIFF
                INCLUDE EB.OS.INCLUDES SYSTEM.14
            WHILE SYSTEM.14 AND DIFF<=0 DO
                PREV.CHARS:=CHR
                PREV.SUBS:=CHR
                INCLUDE EB.OS.INCLUDES INPUT.ZERO
                IF FG$EXPECT.CR AND CHR=CR THEN CHR=''; CHR.NBR=0
                INCLUDE EB.OS.INCLUDES SYSTEM.12
            REPEAT
        END
        PREV.CHARS=(PREV.CHARS:CHR) MAX.JUST
        CHR1=''
        FOR I=FG$MAX.CHARS TO 1 STEP -1 UNTIL FG$ACT.CODE
            CHR1=PREV.CHARS[I,1]:CHR1
            INCLUDE EB.OS.INCLUDES CHECK.ACT.SUB
        NEXT I
        IF FG$ACT.CODE THEN
            CHR=CHR1
        END ELSE
            IF CHR.NBR#13 THEN
                IF MAX.SUB THEN
                    IF CONV THEN CHR=OCONV(CHR,'MCU') SUB.JUST
                    PREV.SUBS=(PREV.SUBS:CHR) SUB.JUST
                    FOR I=1 TO MAX.SUB UNTIL CODE
                        CHR1=PREV.SUBS[I,MAX.SUB]
                        INCLUDE EB.OS.INCLUDES CHECK.SUB.SUB
                    NEXT I
                    IF CONV AND NOT(CODE) THEN
                        PREV.SUBS=OCONV(PREV.SUBS,'MCU')
                        FOR I=1 TO MAX.SUB UNTIL CODE
                            CHR1=PREV.SUBS[I,MAX.SUB]
                            INCLUDE EB.OS.INCLUDES CHECK.SUB.SUB
                        NEXT I
                    END
                END ELSE
                    IF CHR.NBR>SUB.CODE1 AND CHR.NBR<=SUB.CODE2 THEN CODE=TRUE
                END
                IF CODE THEN
                    CHR=CHR1
                    RETURN
                END ELSE
                    IF MATCH.SET#'' THEN
                        IF CHR1 MATCHES MATCH.SET THEN CHR=CHR1; GOTO FINISH
                    END
                END
            END
        END
    UNTIL CHR=CR OR FG$ACT.CODE DO REPEAT
    IF CHR=CR THEN CHR=DFLT.CHR
FINISH: !
    ECHO ON
    RETURN
