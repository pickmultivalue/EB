    SUBROUTINE EB_PRINT
!
    INCLUDE EB.EQUS EB.COMMON
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS ACT.CODES
!
    EQU AM TO CHAR(254), ESC TO CHAR(27), BELL TO CHAR(7), TOF TO CHAR(12)
MAIN$:!
    X=INDROW
    MCNT=DCOUNT(REC,AM)
    SCR.UD=1
!
    CRT MSG.CLR:"(S)creen or (P)rinter? ":
    ILEN=0; IDATA=''; INPTYPE='U'
    GOSUB 1500      ;! input the field
    IF IDATA=ESC THEN RETURN
    IF IDATA='^' OR FG_ACT.CODE=FG_BCK.CODE THEN RETURN
    TO_SCREEN = UPCASE(IDATA) EQ 'S'
5040 CRT MSG.CLR:"From line or (S)creen or (W)hole? ":
    ILEN=6; IDATA=''; INPTYPE='U'
    GOSUB 1500      ;! input the field
    FRLN=OCONV(IDATA,'MCU')
    CRT MSG.AKN:
    IF FRLN=ESC THEN RETURN
    IF FRLN='^' OR FG_ACT.CODE=FG_BCK.CODE THEN RETURN
    IF FRLN='' OR FRLN='F' THEN FRLN='F1'
    IF FRLN='B' THEN FRLN='B1'
    BEGIN CASE
        CASE NUM(FRLN); X=FRLN; GO 5050
        CASE FRLN[1,1]='F' AND FRLN[2,5] MATCHES('0N'); X+=(FRLN[2,5]*22)
        CASE FRLN[1,1]='B' AND FRLN[2,5] MATCHES('0N'); X-=(FRLN[2,5]*22)
        CASE FRLN='S'
            IF NOT(TO_SCREEN) THEN PRINTER ON
            PRINT TOF:FLNM 'L#20 ':ITNM
            PRINT
            RR=INDROW
            FOR I=1 TO 24
                PRINT RR 'R%4  ':RDSP(I); RR=RR+1
            NEXT I
            IF NOT(TO_SCREEN) THEN
                PRINTER CLOSE
                PRINTER OFF
            END
            RETURN
        CASE FRLN='W'
            IF TO_SCREEN THEN
                DC = DCOUNT(REC, @AM)
                CRT @(-1):FLNM:',':ITNM
                CRT
                FOR L = 1 TO DC
                    CRT REC<L>
                NEXT
                CRT MSG.CLR:"Press <return>":
                ILEN=1; IDATA=''; INPTYPE='U'
                GOSUB 1500      ;! input the field
                RETURN
            END ELSE
                WRITE REC ON JET.PASTE,'%':ITNM:'%'
                EXECUTE 'COPY JET.PASTE %':ITNM:'% (P'
                DELETE JET.PASTE,'%':ITNM:'%'
            END
            RETURN
    END CASE
5050 !
    FOR K=X TO X+22
        CRT @(0,K-X):CLEOL:K'R#4':' ':; CRTLN=REC<K>[1,75]; GOSUB CRT.LN
    NEXT K
    IF NOT(NUM(FRLN)) THEN GO 5040
5060 !
    CRT MSG.CLR:'To line ':
    ILEN=6; IDATA=''
    GOSUB 1500      ;! input the field
    TOLN=OCONV(IDATA,'MCU')
    CRT MSG.AKN:
    IF TOLN=ESC THEN RETURN
    IF TOLN='^' OR FG_ACT.CODE=FG_BCK.CODE THEN GO 5040
    IF NUM(TOLN) THEN GO 5065
    OK=0
    IF TOLN='' OR TOLN='F' THEN TOLN='F1'; OK=1
    IF TOLN='B' THEN TOLN='B1'; OK=1
    IF TOLN[1,1]='F' AND TOLN[2,5] MATCHES('0N') THEN X+=(TOLN[2,5]*22); OK=1
    IF TOLN[1,1]='B' AND TOLN[2,5] MATCHES('0N') THEN X-=(TOLN[2,5]*22); OK=1
    IF NOT(OK) THEN GO 5050
    IF X>MCNT THEN X=MCNT-22
    IF X<FRLN THEN X=FRLN
    GO 5050
5065 !
    IF TOLN<FRLN THEN GO 5060
    IF TOLN>MCNT THEN TOLN=MCNT
5070 ! perform Merge
    CRT MSG.CLR:'To perform print, enter <Y>, else press <RETURN> to abort  ':
    IDATA='Y'; ILEN=1; GOSUB 1500; DMY=OCONV(IDATA,'MCU')
    CRT MSG.AKN:
    IF INDEX(ESC:'N',TRIM(DMY),1) THEN RETURN
    IF DMY#'Y' THEN GO 5070
    CRT @(0,20):CLEOP
    PRINTER ON
    PRINT TOF:FLNM 'L#20 ':ITNM
    PRINT
    FOR J=FRLN TO TOLN
        PRINT J 'R%4  ':REC<J>
    NEXT J
    PRINTER CLOSE
    PRINTER OFF
    RQM
    RETURN
CRT.LN: !
    CONVERT @VM:@SVM TO ']\' IN CRTLN
    CRT OCONV(CRTLN,'MCP')
    RETURN
!
1500 !
!
    ECHO ON
    CALL EB_UT_WP(IDATA,INPTYPE,ILEN,1,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',ESC)
    INPTYPE='AN'
    RETURN
