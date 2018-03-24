    SUBROUTINE EB_SWAP(STMP,POS)
* @(#) EB_SWAP.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.SWAP Ported to jBASE 16:15:16  27 JUL 2001
!
! Program to swap statments around
! ie. var1=var2 to var2=var1
!     READs to WRITEs and vice versa
!
    EQU MAX TO 999
    OPERANDS='+-:'
    ALPHA.MATCH="0X2N' '3A' '4N0X"
    LONG.MATCH="0X2N'/'2N'/'4N0X"
    SHORT.MATCH="0X2N'/'2N'/'2N0X"
!
! Get indentation
!
    ORIG.STRING=STMP
    FIRSTCHR=STMP[1,1]
    IF INDEX(' ':CHAR(9), FIRSTCHR, 1) THEN
        I=1
        LOOP WHILE STMP[I,1]=FIRSTCHR DO I+=1 REPEAT
        INDENT=STR(FIRSTCHR,I-1)
        CALL EB_TRIM(STMP,STMP[I,MAX],FIRSTCHR,'T')
    END ELSE INDENT = ''
    IF STMP 'R#1'=';' THEN
        SUFFIX=';'
        STMP=STMP[1,LEN(STMP)-1]
    END ELSE SUFFIX=''
    NBR.WORDS=DCOUNT(STMP,' ')
!
! READs to WRITEs
!
    RPOS=INDEX(STMP,'READ',1)
    IF RPOS AND INDEX(STMP[RPOS+4,3],' ',1) THEN
        POS=INDEX(STMP,' FROM ',1)
        IF POS THEN
            POS = INDEX(STMP, ' LOCKED', 1)
            IF NOT(POS) THEN POS = INDEX(STMP, ' THEN', 1)
            IF NOT(POS) THEN POS = INDEX(STMP, ' ELSE', 1)
            IF POS THEN
                STMP=STMP[1,POS-1]
                RSTMP=STMP[RPOS+4,MAX]:SUFFIX
                IF RSTMP[1,1]='U' THEN RSTMP=RSTMP[2,MAX]
                STMP=INDENT:STMP[1,RPOS-1]:'WRITE':RSTMP
                STMP=SWAP(STMP,' FROM ',' ON ')
                RETURN
            END
        END
    END
!
! WRITEs to READs
!
    RPOS=INDEX(STMP,'WRITE',1)
    IF RPOS AND INDEX(STMP[RPOS+5,3],' ',1) THEN
        POS=INDEX(STMP,' ON ',1)
        IF POS THEN
            SOP=INDEX(STMP[POS+5,MAX],',',1)
            SOP+=INDEX(STMP[SOP+1,MAX],' ',1)+2
            IF SOP THEN
                STMP=STMP[1,POS+SOP+4]
            END
            STMP=SWAP(STMP,' ON ',' FROM ')
            STMP=INDENT:STMP[1,RPOS-1]:'READ':STMP[RPOS+5,MAX]:SUFFIX
            RETURN
        END
    END
!
! LOCATE to INS BEFORE
!
    POS=INDEX(STMP,'LOCATE ',1)
    IF POS THEN
        SOP=INDEX(STMP,'SETTING',1)
        POS.VAR=FIELD(STMP[SOP,MAX],' ',2)
        SOP = 1
        LOOP
            CONV = 'G3 ':SOP
            INS.VAR=OCONV(STMP, CONV)
        WHILE INS.VAR 'R#1' = ',' DO SOP++ REPEAT
        IF INS.VAR 'R#1'='>' THEN
            INS.VAR=FIELD(INS.VAR,'>',1):',':POS.VAR:'>'
        END ELSE
            INS.VAR:='<':POS.VAR:'>'
        END
        STMP=INDENT:'    INS ':FIELD(STMP,' ',2):' BEFORE ':INS.VAR:SUFFIX
        RETURN
    END
!
! FOR xx=1 TO count to FOR xx=count TO 1 STEP -1
!
    POS=STMP[1,4]='FOR '
    IF POS THEN
        PART1=FIELD(STMP,'=',1)
        PART2=TRIM(STMP[COL2()+1,MAX])
        VAR1=FIELD(PART2,' ',1)
        VAR2=FIELD(PART2,' ',3)
        STMP=INDENT:PART1:'=':VAR2:' TO ':VAR1:' STEP -1':SUFFIX
        RETURN
    END
!
! SUBROUTINE to CALL
!
    POS=STMP[1,11]='SUBROUTINE '
    IF POS THEN
        STMP=INDENT:'CALL':STMP[11,MAX]:SUFFIX
        RETURN
    END
!
! CALL to SUBROUTINE(
!
    POS=STMP[1,5]='CALL '
    IF POS THEN
        STMP=INDENT:'SUBROUTINE':STMP[5,MAX]:SUFFIX
        RETURN
    END
!
! INS to DEL
!
    POS=STMP[1,4]='INS '
    IF POS THEN
        INS.VAR=FIELD(STMP,' ',4)
        STMP=INDENT:'DEL ':INS.VAR:SUFFIX
        RETURN
    END
!
! END to END CASE
!
    POS=STMP[1,3]='END'
    IF POS THEN
        STMP=INDENT:'END CASE':SUFFIX
        RETURN
    END
!
! IF to CASE
!
    IF FIELD(STMP,' ',1)='IF' THEN
        IF FIELD(STMP,' ',NBR.WORDS)='THEN' THEN
            STMP=INDENT:'CASE ':OCONV(STMP,'G1 ':NBR.WORDS-2):SUFFIX
            POS=1
        END ELSE
            POS=INDEX(STMP,' THEN ',1)
            IF POS AND NOT(INDEX(STMP,' ELSE ',1)) THEN
                STMP=SWAP(STMP,' THEN ',' ; ')
                STMP=INDENT:'CASE ':OCONV(STMP,'G1 ':NBR.WORDS):SUFFIX
            END
        END
        RETURN
    END
!
! CASE to IF
!
    IF FIELD(STMP,' ',1)='CASE' THEN
        STMP = STMP[COL2(), MAX]
        SUFFIX = FIELD(STMP:';', ';', 2)
        STMP = STMP[1, COL1()-1]
        SUFFIX = TRIM(' THEN ':SUFFIX, ' ', 'L')
        SUFFIX = TRIM(SUFFIX, ' ', 'T')
        STMP=INDENT:'IF':STMP:SUFFIX
        POS = 1
        RETURN
    END
!
    POS=INDEX(STMP,'=',1)
    IF POS THEN
!
! =+ to += OR var=var+ to var+=
!
        OPERAND=STMP[POS+1,1]
        IF INDEX(OPERANDS,OPERAND,1) THEN
            STMP=INDENT:STMP[1,POS-1]:OPERAND:'=':STMP[POS+2,MAX]:SUFFIX
            RETURN
        END
        VAR1=STMP[1,POS-1]
        SOP=POS+1
        LOOP
            OPERAND=STMP[SOP,1]
        UNTIL INDEX(OPERANDS,OPERAND,1) DO SOP+=1 REPEAT
        VAR2=STMP[POS+1,SOP-POS-1]
        IF VAR1=VAR2 THEN
            STMP=INDENT:STMP[1,POS-1]:OPERAND:'=':STMP[SOP+1,MAX]:SUFFIX
            RETURN
        END
!
! Equals (swap sides)
!
        VAR2 = STMP[POS+1,MAX]
        VAR1 = STMP[1,POS-1]
        V1P = ''
        V2S = ''
        IF VAR1[LEN(VAR1), 1] = ' ' THEN
            V1P = ' '
            VAR1 = VAR1[1, MAX]
        END
        IF VAR2[1, 1] = ' ' THEN
            VAR2 = VAR2[2, MAX]
            V2S = ' '
        END
        TORF = 1:@AM:0:@AM:'TRUE':@AM:'FALSE'
        LOCATE VAR2 IN TORF SETTING TORFPOS THEN
            V2 = VAR1
            IF MOD(TORFPOS, 2) THEN TORFPOS++ ELSE TORFPOS--
            VAR1 = V1P:TORF<TORFPOS>
            VAR2 = V2
        END ELSE
            VAR1 = V1P:VAR1
            VAR2 = VAR2:V2S
        END
        STMP=INDENT:VAR2:'=':VAR1:SUFFIX
        RETURN
    END
!
! Convert Date
!
    IF STMP MATCHES ALPHA.MATCH THEN        ;! convert to LONG.DATE
        MTCH=SWAP(ALPHA.MATCH,'0X','')
        L=LEN(OCONV(DATE(),'D'))
        GOSUB GET.DATE
        STMP=SWAP(STMP,ALPHA.DATE,LONG.DATE)
        STMP=INDENT:STMP:SUFFIX
        POS=1
        RETURN
    END
    IF STMP MATCHES LONG.MATCH THEN         ;! convert to SHORT.DATE
        MTCH=SWAP(LONG.MATCH,'0X','')
        L=LEN(OCONV(DATE(),'D4/'))
        GOSUB GET.DATE
        STMP=SWAP(STMP,LONG.DATE,SHORT.DATE)
        STMP=INDENT:STMP:SUFFIX
        POS=1
        RETURN
    END
    IF STMP MATCHES SHORT.MATCH THEN        ;! convert to ALPHA.DATE
        MTCH=SWAP(SHORT.MATCH,'0X','')
        L=LEN(OCONV(DATE(),'D2/'))
        GOSUB GET.DATE
        STMP=SWAP(STMP,SHORT.DATE,ALPHA.DATE)
        STMP=INDENT:STMP:SUFFIX
        POS=1
        RETURN
    END
! * PROGRAM to SUBROUTINE to FUNCTION
    POS=INDEX(ORIG.STRING,'! PROGRAM',1)
    IF POS THEN
        STMP=SWAP(ORIG.STRING,'! PROGRAM','  SUBROUTINE')
        RETURN
    END
    POS=INDEX(ORIG.STRING,'SUBROUTINE',1)
    IF POS THEN
        STMP=SWAP(ORIG.STRING, 'SUBROUTINE', 'FUNCTION')
        RETURN
    END
    POS=INDEX(ORIG.STRING,'FUNCTION',1)
    IF POS THEN
        STMP=SWAP(ORIG.STRING, 'FUNCTION', 'PROGRAM')
        RETURN
    END
    STMP=ORIG.STRING
!
! MATPARSE to MATBUILD
!
    WORD = 'MATPARSE'
    RWORD = 'MATBUILD'
    GOSUB MATSWITCH
    IF STMP # ORIG.STRING THEN RETURN
!
    WORD = 'MATBUILD'
    RWORD = 'MATPARSE'
    GOSUB MATSWITCH
    IF STMP # ORIG.STRING THEN RETURN
!
    RETURN
GET.DATE: !
    P=1
    LOOP
        DTE=STMP[P,L]
    UNTIL DTE MATCHES MTCH DO P+=1 REPEAT
    DTE=ICONV(DTE,'D')
    ALPHA.DATE=OCONV(DTE,'D')
    LONG.DATE=OCONV(DTE,'D4/')
    SHORT.DATE=OCONV(DTE,'D2/')
    RETURN
!
MATSWITCH:
!
    RPOS=INDEX(STMP, WORD, 1)
    IF RPOS AND INDEX(STMP[RPOS+LEN(WORD),3],' ',1) THEN
        RSTMP = STMP[1, RPOS-1]
        STMP=STMP[RPOS+LEN(WORD),MAX]
        POS=INDEX(STMP,' FROM ',1)
        IF POS THEN
            STMP = CHANGE(STMP, ' FROM ', @AM)
            STMP=INDENT:RSTMP:RWORD:' ':TRIM(STMP<2>):' FROM ':TRIM(STMP<1>):SUFFIX
            RETURN
        END
    END
    STMP = ORIG.STRING
    RETURN
