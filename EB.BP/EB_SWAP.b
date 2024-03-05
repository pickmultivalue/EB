    SUBROUTINE EB_SWAP(STMP,POS)
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
    WHOLENUM.MATCH="1N0N":@VM:"1N0N' '0X"
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
    USTMP=UPCASE(STMP)
    RPOS=INDEX(USTMP,'READ',1)
    IF RPOS AND INDEX(USTMP[RPOS+4,3],' ',1) THEN
        POS=INDEX(USTMP,' FROM ',1)
        IF POS THEN
            POS = INDEX(USTMP, ' LOCKED', 1)
            IF NOT(POS) THEN POS = INDEX(USTMP, ' THEN', 1)
            IF NOT(POS) THEN POS = INDEX(USTMP, ' ELSE', 1)
            IF POS THEN
                STMP=STMP[1,POS-1]
                RSTMP=STMP[RPOS+4,MAX]:SUFFIX
                IF RSTMP[1,1]='U' THEN RSTMP=RSTMP[2,MAX]
                IF INDEX(STMP,'read',1) THEN
                    w = 'write'
                    f = ' from '
                    o = ' on '
                END ELSE
                    w = 'WRITE'
                    f = ' FROM '
                    o = ' ON '
                END
                STMP=INDENT:STMP[1,RPOS-1]:w:RSTMP
                STMP=SWAP(STMP,f,o)
                RETURN
            END
        END
    END
!
! WRITEs to READs
!
    RPOS=INDEX(USTMP,'WRITE',1)
    IF RPOS AND INDEX(USTMP[RPOS+5,3],' ',1) THEN
        POS=INDEX(USTMP,' ON ',1)
        IF POS THEN
            SOP=INDEX(STMP[POS+5,MAX],',',1)
            SOP+=INDEX(STMP[SOP+1,MAX],' ',1)+2
            IF SOP THEN
                STMP=STMP[1,POS+SOP+4]
            END
            IF INDEX(STMP,'write',1) THEN
                r = 'read'
                f = ' from '
                o = ' on '
            END ELSE
                r = 'READ'
                f = ' FROM '
                o = ' ON '
            END
            STMP=SWAP(STMP,o,f)
            STMP=INDENT:STMP[1,RPOS-1]:r:STMP[RPOS+5,MAX]:SUFFIX
            RETURN
        END
    END
!
! LOCATE to INS BEFORE
!
    POS=INDEX(USTMP,'LOCATE ',1)
    IF POS THEN
        SOP=INDEX(USTMP,'SETTING',1)
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
        IF INDEX(STMP,'locate',1) THEN
            i = 'ins'
            b = 'before'
        END ELSE
            i = 'INS'
            b = 'BEFORE'
        END
        STMP=INDENT:'    ':i:' ':FIELD(STMP,' ',2):' ':b:' ':INS.VAR:SUFFIX
        RETURN
    END
!
! FOR xx=1 TO count to FOR xx=count TO 1 STEP -1
!
    POS=USTMP[1,4]='FOR '
    IF POS THEN
        PART1=FIELD(STMP,'=',1)
        PART2=STMP[COL2()+1,MAX]
        SPACING = ''
        I = 1
        LOOP WHILE PART2[I,1] EQ ' ' DO
            SPACING := ' '
            I++
        REPEAT
        PART2 = PART2[I,MAX]
        VAR1=FIELD(PART2,' ',1)
        VAR2=FIELD(PART2,' ',3)
        hass=FIELD(PART2,' ',4)
        IF STMP[1,3] EQ 'FOR' THEN
            t = ' TO '
            s = 'STEP'
        END ELSE
            t = ' to '
            s = 'step'
        END
        IF hass EQ 'STEP' THEN
            s = ''
        END ELSE
            s = ' ':s
            s := ' -1':SUFFIX
        END
        STMP=INDENT:PART1:'=':SPACING:VAR2:t:VAR1:s
        RETURN
    END
!
! INS to DEL
!
    POS=USTMP[1,4]='INS '
    IF POS THEN
        INS.VAR=FIELD(STMP,' ',4)
        IF STMP[1,3] EQ 'INS' THEN
            d = 'DEL '
        END ELSE
            d = 'del '
        END
        STMP=INDENT:d:INS.VAR:SUFFIX
        RETURN
    END
!
! END to END CASE
!
    POS=USTMP[1,3]='END'
    IF POS THEN
        IF STMP[1,3] EQ 'END' THEN
            ec = 'END CASE'
        END ELSE
            ec = 'end case'
        END
        STMP=INDENT:ec:SUFFIX
        RETURN
    END
!
! IF to CASE
!
    IF FIELD(USTMP,' ',1)='IF' THEN
        IF STMP[1,2] = 'IF' THEN
            c = 'CASE '
            t = ' THEN '
        END ELSE
            c = 'case'
            t = ' then '
        END
        IF FIELD(USTMP,' ',NBR.WORDS)='THEN' THEN
            STMP=INDENT:c:OCONV(STMP,'G1 ':NBR.WORDS-2):SUFFIX
            POS=1
        END ELSE
            POS=INDEX(USTMP,' THEN ',1)
            IF POS AND NOT(INDEX(USTMP,' ELSE ',1)) THEN
                STMP=SWAP(STMP,t,' ; ')
                STMP=INDENT:c:OCONV(STMP,'G1 ':NBR.WORDS):SUFFIX
            END
        END
        RETURN
    END
!
! CASE to IF
!
    IF FIELD(USTMP,' ',1)='CASE' THEN
        IF STMP[1,4] EQ 'CASE' THEN
            t = ' THEN '
            i = 'IF'
        END ELSE
            t = ' then '
            i = 'if'
        END
        STMP = STMP[COL2(), MAX]
        SUFFIX = FIELD(STMP:';', ';', 2)
        STMP = STMP[1, COL1()-1]
        SUFFIX = TRIM(t:SUFFIX, ' ', 'L')
        SUFFIX = TRIM(SUFFIX, ' ', 'T')
        STMP=INDENT:i:STMP:SUFFIX
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
        IF STMP EQ USTMP THEN
            t = 'TRUE'
            f = 'FALSE'
        END ELSE
            t = 'true'
            f = 'false'
        END
        TORF = 1:@AM:0:@AM:t:@AM:f
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
    IF STMP MATCHES SHORT.MATCH THEN        ;! convert to internal
        MTCH=SWAP(SHORT.MATCH,'0X','')
        L=LEN(OCONV(DATE(),'D2/'))
        GOSUB GET.DATE
        STMP=SWAP(STMP,SHORT.DATE,DTE)
        STMP=INDENT:STMP:SUFFIX
        POS=1
        RETURN
    END
    IF TRIM(STMP) MATCHES WHOLENUM.MATCH THEN        ;! convert to ALPHA.DATE
        SUFFIX = ' ':FIELD(STMP,' ', 2, 999)
        STMP = STMP[1, COL1()-1]
        STMP = OCONV(STMP, 'D')
        STMP=INDENT:STMP:SUFFIX
        POS=1
        RETURN
    END
! PROGRAM to SUBROUTINE to FUNCTION
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
        STMP=SWAP(ORIG.STRING, 'FUNCTION', 'CALL')
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
    STMP=ORIG.STRING
!
! MATPARSE to MATBUILD
!
    WORD = 'MATPARSE'
    RWORD = 'MATBUILD'
    GOSUB MATSWITCH
    IF STMP NE ORIG.STRING THEN RETURN
!
    WORD = 'MATBUILD'
    RWORD = 'MATPARSE'
    GOSUB MATSWITCH
    IF STMP NE ORIG.STRING THEN RETURN
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
    RPOS=INDEX(USTMP, WORD, 1)
    IF RPOS AND INDEX(USTMP[RPOS+LEN(WORD),3],' ',1) THEN
        RSTMP = STMP[1, RPOS-1]
        STMP=STMP[RPOS+LEN(WORD),MAX]
        POS=INDEX(USTMP,' FROM ',1)
        IF INDEX(STMP,' FROM ',1) THEN
            f = ' FROM '
        END ELSE
            f = ' from '
            RWORD = DOWNCASE(RWORD)
        END
        IF POS THEN
            STMP = CHANGE(STMP, ' FROM ', @AM)
            STMP=INDENT:RSTMP:RWORD:' ':TRIM(STMP<2>):' FROM ':TRIM(STMP<1>):SUFFIX
            RETURN
        END
    END
    STMP = ORIG.STRING
    RETURN
