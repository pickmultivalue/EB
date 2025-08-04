    SUBROUTINE EB_GETRPL(MAT RPL.PARMS,MAT RPL.PROMPTS,MAT RPL.COLS)
    INCLUDE EB.EQUS EB.COMMON
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS ACT.CODES
    EQU DELIMS TO ' ():;+-*/,&!^#=<>[]@'
    EQU MAX TO 9999
MAIN$:!
!
    DIM RPL.PARMS(3),RPL.PROMPTS(3),RPL.COLS(3)
    EQU WHOLE TO RPL.PARMS(1)
    EQU ALOC TO RPL.PARMS(2)
    EQU CONFIRM TO RPL.PARMS(3)
    PR="--Press <RETURN>"
    INPTYPE='AN'
    LSTR = ''
!
!  Get String to Be Replaced
!
    CNT=DCOUNT(REC,AM)
    GOSUB DisplayPrompt
    RPOS=1
    CONVERT AM TO VM IN RSS
    LOOP
        CRT @(ICOL,IROW):
        IF RPOS AND RPOS<21 THEN Z=RSS<1,RPOS>
        IF FG_ACT.CODE = FG_AMD.CODE THEN
            POS = FALSE
        END ELSE
            INPTYPE='LIT'; GOSUB INPT
            LOCATE FG_ACT.CODE IN CYCLES<am_start> SETTING POS ELSE POS=FALSE
            IF FG_ACT.CODE=FG_HLP.CODE THEN
                CALL EB_HELP('EBREPLACE', TRUE)
                GOSUB DisplayPrompt
                POS = TRUE
            END
        END
    WHILE POS DO
        BEGIN CASE
            CASE FG_ACT.CODE=FG_OPT.CODE
                CALL EB_CHOICES(10,8,71,20,'',RSS,Z,1,RPOS,1,'L#70','Previous Searches')
            CASE FG_ACT.CODE=FG_SKP.CODE OR FG_ACT.CODE=FG_MULTI.CODE; RPOS+=1
            CASE FG_ACT.CODE=FG_SEL.CODE
                ReplMode=NOT(ReplMode)
                GOSUB DisplayPrompt
            CASE 1; RPOS-=1
        END CASE
        IF RPOS<1 THEN RPOS=0
        FG_ACT.CODE=FALSE
    REPEAT
    IF FG_TIMEDOUT OR FG_ACT.CODE=FG_ABT.CODE THEN
        CRT MSG.CLR:
        RETURN
    END
    CONVERT VM TO AM IN RSS
    IF FG_ACT.CODE AND FG_ACT.CODE NE FG_AMD.CODE THEN GO 3099
    LOCATE Z IN RSS<am_start> SETTING RPOS ELSE
        IF RPOS<50 THEN RPOS=0 ELSE
            RPOS=50
            PSS<50>=''
        END
    END
    IF RPOS THEN
        DEL RSS<RPOS>
        Y=PSS<RPOS>
        DEL PSS<RPOS>
    END ELSE Y=''
    INS Z BEFORE RSS<1>
    INS Y BEFORE PSS<1>
    PPOS=1
    RSTR=Z
    CRT MSG.AKN:
    IF RSTR=ESC THEN GO 3099
    RSTRL=LEN(RSTR)
    IF RSTRL=0 THEN GO 3099
    IF NOT(ReplMode) THEN
        rdelim=''
        FOR rr=1 TO RSTRL
            c=RSTR[rr,1]
            IF NOT(c MATCHES "1A" OR NUM(c)) THEN
                IF NOT(c='-' AND rr>1 AND RSTR[rr-1,1] MATCHES "1N") THEN
                    rdelim=c
                    BREAK
                END
            END
        NEXT rr
        OK=TRUE
        IF LEN(rdelim)=1 THEN
            IF COUNT(RSTR,rdelim)=1 THEN OK=FALSE
        END ELSE OK=FALSE
        IF NOT(OK) THEN
            CRT MSG.CLR:'Malformed replace command':BELL:;RQM;
            GO 3099
        END
        ROPTS=OCONV(FIELD(RSTR, rdelim, 1), 'MCU')
        IF rdelim='-' THEN
            RANGE=''
        END ELSE
            RANGE=FIELD(ROPTS,'-',2)
            IF LEN(RANGE) NE 0 THEN
                IF RANGE[1,1]='E' THEN
                    RANGE=CNT
                END ELSE
                    RANGE=OCONV(RANGE,'MCN')
                END
                RANGE<2> = OCONV(ROPTS[1,COL1()-1],'MCN')
            END ELSE
                RANGE=OCONV(ROPTS,'MCN')
                IF LEN(RANGE) = 0 THEN RANGE = 1
                RANGE += INDROW+ROW-1
            END
        END
        LSTR=FIELD(RSTR, rdelim, 4)
        WSTR=FIELD(RSTR, rdelim, 3)
        RSTR=FIELD(RSTR, rdelim, 2)
3010    !  Get Replacement String
        IF NOT(ReplMode) THEN GO 3019

        CRT MSG.CLR:"With string ":
        ICOL=16; IROW=(PDEPTH-1); L=PWIDTH-17
        CONVERT AM TO VM IN PSS
        LOOP
            CRT @(ICOL,IROW):
            IF PPOS AND PPOS<21 THEN Z=PSS<1,PPOS>
            INPTYPE='LIT'; GOSUB INPT
            LOCATE FG_ACT.CODE IN CYCLES<am_start> SETTING POS ELSE POS=FALSE
        WHILE POS DO
            BEGIN CASE
                CASE FG_ACT.CODE=FG_OPT.CODE
                    CALL EB_CHOICES(50,8,31,10,'',PSS,Z,1,PPOS,1,'L#30','Previous Replaces')
                CASE FG_ACT.CODE=FG_SKP.CODE OR FG_ACT.CODE=FG_MULTI.CODE; PPOS+=1
                CASE 1; PPOS-=1
            END CASE
            IF PPOS<1 THEN PPOS=0
            FG_ACT.CODE=FALSE
        REPEAT
        CONVERT VM TO AM IN PSS
        IF FG_ACT.CODE THEN GO 3099
        DEL PSS<1>
        LOCATE Z IN PSS<am_start> SETTING WPOS ELSE
            IF WPOS<50 THEN WPOS=0 ELSE WPOS=50
        END
        INS Z BEFORE PSS<1>
        WSTR=Z
3019    *
        CRT MSG.AKN:
        ORIG.ROW=INDROW+ROW
3020    !  Get Start Line
        L=6; Z=INDROW+ROW; INPTYPE='N0'
        STRT=Z
        IF NOT(ReplMode) THEN
            IF LEN(RANGE<2>) NE 0 THEN STRT=RANGE<2>
            GO 3029
        END
        CRT MSG.CLR:"From line ":
        GOSUB INPT
        STRT=Z
        CRT MSG.AKN:
        IF STRT=ESC THEN GO 3099
        IF STRT<1 THEN GO 3020
        IF STRT>CNT THEN GO 3020
3029    *
        IF STRT=1 THEN LINE.POS=0 ELSE LINE.POS=INDEX(REC,AM,STRT-1)  ;!+1
3030    ! Get End Line
        IF NOT(ReplMode) THEN
            IF LEN(RANGE) NE 0 THEN
                ENDL=RANGE<1>
            END ELSE
                nlines = OCONV(ROPTS, 'MCN')
                IF nlines='' THEN
                    IF INDEX(ROPTS,'E',1) THEN
                        nlines = CNT
                    END ELSE
                        nlines = 1
                    END
                END
                ENDL=STRT+nlines-1
            END
            GO 3039
        END
        CRT MSG.CLR:"To line ":
        L=6; Z=''
        GOSUB INPT
        ENDL=Z
        CRT MSG.AKN:
        IF ENDL>CNT OR ENDL='E' THEN ENDL=CNT
        IF TRIM(ENDL)='' THEN ENDL=STRT
        IF ENDL=ESC OR NOT(NUM(ENDL)) OR ENDL<STRT THEN GO 3030
3039    !
        IF ENDL>=CNT THEN END.POS=LEN(REC) ELSE END.POS=INDEX(REC,AM,ENDL)-1
!
3040    !
        SwitchValues = FALSE
        IF ReplMode THEN
            WHOLE='YN'[1+(INDEX(RSTR,'@1',1) OR INDEX(DELIMS,RSTR[1,1],1) OR INDEX(DELIMS,RSTR[LEN(RSTR),1],1) OR INDEX(RSTR,' ',1)),1]
            IF ENDL=CNT THEN ALOC='N' ELSE ALOC='Y'
            IF ENDL=STRT THEN CONFIRM='N' ELSE CONFIRM='Y'
!
            YNR=(PDEPTH-1); YNCHRS='Y':VM:'N'; YNL=1
            FOR I=1 TO 3
                Y=RPL.PARMS(I)
                IF ReplMode THEN
                    CRT MSG.CLR:RPL.PROMPTS(I):Y:@(-9):
                    YNCHRS<3>=RPL.PARMS(I)
                    YNC=RPL.COLS(I); GOSUB GET.CHAR
                    IF FG_ACT.CODE THEN
                        IF FG_ACT.CODE=FG_BCK.CODE AND I>1 THEN I-=2 ELSE GO 3099
                    END ELSE
                        RPL.PARMS(I)=(Y NE 'N')
                    END
                END
            NEXT I
        END ELSE
            WHOLE=(INDEX(ROPTS,'V',1) OR INDEX(ROPTS,'W',1))
            WHOLE:=INDEX(ROPTS,'N',1)
            WHOLE:=INDEX(ROPTS,'X',1)
            WHOLE:=INDEX(ROPTS,'S',1)
            ALOC=INDEX(ROPTS,'U',1) OR INDEX(ROPTS,'A',1)
            CONFIRM=INDEX(ROPTS,'C',1)
            SwitchValues=ROPTS[1,1] EQ 'S'
        END
        CRT MSG.AKN:
! Perform the Replacements
        IF CONFIRM OR ENDL NE STRT THEN CRT ELSE IF ENDL=ORIG.ROW THEN CRT @(5,ROW):
        IF SwitchValues THEN
            SwitchValues = RSTR
            RSTR = WSTR
            WSTR = SwitchValues
        END
        IF RSTR[1,2]='C/' THEN          ;! convert chars
            RSTR=RSTR[3,MAX]
            L=LEN(RSTR)
            FOR I=1 TO L
                CHR.NBR=RSTR[I,3]
                IF CHR.NBR MATCHES "3N" THEN
                    RSTR=RSTR[1,I-1]:CHAR(CHR.NBR):RSTR[I+3,MAX]
                    L-=2
                END
            NEXT I
        END
        IF WSTR[1,2]='C/' THEN          ;! convert chars
            WSTR=WSTR[3,MAX]
            L=LEN(WSTR)
            FOR I=1 TO L
                CHR.NBR=WSTR[I,3]
                IF CHR.NBR MATCHES "3N" THEN
                    WSTR=WSTR[1,I-1]:CHAR(CHR.NBR):WSTR[I+3,MAX]
                    L-=2
                END
            NEXT I
        END
        IF ALOC AND STRT EQ 1 AND ENDL EQ CNT AND WHOLE[4,1] AND NOT(INDEX(RSTR, '@', 1)) THEN
            REC = CHANGE(REC, RSTR, WSTR)
        END ELSE
            RSTRL=LEN(RSTR)
            LEN.DIFF=LEN(WSTR)-RSTRL
            CALL EB_REPLACE(ENDL,STRT,LINE.POS,END.POS,LEN.DIFF,MAT RPL.PARMS,MAT RPL.PROMPTS,MAT RPL.COLS)
        END
3095    !
        BEGIN CASE
            CASE LSTR='FI'
                FG_ACT.CODE = FG_AMD.CODE
            CASE LSTR='A'
                FG_ACT.CODE = FG_MULTI.CODE
            CASE 1
                IF ENDL NE STRT THEN
                    CRT; CRT MSG.CLR:"That's all!  ":PR:" for original page, or line number? ":
                    L=6; Z=""; INPTYPE='N0'
                    GOSUB INPT
                    Y=Z
                    CRT MSG.DSP:
                    IF Y="" THEN Y=INDROW
                    IF Y<1 THEN Y=1
                    IF Y>CNT THEN Y=CNT-(PDEPTH-2)
                    INDROW=Y
                    CRT @(0,0):CLEOP:
                    SCR.UD=TRUE
                END ELSE
                    IF ENDL=ORIG.ROW THEN
                        RDSP(LROW)=REC<ENDL>
                        GOSUB 6000
                    END
                    SCR.UD=NOT(ALOC AND NOT(CONFIRM))
                END
        END CASE
3099    !
        CRT MSG.DSP:
        RETURN
INPT:   !
        POS=1
        EDITED=FALSE
        CALL EB_UT_WP(Z,INPTYPE,L,1,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',ESC)
        INPTYPE='AN'
        RETURN
GET.CHAR: !
        CALL EB_UT_INPUT_ZERO(Y,MAT EB_CHARS,FG_ACT.CODE,YNC,YNR,FG_INPUT.CODES,YNCHRS,YNL,FG_TIMEOUT)
        RETURN
6000    ! Incorporate changed lines into dynamic array, REC.
        CHANGES(LROW)=TRUE
        FOR I=1 TO PDEPTH
            IF CHANGES(I) THEN
                CALL EB_TRIM(RDSP(I),RDSP(I):'',' ','T')
                REC<I+INDROW-1>=RDSP(I)
            END
        NEXT I
        CHANGED=FALSE; MAT CHANGES=FALSE
        RETURN
DisplayPrompt:
        IF ReplMode THEN
            ReplPrompt='Replace string'
        END ELSE
            ReplPrompt='Search/Replace ([F1]/[F2]/up-arrow/down-arrow)'
        END
        CRT MSG.CLR:ReplPrompt:' ':
        ICOL=LEN(ReplPrompt)+1
        IROW=(PDEPTH-1)
        L=PWIDTH-ICOL-1:@AM:999
        RETURN
    END
