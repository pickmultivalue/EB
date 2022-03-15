    SUBROUTINE EB_TCL
!
!=========== Program's Purpose ===============
!
! TCL Stacker
!
! "."                    exit stack
!
! .{X}{n{,n,n,...}}      eXecutes stack entrie(s)
!                        n can be numeric or any TCL command
! .Ln                    List first n entries
! .LPn                   Print first n entries to currently assigned ptr
! .R{s}                  Edit entry s (default=1)
! .R{s{-f}}/str1/str2    Replace string from entries s thru f
! .A{s{-f}}/str1         Appends string from entries s thru f
!
!  NOTE: the "/" delimiter can be any non-alphanumeric
!
! .Ds{-f}                Deletes entries s thru f
! .Tn                    Pops nth entry to Top of stack
! .E{n}                  Edit stack starting from nth entry
!
!=============================================
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(500),EB.FILE.LIST
    COM RDSP(100), CHANGES(100)
!
    GO MAIN$
!
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS SCR.CRT.PARAMS
    INCLUDE EB.EQUS CRT.PARAMS
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS EB.CHARS
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS COLOURS
    EQU SQUERY.VERB TO 'SQUERY'
    EQU MAX TO 9999
MAIN$:!
    DEFC INT JBASEEmulateGETINT(INT, INT)
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    vm_start=IF_COMPILED_PRIME
    INCLUDE EB.OS.INCLUDES OS
!
    IF FG_OSTYPE='UDT' THEN
        OPEN 'CTLGTB' TO F.CTLGTB ELSE
            MSG='Cannot open file CTLGTB'
            CALL EB_MERRMSG(BELL,MSG,'',RESP,'OK')
            RETURN
        END
    END
!
    INCLUDE EB.OS.INCLUDES TIMEOUT.OFF
!
    EQU MAX.STACK TO 1000
    TCL.LIST=''
    READ TCL.PARAMS FROM FG_EB.CONTROL,'TCL.PARAMS' ELSE
        INCLUDE EB.OS.INCLUDES TCL.PARAMS
    END
    STK.FILE=TCL.PARAMS<1>
    STACK.ID=TCL.PARAMS<2>
    IF TCL.PARAMS<4> THEN
        STACK.ID:=OCONV(FG_TUSER,'MCL')
    END ELSE
        PORT.HASH=TCL.PARAMS<3>
        STACK.ID:=FG_TLINE PORT.HASH
    END
    CALL EB_OPEN_FILE(STK.FILE,POS)
    IF POS THEN STACK.FILE=OPENED.FILES(POS) ELSE
        STACK.FILE=FG_WORK.FILE
        STK.FILE='EB.WORK,':FG_TLINE
        STACK.ID='TCL.STACK'
    END
    READ STACK FROM STACK.FILE,STACK.ID ELSE STACK=''
    RNET=INDEX(CRT.BOX,'\\box',1)
    GUI=FG_STERM=6
!
    SEARCH.STRING=''
    EQU CTRL.X TO CHAR(24)
!
    EB.PROMPT=FG_TLINE:':':FG_TACC.NAME:'>'
    COL=LEN(EB.PROMPT)+1; PROW=22
    POST.PROMPT=FALSE
    EB.PROMPT=@(0,PROW):BG:EB.PROMPT:FG
    SAVE.COLOURS=FG_CURR.COLOURS:AM:FG_PREV.COLOURS
    IF WHITE<1,1> NE '' THEN CALL EB_CH_COLOUR(FG_COLOURS<1,4,1>,FG_COLOURS<1,4,2>)
    FG_CURR.COLOURS=SAVE.COLOURS<1>; FG_PREV.COLOURS=SAVE.COLOURS<2>
    IF GUI THEN
        CALL EB_ERRMSG(FG_ERROR.MSGS<83>, 0)
        CALL EB_GUI_SEND('r','')
        PROMPT ''
    END ELSE CRT @(0,PROW):CLEOP:@(0,21):FG_ERROR.MSGS<83>
    IF MOD(FG_STERM,3) THEN
        OPTIONS=2:AM:-1
        OPTIONS<6>=1
    END
    LOOP
        IF SYSTEM(11) OR TCL.LIST NE '' THEN PROMPT.CHR='>' ELSE PROMPT.CHR=' '
        CRT EB.PROMPT:PROMPT.CHR:CLEOL:
        COMMAND=''
        IF GUI THEN
            CALL EB_GUI_SEND('r','')
        END
        CALL EB_INPUT(COMMAND,'AN',200,0,MAT EB_CHARS,MAT OTHER.PARAMS,FG_EB.PARAMS,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,'','','','')
        CRT
!    END
        UCOMMAND=OCONV(COMMAND,'MCU')
    UNTIL UCOMMAND='OFF' OR COMMAND='.' OR FG_ACT.CODE=FG_ABT.CODE OR FG_ACT.CODE=FG_END.CODE DO
        IF UCOMMAND[1,3]='.DE' THEN
            COMMAND='.D':COMMAND[4,MAX]; UCOMMAND=OCONV(COMMAND,'MCU')
        END
        ORIG.COMMAND=COMMAND
        BEGIN CASE
            CASE COMMAND=FG_MOUSE.CODE
                ECHO OFF
                INPUT NOTHING
                ECHO ON
            CASE COMMAND='.?' OR FG_ACT.CODE=FG_HLP.CODE
                CRT @(-1):
                READ HELP FROM FG_EB.HELP,'TCL' ELSE HELP='Sorry no help'
                NBR=DCOUNT(HELP,AM)
                FOR I=1 TO NBR; CRT HELP<I>; NEXT I
            CASE FG_ACT.CODE=FG_SEL.CODE
                POST.PROMPT=NOT(POST.PROMPT)
                CRT @(0,23):CLEOL:FG_ERROR.MSGS<100,1>:FG_ERROR.MSGS<100,2+POST.PROMPT>:
                RQM
            CASE FG_ACT.CODE=FG_TCL.CODE AND MOD(FG_STERM,3)
                CALL EB_AT_WINDOW_OPEN(PDEPTH+1,PWIDTH+1,1,1,1,'','S-Query',1)
                EXECUTE SQUERY.VERB
                CALL EB_AT_WINDOW_CLOSE(1)
            CASE UCOMMAND[1,2]='.L'
                GOSUB DISPLAY.STACK
            CASE UCOMMAND[1,2]='.X' OR COMMAND MATCHES "'.'1N0X"
                ORIG.COMMAND=UCOMMAND
                IF UCOMMAND[2,1] NE 'X' THEN COMMAND='.X':COMMAND[2,MAX]
                STMP=COMMAND[3,MAX]
                IF STMP='' THEN STMP=1
                DELIMS='.,'; DELIM=''
                I=1
                LOOP
                    LOOP
                        CHR=STMP[I,1]
                    UNTIL INDEX(DELIMS,CHR,1) DO I+=1 REPEAT
                    IF DELIM='' THEN
                        DELIM=CHR
                        DELIMS=CHR
                    END
                    NBR=STMP[1,I-1]
                    EXEC.CMD=''
                    IF NUM(NBR) THEN
                        IF LEN(NBR) AND NBR<=MAX.STACK THEN EXEC.CMD=STACK<NBR>
                    END ELSE
                        EXEC.CMD=NBR
                    END
                    IF LEN(EXEC.CMD) THEN
                        IF FIELD(EXEC.CMD,' ',1)[1,1]='"' THEN
                            CALL EB_DEBUG(EXEC.CMD)
                        END ELSE
                            IF MD_flag THEN
                                READ MD.ITEM FROM F.MD,EXEC.CMD THEN
                                    IF INDEX(MD.ITEM,'EB.INIT',1) THEN
                                        CRT 'Attempted execute of EB.INIT'
                                        EXEC.CMD=''
                                    END
                                END
                            END
                            IF LEN(EXEC.CMD) THEN
                                IF EXEC.CMD='{0S"3' THEN EXEC.CMD='IL.HELPER'
                                CRT @(0):EXEC.CMD:CLEOL
                                CMDOK=TRUE
                                READ CHECK FROM FG_PROCESSES,EXEC.CMD THEN
                                    IF CHECK<2> NE 'T' THEN CMDOK=FALSE
                                END
                                IF CMDOK THEN
                                    INCLUDE EB.OS.INCLUDES TCL.EXEC.CMD
                                END ELSE
                                    CRT EXEC.CMD:' is a registered process and cannot be run from TCL.'
                                END
                                CRT; CRT    ;! Ace. Scroll up output so it is not overwritten
                            END
                        END
                    END
                    CRT
                UNTIL CHR='' DO
                    STMP=STMP[I+1,MAX]
                    I=1
                REPEAT
                IF ORIG.COMMAND MATCH '".X"1N0N' THEN
                    IF NBR NE 1 THEN
                        INS STACK<NBR> BEFORE STACK<1>
                        WRITE STACK ON STACK.FILE,STACK.ID
                    END
                END
            CASE UCOMMAND MATCHES "'.D'0N" OR UCOMMAND MATCHES "'.D'0N'-'0N"
                GOSUB PROCESS.RANGE
                FOR CNT=START TO FINISH
                    DEL STACK<START>
                NEXT CNT
                WRITE STACK ON STACK.FILE,STACK.ID
                UCOMMAND='.L'
                GOSUB DISPLAY.STACK
            CASE FG_ACT.CODE=FG_SEARCH.CODE OR FG_ACT.CODE=FG_BCK.CODE OR UCOMMAND='.R' OR UCOMMAND MATCHES "'.R'1N0N"
                IF FG_ACT.CODE THEN
                    SEARCH.STRING=COMMAND
                    START=0
                END ELSE GOSUB PROCESS.RANGE
                LOOP
                    IF LEN((FG_ACT.CODE=FG_BCK.CODE AND UCOMMAND) AND START=0) OR FG_ACT.CODE=FG_SEARCH.CODE THEN
                        IF LEN(START=0 AND SEARCH.STRING) THEN
                            FG_ACT.CODE=FG_SEARCH.CODE
                        END ELSE
                            CRT @(0,23):FG_ERROR.MSGS<99>:' ':CLEOL:
                            CALL EB_INPUT(SEARCH.STRING,'AN',50,0,MAT EB_CHARS,MAT OTHER.PARAMS,FG_EB.PARAMS,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,1,'','','')
                        END
                        IF NOT(FG_ACT.CODE) OR FG_ACT.CODE=FG_SEARCH.CODE THEN
                            WRITE STACK ON STACK.FILE,STACK.ID
                            CALL EB_FIND_STACK(STACK.FILE,STACK.ID,SEARCH.STRING,START)
                        END
                    END
                    IF NOT(NUM(START)) THEN START=1 ELSE
                        START+=0
                        IF START<1 THEN START=1
                    END
                    COMMAND=STACK<START>; UCOMMAND=OCONV(COMMAND,'MCU')
                    CRT @(COL,PROW):CLEOP:START 'R#2 ':
                    CALL EB_INPUT(COMMAND,'AN':AM:LEN(COMMAND)+1,200,0,MAT EB_CHARS,MAT OTHER.PARAMS,FG_EB.PARAMS,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,1,'','','')
                    IF NOT(FG_ACT.CODE) THEN
                        UCOMMAND=OCONV(COMMAND,'MCU')
                        STACK<START>=TRIM(COMMAND,' ',"B")
                        WRITE STACK ON STACK.FILE,STACK.ID
                        CRT
                        GOSUB PERFORM.COMMAND
                    END ELSE
                        BEGIN CASE
                            CASE FG_ACT.CODE=FG_BCK.CODE
                                IF START<MAX.STACK THEN START+=1
                            CASE FG_ACT.CODE=FG_SKP.CODE
                                IF START>1 THEN START-=1
                            CASE FG_ACT.CODE=FG_SEARCH.CODE
                            CASE 1
                                START=''
                                FG_ACT.CODE=FALSE
                                CRT @(0,PROW):CLEOL:
                        END CASE
                    END
                WHILE FG_ACT.CODE DO REPEAT
            CASE UCOMMAND[1,2]='.R' OR UCOMMAND[1,2]='.C'
                GOSUB REPLACE.STRING        ;! Decode and execute replace command
            CASE UCOMMAND[1,2]='.A' OR UCOMMAND MATCHES "'.A'1N0N'-'1N0X"
                GOSUB PROCESS.RANGE
                DELIM=COMMAND[I,1]
                STR1=FIELD(COMMAND,DELIM,2)
                FOR NBR=FINISH TO START STEP -1 UNTIL STACK<NBR>=''
                    STACK<NBR>:=STR1
                    CRT NBR 'R#3 ':STACK<NBR>
                NEXT NBR
                WRITE STACK ON STACK.FILE,STACK.ID
            CASE UCOMMAND MATCHES "'.T'1N0N"
                NBR=COMMAND[3,LEN(COMMAND)]
                INCLUDE EB.OS.INCLUDES TCL.MERGE
                INCLUDE EB.OS.INCLUDES ED.STACK
                READ STACK FROM STACK.FILE,STACK.ID ELSE NULL
                COMMAND='.L'; UCOMMAND=COMMAND
                GOSUB DISPLAY.STACK
            CASE UCOMMAND MATCHES "'.E'0N"
                CRT
                DATA COMMAND[3,LEN(COMMAND)]
                INCLUDE EB.OS.INCLUDES ED.STACK.NOCAP
                READ STACK FROM STACK.FILE,STACK.ID ELSE NULL
                COMMAND='.L'; UCOMMAND=COMMAND
                GOSUB DISPLAY.STACK
            CASE UCOMMAND MATCHES "'.'0X" OR COMMAND=''
            CASE 1
                GOSUB PERFORM.COMMAND
        END CASE
        IF POST.PROMPT THEN
            CRT; CRT FG_ERROR.MSGS<24>:; INPUT CONT
        END
    REPEAT
    FG_ACT.CODE=FALSE
    CRT FG_CURR.COLOURS:
    IF INDEX('ZR',FG_TERM.TYPE,1) THEN FG_CRT.PAGES=''; FG_CRT.SEQUENCES=''
    IF MOD(FG_STERM,3) THEN
        CALL EB_AT_WINDOW_CLOSE(1)
    END
    RETURN
!
!----------------------------------------------------------------------
!
REPLACE.STRING: ! Decode and excute replace command
!
    GLOBAL.REPLACE=FALSE
!
!    Change .C commands to .R commands
!
    IF UCOMMAND[1,2]='.C' THEN
        COMMAND='.R':COMMAND[3,LEN(COMMAND)]
        UCOMMAND=OCONV(COMMAND,'MCU')
    END
!
!    Change .RU/bla/replace bla/ to .R/bla/replace bla/G
!
    IF UCOMMAND[1,3]='.RU' THEN
        COMMAND='.R':COMMAND[4,LEN(COMMAND)]
        GLOBAL.REPLACE=TRUE
    END
!
!    if replace is to be carried out on all commands
    IF UCOMMAND[1,3]='.RA' THEN
        S.COMM=1
        F.COMM=MAX.STACK
        DELIM=COMMAND[4,1]
    END ELSE
!       Count number of digits
        FOR DIGITS=1 TO 5 WHILE COMMAND[2+DIGITS,1] MATCHES '1N'
        NEXT DIGITS
        DIGITS-=1
        IF DIGITS=0 THEN
            COMMAND=COMMAND[1,2]:1:COMMAND[3,MAX]
            FIRST.NUM=1
            DIGITS=1
        END ELSE
            FIRST.NUM=COMMAND[3,DIGITS]
        END
        DIGIT.POS=LEN('.R')+DIGITS
        DASHPOS=DIGIT.POS+1
        IF COMMAND[DASHPOS,1]='-' THEN
            FOR DIGITS2=1 TO 5 WHILE COMMAND[DASHPOS+DIGITS2,1] MATCHES '1N'
            NEXT DIGITS2
            DIGITS2-=1
            IF DIGITS2=0 THEN
                COMMAND=COMMAND[1,DIGIT.POS]:COMMAND[DASHPOS+1,MAX]
                LAST.NUM=FIRST.NUM
            END ELSE
                LAST.NUM=COMMAND[DASHPOS+1,DIGITS2]
                COMMAND=COMMAND[1,DIGIT.POS]:COMMAND[DASHPOS+DIGITS2+1,MAX]
            END
        END ELSE
            LAST.NUM=FIRST.NUM
        END
        IF LAST.NUM < FIRST.NUM THEN
            TEMP=FIRST.NUM
            FIRST.NUM=LAST.NUM
            LAST.NUM=TEMP
        END
        IF FIRST.NUM < 1 THEN
            IF LAST.NUM=FIRST.NUM THEN LAST.NUM=1
            FIRST.NUM=1
        END
        IF LAST.NUM > MAX.STACK THEN
            IF FIRST.NUM=LAST.NUM THEN FIRST.NUM=MAX.STACK
            LAST.NUM=MAX.STACK
        END
!
!       Set start,end and delimiter variables
        S.COMM=FIRST.NUM
        F.COMM=LAST.NUM
        DELIM=COMMAND[2+DIGITS+1,1]
    END
!
!    Command must be in the following format
!    .R delim search text delim replace text delim type
!
    S.STR=FIELD(COMMAND[4,MAX],DELIM,2) ;! String to be replaced
    T.STR=TRIM(S.STR,' ',"T") ;! Trailing spaces removed
    T.LEN=LEN(T.STR)          ;! Length of truncated string
    R.STR=FIELD(COMMAND[4,MAX],DELIM,3) ;! Replacement string
    R.TYPE=FIELD(COMMAND[4,MAX],DELIM,4)          ;! Replacement type
!
!    Remove G replacement type and set global replace flag
!
    IF R.TYPE='G' THEN
        GLOBAL.REPLACE=TRUE
        R.TYPE=''
    END
!
!    Check for range
!
    IF INDEX(R.TYPE,'-',1) THEN
        START.RANGE=TRIM(FIELD(R.TYPE,'-',1))
        IF NOT(START.RANGE MATCH '1N0N' ) THEN START.RANGE=1
        END.RANGE=TRIM(FIELD(R.TYPE,'-',2))
        IF NOT(END.RANGE MATCH '1N0N') THEN END.RANGE=9999
        END.RANGE=END.RANGE-START.RANGE+1
        R.TYPE=''
    END ELSE
        START.RANGE=1 ; END.RANGE=9999
    END
!
!    Single number indicates occurrence to be replaced eg. r/x/y/3
!
    IF R.TYPE MATCH '1N0N' THEN
        OCCNO=R.TYPE ; R.TYPE=''
    END ELSE
        IF GLOBAL.REPLACE THEN OCCNO='*' ELSE OCCNO=1
    END
!
    GOSUB REPLACE.LINES
!
    WRITE STACK ON STACK.FILE,STACK.ID
!
    RETURN
!
!----------------------------------------------------------------------
!
REPLACE.LINES: !
!
!    The following variables are used by the replace code
!
!    S.COMM=First command in the stack to be replaced
!    F.COMM=Last command in the stack to be replaced
!    START.RANGE=The column to start replace command in
!    END.RANGE  =The column to end the replace command in
!    S.STR      =The string to be replaced
!    R.STR      =The replacement string
!    OCCNO      =The occurrence to replace or * for all occurences
!
!    loop to replace one or all commands
    FOR CMDNO=F.COMM TO S.COMM STEP -1
        COMMAND=STACK<CMDNO>
        IF OCCNO NE '*' THEN
!          replace specific occurence
            BPOS=INDEX(COMMAND[START.RANGE,END.RANGE],S.STR,OCCNO)
            IF BPOS NE 0 THEN
                COMMAND=COMMAND[1,BPOS-1]:R.STR:COMMAND[BPOS+LEN(S.STR),MAX]
            END ELSE
                IF T.LEN NE 0 THEN
!                check is it was append type command
                    IF COMMAND[LEN(COMMAND)+1-T.LEN,T.LEN]=T.STR THEN
                        COMMAND=COMMAND[1,LEN(COMMAND)-T.LEN]:R.STR
                    END
                END ELSE
!                Command must be .r/        / jdkfjl
                    COMMAND:=R.STR
                END
            END
        END ELSE
!          replace all occurences
            TMP.BEFORE=COMMAND[1,START.RANGE-1]
            TMP=COMMAND[START.RANGE,END.RANGE]
            TMP.AFTER=COMMAND[START.RANGE+END.RANGE,LEN(COMMAND)]
            TMP=SWAP(TMP,S.STR,R.STR)
            COMMAND=TMP.BEFORE:TMP:TMP.AFTER
        END
        COMMAND=COMMAND<1>    ;! In case attribute mark used eg. r/abc/abc^
        STACK<CMDNO>=COMMAND
    NEXT CMDNO
!
    FOR NBR=F.COMM TO S.COMM STEP -1
        CRT NBR 'R#3 ':STACK<NBR>
    NEXT NBR
    CRT
!
    RETURN
!
!----------------------------------------------------------------------
!
UPDATE.STACK: !
!
    IF NOT(ORIG.COMMAND MATCHES "'.'1N0X" OR ORIG.COMMAND MATCHES "'.X'1N0X") THEN
        IF STACK<1> NE COMMAND THEN
            LOCATE COMMAND IN STACK<am_start> SETTING START THEN DEL STACK<START>
            INS COMMAND BEFORE STACK<1>
            WRITE STACK ON STACK.FILE,STACK.ID
        END
    END
    RETURN
!
!----------------------------------------------------------------------
!
PROCESS.RANGE: !
!
    CMD=OCONV(COMMAND[2,2],'MCA')
    IF COMMAND MATCHES "'.":CMD:"'1N0N'-'1N0X" THEN
        START=FIELD(COMMAND[LEN(CMD)+2,MAX],'-',1)
        COMMAND=FIELD(COMMAND[LEN(CMD)+2,MAX],'-',2)
        IF START='' THEN START=1
        IF NOT(NUM(START)) OR START='.' OR START='-' THEN START=1
    END ELSE
        START=''
        COMMAND=COMMAND[LEN(CMD)+2,LEN(COMMAND)]
    END
    I=1
    LOOP
        INV.CHAR=COMMAND[I,1]
    WHILE NUM(INV.CHAR) AND INV.CHAR NE '' AND INV.CHAR NE '.' DO
        I+=1
    REPEAT
    FINISH=COMMAND[1,I-1]
    IF FINISH='' THEN FINISH=START
    IF FINISH='' THEN FINISH=1
    IF START='' THEN START=FINISH
    IF START>MAX.STACK THEN START=1
    IF FINISH>MAX.STACK THEN FINISH=1
!
    RETURN
!
!----------------------------------------------------------------------
!
DISPLAY.STACK: !
!
    IF UCOMMAND[3,1]='P' THEN PRINTER ON ELSE CRT @(0,1):CLEOP:
    NBR=OCONV(UCOMMAND,'MCN')
    IF NBR='' THEN NBR=20
    STMP=''
    LOOP UNTIL STACK<NBR> NE '' OR NBR=1 DO NBR-=1 REPEAT
    FOR I=NBR TO 1 STEP -1 UNTIL STMP=CTRL.X
        PRINT I 'R#3 ':STACK<I>
        IF UCOMMAND[3,1] NE 'P' THEN
            IF MOD(I,21) ELSE
                INCLUDE EB.OS.INCLUDES INPUT.TIMEOUT
                IF CHR NE CTRL.X THEN CRT @(0,1):CLEOP:
            END
        END
    NEXT I
    IF SYSTEM(1) THEN PRINTER OFF
    CRT
!
    RETURN
!
!----------------------------------------------------------------------
!
PERFORM.COMMAND: !
!
    IF FIELD(COMMAND,' ',1)[1,1]='"' THEN
        GOSUB UPDATE.STACK
        CALL EB_DEBUG(COMMAND)
        RETURN
    END
!
    IF COMMAND NE UCOMMAND THEN
        VERBNAME=FIELD(COMMAND,' ',1)
        UVERBNAME=FIELD(UCOMMAND,' ',1)
        IF MD_flag THEN
            IF FG_OSTYPE='AP' THEN
                READV CHECK FROM F.MD,UVERBNAME,1 THEN COMMAND=UCOMMAND
            END ELSE
                READV CHECK FROM F.MD,VERBNAME,1 ELSE
                    READV CHECK FROM F.MD,UVERBNAME,1 THEN COMMAND=UCOMMAND
                END
            END
        END
    END
!
!    Verify valid command
!
    IF INDEX('!\',COMMAND[1,1],1) THEN
        VALID.CMD=(COMMAND[1,1]='!')
    END ELSE
        IF FG_OSTYPE='JB' THEN VALID.CMD=TRUE ELSE
            IF MD_flag THEN
                F.VERBFILE=F.MD
                GOSUB VERIFY.CMD
                IF NOT(VALID.CMD) AND FG_OSTYPE='UDT' THEN
                    F.VERBFILE=F.CTLGTB
                    GOSUB VERIFY.CMD
                END
            END
        END
    END
    IF VALID.CMD THEN
        GOSUB UPDATE.STACK
        EXEC.CMD=COMMAND
        IF EXEC.CMD='{0S"3' THEN EXEC.CMD='IL.HELPER'
        PROCESS.ID=FIELD(EXEC.CMD,' ',1)
        READ TITLE FROM FG_PROCESSES,PROCESS.ID ELSE TITLE=COMMAND
        CHECK=TITLE<2>; TITLE=TITLE<1>
        CRT @(0):EXEC.CMD:CLEOL
        IF FG_OSTYPE='JB' OR INDEX('BT',CHECK,1) THEN
            IF CHECK NE '' AND INDEX('BT',CHECK,1) THEN
                CALL EB_CHK_SEC(PROCESS.ID,FALSE,OK)
            END ELSE OK=TRUE
            IF OK THEN
                INCLUDE EB.OS.INCLUDES TCL.EXEC.CMD
                IF LEN(CHECK) THEN GOSUB RESET.SESSION
                CRT
            END
        END ELSE
            CRT EXEC.CMD:' cannot be run from TCL.'
        END
    END ELSE
        BEGIN CASE
            CASE RNET
                GOSUB UPDATE.STACK
                CRT ESC:'\ ':COMMAND:CHAR(13)
                IF COMMAND[1,1]='\' THEN COMMAND=COMMAND[2,MAX]
!      CASE MOD(FG_STERM,3)
!        CALL AT.EXECUTE.DOS(COMMAND,'',0,0);!STKING,CAPTRING,TYPE)
            CASE 1
                GOSUB UPDATE.STACK
                CRT BELL:'[3] ':COMMAND:' is not a verb'
        END CASE
    END
!
    RETURN
!
!----------------------------------------------------------------------
!
VERIFY.CMD: !
!
    VALID.CMD=FALSE
    VERBNAME=FIELD(COMMAND,' ',1)
    READ MD.ITEM FROM F.VERBFILE,VERBNAME THEN
        IF INDEX(MD.ITEM,'EB.INIT',1) THEN
            CRT 'Attempted execute of EB.INIT'
            RETURN
        END
    END
    RCOMMAND=COMMAND[COL2(),LEN(COMMAND)]
    READV VALID.CMD FROM F.VERBFILE,VERBNAME,1 THEN
        VALID.CMD=TRUE
    END ELSE
!       Replace dashes with dots in verb
        VERBNAME=SWAP(VERBNAME,'-','.')
        READV VALID.CMD FROM F.VERBFILE,VERBNAME,1 THEN
            COMMAND=VERBNAME:RCOMMAND; VALID.CMD=1
        END ELSE
!          Replace dots with dashes in verb
            VERBNAME=SWAP(VERBNAME,'.','-')
            READV VALID.CMD FROM F.VERBFILE,VERBNAME,1 THEN
                COMMAND=VERBNAME:RCOMMAND; VALID.CMD=1
            END
        END
    END
!
    RETURN
RESET.SESSION: !
!
! This section is only called after B and T types
! so we will reset their session now
!
    LOCATE PROCESS.ID IN FG_SESSIONS<1,vm_start> BY 'AL' SETTING POS THEN
        LPOS=FG_SESSIONS<2,POS>
        DEL FG_SESSIONS<1,POS>
        DEL FG_SESSIONS<2,POS>
        LOCATE LPOS IN FG_SESSIONS<2,vm_start> SETTING POS ELSE CALL EB_RESET(LPOS)
    END
    RETURN
!
!----------------------------------------------------------------------
!
