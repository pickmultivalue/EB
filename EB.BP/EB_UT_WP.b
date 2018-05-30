    SUBROUTINE EB_UT_WP(INP.STRING,INP.FLD,LENTH,UNDERLINE.FLAG,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,INIT.FLAG,ROW,HELP.ID1,EB.CMD)
    INCLUDE EB.EQUS EB.COMMON
    INCLUDE EB.OS.INCLUDES TODAYS.DATE
    INCLUDE EB.OS.INCLUDES DATE.CONV
!
    INCLUDE EB.INCLUDES EB.STD.EQUS
    INCLUDE EB.OS.INCLUDES USER.EXITS
!
    EQU IND.CHAR TO CHAR(1)
    EQU RTN.VAL TO 13
    EQU LF.VAL TO 10
    EQU HARD.RTN TO '<'
    INCLUDE EB.OS.INCLUDES AT.MINUS.CODES
!INCLUDE jutil.h
    Timeout=300
    DEFC INT JBASEEmulateGETINT(INT, INT)
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    vm_start=IF_COMPILED_PRIME
    DIMM=BG
    BRIGHT=FG
    FG$TAG.CMD.CODE='T'
    FG$FLD=1
    EQU THE.REST TO 9999
    WPCOL=0; Sub=''; HELP.ID=''
    IF UNASSIGNED(INP.STRING) THEN INP.STRING=''  ;!SPACE(LENTH)
    IF INIT.FLAG#AM THEN INP.STRING=INIT.FLAG
    FG$OLD.FIELD=INP.STRING
!    BACK=@(-AM.BACK)          ;!CHAR(21)
!    FWD=@(-AM.FWD)  ;!CHAR(6)
    ERASE=@(-1)     ;* Used when in debug to clear protect from screen
*
*    EB.CMD contains the string to return for the following
*    1. Abort
*    2. Reverse
*    3. Choose
*    4. Update
*    5. Refresh
*    6. Help
*    7. Zoom
*
!  IF EB.CMD<1>='' THEN EB.CMD<1>='X'; * Abort cmd
!  IF EB.CMD<1>=';;' THEN EB.CMD<1>=''
!
!!#ifdef jBASE
    MATCH.SET=''
    CODES=FG$INPUT.CODES
    SUB.CODES=''
    SUB.CODE1=-1; SUB.CODE2=-1
    MAX.SUB=1
    SUB.JUST='R#1'
    CONV=0
    MAX.JUST='R#':FG$MAX.CHARS
    IF FG$MAX.CHARS>MAX.SUB THEN MAX.LEN=FG$MAX.CHARS+FG$EXPECT.CR*0 ELSE MAX.LEN=MAX.SUB
!!#endif
    MAX.JUST='R#':FG$MAX.CHARS
    MAX.LEN=FG$MAX.CHARS      ;!+FG$EXPECT.CR
    ERRS=0
    LAST.CHR=''
    WORD.WRAP=''
    PASTE.STRING=''
    OLD.POS=0
    SAVE.POS=''
    ACT.CODE=FALSE
    PASTE=FALSE
    INSERTING=FALSE
    NUM.FIELD=FALSE
    INDENT=FALSE; MARGIN=''
    SKIP.LIST=''
    RTN.CHOICES = ''
    TABS=SPACE(8)
    DO.SPELL=FALSE
! initialize Paragraph Reformat codes
    P=FG$ERROR.MSGS<46,1>
    D=FG$ERROR.MSGS<46,3>
!
! Display "WP Mode"
!
    WORD.PROCESSING=FALSE
    IF INP.FLD='' THEN INP.FLD='AN'
    TYPE=INP.FLD<1>
    JUST=INP.FLD<2>
    NUM.FIELD=INDEX('NM',TYPE[1,1],1)
    DFLD=0
    FG$ACT.CODE=''
    CHR1=''
    EXIT.CODE=FG$END.CODE
!
! Get length from SCR.PARAMS ?
!
    IF LENTH THEN
        RTN.REQUIRED = TRUE
    END ELSE
        RTN.REQUIRED = FALSE
        LENTH = 1
    END
    MAXLENTH = LENTH<2>
    LENTH = LENTH<1>
    IF NOT(MAXLENTH) THEN MAXLENTH = LENTH
    CLEAR.FIELD=SPACE(LENTH)
    DOTS=STR('_',LENTH)
    LAST.WPCOL=WPCOL+LENTH
    IF LAST.WPCOL>79 THEN
        LAST.POS=@(LAST.WPCOL-80,ROW+1)
    END ELSE LAST.POS=@(LAST.WPCOL,ROW)
    IF INP.FLD<3>#'' THEN INP.POS=INP.FLD<3> ELSE INP.POS=1
    CC=INP.FLD<4,1>
    RR=INP.FLD<4,2>
    REPAINT=INP.FLD<5>
    HELP.TEXT=INP.FLD<20>
    HVMC = DCOUNT(HELP.TEXT, @VM)
    ORIG.COL=WPCOL
    WPCOL+=INP.POS-1
    PREV.CHARS=''
    WPCHR.NBR=0     ;!FIRST.ASCII
    RTN.KEY=FALSE
    NORMAL.FIELD=(TYPE#'LIT' AND TYPE#'MENU' AND TYPE#'ACT')
    SECRET=(TYPE='HD')
    HIDDEN=(TYPE[1,1]='H' AND NOT(SECRET))
    IF LEN(JUST) = 0 THEN
        BEGIN CASE
            CASE TYPE='D'
                JUST='D'
            CASE TYPE='T'
                JUST='MTS'
            CASE 1
                JUST="L#":LENTH
        END CASE
    END
    GOSUB STMP.OCONV
    STMP=TRIM(STMP,' ',"T")
    TRAIL=LENTH-LEN(STMP)
    INCLUDE EB.OS.INCLUDES PC.OFF.CURSOR
    INP.STRING=STMP
    XX=LEN(STMP)
    CHOICES=RAISE(EB.CMD<8>)
    F_CHOICES = FIELD(CHOICES, CHAR(6), 1)
    IF LEN(F_CHOICES) # LEN(CHOICES) THEN
        CHOICES = CHOICES[COL2()+1, LEN(CHOICES)]
    END
    BEGIN CASE
        CASE HIDDEN
        CASE SECRET
            CRT STR('*',XX): ; SEC.STRING=INP.STRING
            INP.STRING=STR('*',LEN(INP.STRING))
        CASE 1
            CRT STMP:
            IF TRAIL < 80 THEN
                IF UNDERLINE.FLAG THEN
                    CRT BG:DOTS[1,TRAIL]:FG:
                END ELSE CRT SPACE(TRAIL):
                XX+=TRAIL
            END
    END CASE
    XX+=(1-INP.POS)
    IF XX > LENTH THEN XX=0   ;* ace Somehow XX gets set to 65373
    CRT STR(BACK,XX):
    TRAIL=0
    INCLUDE EB.OS.INCLUDES PC.RESET.CURSOR
!
! Start of input logic
!
STARTLBL: !
    LOOP
        LAST.NBR=WPCHR.NBR
        IF NOT(RTN.REQUIRED) AND LAST.NBR THEN
            WPCHR.NBR = RTN.VAL
        END ELSE
            FG$ACT.CODE=FALSE
            CALL EB_GET_INPUT(WPCHR, WPCHR.NBR)
            IF FG$TIMEDOUT THEN FG$ACT.CODE=FG$ABT.CODE
        END
!
! Was the <RETURN> or <Line-Feed> key used ?
!
PROCESS.RTN: !
        IF WPCHR.NBR=RTN.VAL THEN
            RTN.KEY=TRUE
            IF NOT(WORD.PROCESSING OR TYPE='MENU' OR TYPE='LIT') THEN
                IF TRIM(INP.STRING)#'' AND TYPE[1,1]#'A' AND NOT(HIDDEN OR SECRET) THEN GOSUB VALID.INPUT.CHECK
            END
        END
    UNTIL RTN.KEY AND NOT(WORD.PROCESSING) DO
        OLD.POS=INP.POS
        Sub=0
!
! Has a valid GALA function been keyed in
!
        IF NOT(FG$ACT.CODE) THEN
            STMP=PREV.CHARS:WPCHR
            IF (OCONV(STMP,'MCP')=STMP AND (OCONV(STMP,'MCA')=STMP OR OCONV(STMP,'MCN')=STMP)) OR TRIM(STMP)='' ELSE
                IF FG$MAX.CHARS<3 AND OCONV(LAST.CHR,'MCA')#'' AND PREV.CHARS='' THEN IF WPCHR.NBR>=FIRST.ALPHA AND WPCHR.NBR<=LAST.ALPHA THEN GO ASCII.START
                PREV.CHARS=PREV.CHARS:WPCHR
                NBR.CHARS=LEN(PREV.CHARS)
                IF NBR.CHARS>FG$MAX.CHARS THEN NBR.CHARS=FG$MAX.CHARS
                FOR TRAP=1 TO NBR.CHARS UNTIL FG$ACT.CODE
                    CHR1=PREV.CHARS[1,TRAP]
                    BEGIN CASE
                        CASE NBR.CHARS=1
                        CASE COUNT(FG$INPUT.CODES,CHR1)=1
                        CASE COUNT(FG$HOT.KEYS,CHR1)=1
                        CASE 1; CHR1=''
                    END CASE
                    IF CHR1#'' THEN
                        INCLUDE EB.OS.INCLUDES CHECK.FOR.CMD
                    END
                NEXT TRAP
                IF FG$ACT.CODE ELSE
                    FOR TRAP=2 TO NBR.CHARS UNTIL FG$ACT.CODE
                        CHR1=PREV.CHARS[TRAP,FG$MAX.CHARS]
                        INCLUDE EB.OS.INCLUDES CHECK.FOR.CMD
                    NEXT TRAP
                END
            END
        END
!
! Ignore certain functions if Word Processing
!
        SUB.CODE=FALSE
        IF FG$ACT.CODE THEN
            IF FG$TYPEAHEAD.BUFF='' AND PREV.CHARS#'' THEN
                FG$TYPEAHEAD.BUFF=PREV.CHARS[LEN(CHR1)+1,THE.REST]
            END
            BEGIN CASE
                CASE FG$ACT.CODE=FG$LEFT.CODE
                    SUB.CODE=FG$LEFT.CODE
                    FG$ACT.CODE=FALSE
                CASE FG$ACT.CODE=FG$RIGHT.CODE
                    SUB.CODE=FG$RIGHT.CODE
                    FG$ACT.CODE=FALSE
                CASE FG$ACT.CODE=FG$ABT.CODE
                    RTN.STRING=EB.CMD<1>
                    IF LEN(RTN.STRING) THEN INP.STRING = RTN.STRING
                CASE FG$ACT.CODE=FG$BCK.CODE
                    RTN.STRING=EB.CMD<2>
                    IF LEN(RTN.STRING) THEN INP.STRING = RTN.STRING
                CASE FG$ACT.CODE=FG$EXIT.LN.CODE
                    RTN.STRING=EB.CMD<3>
                    IF LEN(RTN.STRING) THEN INP.STRING = RTN.STRING
                CASE FG$ACT.CODE=FG$SKP.CODE
                CASE FG$ACT.CODE=FG$SEL.CODE
                CASE FG$ACT.CODE=FG$SEARCH.CODE
                CASE FG$ACT.CODE=FG$NXT.KEY.CODE
                CASE FG$ACT.CODE=FG$PRV.KEY.CODE
                CASE FG$ACT.CODE=FG$INS.LINE.CODE
                CASE FG$ACT.CODE=FG$DEL.LINE.CODE
                CASE FG$ACT.CODE=FG$PRVS.CODE
                CASE FG$ACT.CODE=FG$NXTS.CODE
                CASE FG$ACT.CODE=FG$MOUSE.CODE
                    CALL EB_GETMOUSE(FG$TYPEAHEAD.BUFF, EVENT, C, R)
                    FG$ACT.CODE=FALSE
                CASE FG$ACT.CODE=FG$OPT.CODE
                    IF LEN(CHOICES) OR LEN(F_CHOICES) THEN
                        IF CHOICES = '?' THEN
                            RTN.STRING=CHOICES
                            INP.STRING = RTN.STRING
                        END ELSE
                            FLD.NBRS=EB.CMD<9>
                            ATTRS=EB.CMD<10>
                            JUSTS=EB.CMD<11>
                            HEADER=EB.CMD<12>
                            POSITION=EB.CMD<13>
                            IF LEN(POSITION)=0 AND LEN(CC) THEN POSITION = CC:@VM:RR+1
                            CALL EB_CHOICES(POSITION<1,1>,POSITION<1,2>,'','',F_CHOICES,CHOICES,INP.STRING,FLD.NBRS,'',ATTRS,JUSTS,HEADER)
                            CRT REPAINT:
                            IF LEN(INP.STRING) THEN
                                RTN.CHOICES = DELETE(INP.STRING, 1)
                                INP.STRING = INP.STRING<1>
                                RTN.STRING = INP.STRING
                                FG$ACT.CODE = TRUE
                            END ELSE
                                INP.STRING = FG$OLD.FIELD
                                FG$ACT.CODE = FALSE
                            END
                            IF LEN(CC) THEN
                                CRT @(CC+INP.POS-1,RR):
                                GOSUB CRT.UNDERLINE
                            END
                            IF FG$ACT.CODE THEN
                                FG$ACT.CODE = FALSE
                                BREAK
                            END
                        END
                    END
                CASE FG$ACT.CODE=FG$END.CODE
                    RTN.STRING=EB.CMD<4>
                    IF LEN(RTN.STRING) THEN INP.STRING = RTN.STRING
! TAB
                CASE FG$ACT.CODE=FG$TAB.CODE          ;!OR INDEX(PREV.CHARS,FG$TAB.CH,1)
                    IF TYPE='MENU' ELSE
                        WPCHR.NBR=RTN.VAL
                        GOTO PROCESS.RTN
                    END
! Back TAB
                CASE FG$ACT.CODE=FG$BTAB.CODE OR INDEX(PREV.CHARS,FG$BTAB.CH,1) AND LEN(FG$BTAB.CH)
                    IF TYPE='MENU' ELSE
                        GOTO FINISH
                    END
                CASE FG$ACT.CODE=FG$HLP.CODE
                    IF LEN(HELP.TEXT) THEN
                        CRT @(-1):
                        FOR H = 1 TO HVMC
                            CRT HELP.TEXT<1, H>
                        NEXT H
                        CRT
                        CRT 'Press any key to continue...':
                        INPUT X,0:
                        IF LEN(REPAINT) THEN
                            CRT REPAINT:
                            FG$ACT.CODE = FALSE
                        END
                    END
                CASE FG$ACT.CODE=FG$JMP.CODE
                    IF HELP.ID#'' THEN
                        CALL EB_BASIC_ZOOM(HELP.ID)
                        RTN.STRING=EB.CMD<5>          ;* refresh
                    END ELSE
                        RTN.STRING=EB.CMD<7>          ;* Zoom
                    END
                    IF LEN(RTN.STRING) THEN INP.STRING = RTN.STRING
                CASE FG$ACT.CODE=FG$RFR.CODE
                    RTN.STRING=EB.CMD<5>
                    IF LEN(RTN.STRING) THEN INP.STRING = RTN.STRING
                CASE 1
                    WPCHR.NBR=0
                    SUB.CODE=FG$ACT.CODE
                    FG$ACT.CODE=FALSE
                    PREV.CHARS=''
            END CASE
        END
        IF FG$ACT.CODE THEN
            IF FG$ACT.CODE=FG$HLP.CODE THEN
                IF HELP.ID#'' THEN
                    CALL EB_UT_HELP(HELP.ID,FG$EB.PARAMS,TERM.TYPE)
                    FG$ACT.CODE=FALSE
                    GOTO STARTLBL
                END ELSE
                    RTN.STRING=EB.CMD<6>          ;* Help key
                    IF LEN(RTN.STRING) THEN INP.STRING = RTN.STRING
                END
            END     ;! ELSE
            LAST.CHR=''
            IF JUST[1,2]='MT' THEN INP.STRING=ICONV(OCONV(INP.STRING,'MCU'),JUST)
            GO FINISH
!            END
        END
        Sub=0
        IF WPCHR.NBR>=FIRST.ASCII AND WPCHR.NBR<=LAST.ASCII AND LAST.NBR>=FIRST.ALPHA AND LAST.NBR<=LAST.ALPHA AND PREV.CHARS='' THEN
!
ASCII.START: !
            LAST.CHR=CHAR(LAST.NBR)
            IF LAST.CHR=OCONV(LAST.CHR,'MCA') THEN
ASCII.INPUT:    !
                IF TYPE = 'U' THEN
                    WPCHR = OCONV(WPCHR, 'MCU')
                END ELSE
                    IF TYPE='L' THEN WPCHR=OCONV(WPCHR,'MCL')
                END
                IF WPCOL<LAST.WPCOL AND NOT(HIDDEN) THEN
                    IF SECRET THEN CRT "*": ELSE CRT WPCHR:
                END
!
! Word wrap
!
                IF INSERTING THEN
                    IF TRIM(INP.STRING[MAXLENTH,1])#'' OR INP.POS-1=MAXLENTH THEN
                        CRT BELL:
                    END ELSE
                        INP.STRING=(INP.STRING[1,INP.POS-1]:WPCHR:INP.STRING[INP.POS,LENTH])
                        WPCOL+=1
                        INP.POS+=1
                        DUMMY=TRIM(INP.STRING[INP.POS,MAXLENTH],' ',"T")
                        IF DUMMY#'' THEN GOSUB CRT.UNDERLINE
                    END
                END ELSE
                    IF INP.POS-1=MAXLENTH THEN
                        CRT BELL:
                    END ELSE
                        IF INP.POS=1 AND TRIM(INP.STRING)#'' THEN
                            IF WORD.PROCESSING OR LAST.CHR=FG$BS.CH ELSE
                                INP.POS=2
                                WPCOL+=1
                                GOSUB 10          ;! cleol
                                INP.POS=1
                                WPCOL-=1
                            END
                        END
                        INP.STRING=(INP.STRING[1,INP.POS-1]:WPCHR:INP.STRING[INP.POS+1,MAXLENTH])
                        WPCOL+=1
                        INP.POS+=1
                    END
                END
                IF PASTE THEN
                    IF INP.POS<=PASTE THEN
                        IF NOT(INSERTING) THEN
                            ST=OLD.POS; FI=INP.POS-1; GOSUB FOREGROUND
                        END ELSE
                            PASTE+=1
                            ST=INP.POS+1; FI=PASTE-1; GOSUB BACKGROUND
                        END
                    END ELSE
                        ST=OLD.POS; FI=OLD.POS; GOSUB BACKGROUND
                    END
                END
                LAST.CHR=WPCHR
                GO STARTLBL
            END
        END
        BEGIN CASE
! <ctrl>E - End of Line
            CASE SUB.CODE=FG$EOL.CODE
                Sub=3
! <ctrl>X - Start of Line
            CASE SUB.CODE=FG$SOL.CODE
                Sub=4
! INS char
            CASE SUB.CODE=FG$INS.CODE
                IF INDEX(' ',INP.STRING[MAXLENTH,1],1) THEN Sub=5 ELSE CRT BELL:
! Set INS
            CASE SUB.CODE=FG$INSERT.CODE
                Sub=6
! BackSpace or <==
            CASE WPCHR.NBR=SEQ(FG$BS.CH) OR SUB.CODE=FG$LEFT.CODE
                IF INP.POS>1 THEN
                    Sub=7
                END ELSE
                    IF TYPE='MENU' THEN FG$ACT.CODE=FG$LEFT.CODE; RETURN
                END
! ==>
            CASE SUB.CODE=FG$RIGHT.CODE
                IF INP.POS<MAXLENTH THEN
                    Sub=23
                END ELSE
                    CRT BELL:
                END
! DEL char
            CASE SUB.CODE=FG$DEL.CHAR.CODE
                DUMMY=TRIM(INP.STRING[INP.POS,MAXLENTH],' ',"T")
                IF DUMMY#'' THEN Sub=8
! DEL
            CASE SUB.CODE=FG$DEL.WORD.CODE
                Sub=9
! DEL line
            CASE SUB.CODE=FG$DEL.LINE.CODE
                Sub=10
! <ctrl>DEL (undelete)
            CASE SUB.CODE=FG$UNDEL.CODE
                Sub=11
! <ctrl>D back a word
            CASE SUB.CODE=FG$BWORD.CODE AND INP.POS>1
                Sub=12
! <ctrl> next word
            CASE SUB.CODE=FG$FWORD.CODE
                Sub=14
! => (skip)
            CASE SUB.CODE=FG$SKP.CODE
                IF INP.POS<=MAXLENTH THEN
                    IF TYPE='MENU' AND INP.POS=1 THEN FG$ACT.CODE=FG$RIGHT.CODE; RETURN
                    Sub=15
                END
! <ctrl>C Change case of word
            CASE SUB.CODE=FG$CASE.CODE OR SUB.CODE=FG$L.CASE.CODE
                Sub=18
! <ctrl>] Cut
            CASE SUB.CODE=FG$CUT.CODE
                Sub=20
! <ctrl>P Paste
            CASE SUB.CODE=FG$PASTE.CODE
                Sub=21
            CASE RTN.KEY
                RTN.KEY=FALSE
                INDENT=FALSE; MARGIN=''
                DUMMY=TRIM(INP.STRING[INP.POS,MAXLENTH],' ',"T")
                IF DUMMY='' THEN  ;! end-of-paragraph
                    INP.STRING=TRIM(INP.STRING,' ',"T")
                    IF NOT(INDEX(INP.STRING,NULL.CHAR,1)) THEN INP.STRING:=NULL.CHAR
                END
                PREV.CHARS=FG$DOWN.CH
                INP.POS=1
                WPCOL=ORIG.COL
                Sub=13
!/|\ |
! | (up-arrow) and \|/ (down arrow)
            CASE WORD.PROCESSING AND SUB.CODE=FG$BCK.CODE OR SUB.CODE=FG$SKP.CODE OR SUB.CODE=FG$TOP.CODE
                Sub=13
! <ctrl>O Multi-function facility
! or
! INS Line
            CASE TYPE='MENU' AND SUB.CODE=FG$MULTI.CODE
                FG$ACT.CODE=FG$TAG.CMD.CODE; RETURN
            CASE SUB.CODE=FG$PRVS.CODE OR SUB.CODE=FG$NXTS.CODE
                IF NOT(NORMAL.FIELD) THEN
                    IF SUB.CODE=FG$NXTS.CODE THEN
                        IF INP.FLD='ACT' THEN FG$ACT.CODE=FG$SKP.CODE ELSE FG$ACT.CODE=FG$NXTS.CODE
                    END ELSE
                        IF INP.FLD='ACT' THEN FG$ACT.CODE=FG$BCK.CODE ELSE FG$ACT.CODE=FG$PRVS.CODE
                    END
                    GO FINISH
                END
            CASE WPCHR.NBR>=FIRST.ASCII AND WPCHR.NBR<=LAST.ASCII AND INP.POS-1<=MAXLENTH
                GO ASCII.INPUT
        END CASE
        IF Sub THEN
            ON Sub GOSUB 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,1,1,16
            PREV.CHARS=''
            IF Sub>2 THEN WPCHR=''
        END
        LAST.CHR=WPCHR
    REPEAT
FINISH: !
    IF INP.POS=2 AND TRIM(INP.STRING)='' THEN INP.STRING=' ' ELSE
        IF TYPE#'LIT' THEN INP.STRING=TRIM(INP.STRING,' ',"T")
    END
    IF HIDDEN ELSE
        WPCOL=ORIG.COL
        INCLUDE EB.OS.INCLUDES PC.OFF.CURSOR
        IF NORMAL.FIELD AND INP.STRING#'' THEN
            IF INDEX(INP.STRING,NULL.CHAR,1) THEN CONVERT NULL.CHAR TO HARD.RTN IN INP.STRING
            IF SECRET THEN
                STMP=STR('*',LEN(INP.STRING))
            END ELSE
                BEGIN CASE
                    CASE TYPE='T'
!                        INP.STRING=ICONV(INP.STRING,JUST)
                    CASE TYPE='YN'
                        INP.STRING=(OCONV(INP.STRING, 'MCU') = 'Y')
                END CASE
                GOSUB STMP.OCONV
            END
            CRT STR(BACK,INP.POS-1):STMP:
            GOSUB STMP.ICONV
        END ELSE
            IF UNDERLINE.FLAG THEN CRT STR(BACK,INP.POS-1):CLEAR.FIELD:
        END
        CRT ' ':    ;! weird pixel bug
        INCLUDE EB.OS.INCLUDES PC.BLOCK.CURSOR
    END
    ECHO ON
    IF LEN(RTN.CHOICES) THEN INP.STRING<2> = RTN.CHOICES
    RETURN
VALID.INPUT.CHECK: !
    INP.STRING=TRIM(INP.STRING)
    ERRMSG=''
    BEGIN CASE
        CASE TYPE[1,1]='D'
            BEGIN CASE
                CASE INP.STRING='.'; STMP=TODAYS$DATE
                CASE NUM(INP.STRING) AND LEN(INP.STRING)<3
                    STMP=INP.STRING:' ':OCONV(TODAYS$DATE,DMA.CONV)
                    STMP=ICONV(STMP,'D')
                CASE 1
                    STMP=ICONV(INP.STRING,'D')
            END CASE
            IF STMP#'' THEN
                INP.STRING=STMP
            END ELSE
                CRT BELL:
                RTN.KEY=FALSE
                RETURN
            END
        CASE TYPE='T'
            INP.STRING=OCONV(INP.STRING,'MCU')
            IF INDEX(INP.STRING,':',1) THEN
                STMP=ICONV(INP.STRING,'MTS')
            END ELSE
                STMP=OCONV(INP.STRING,'MCN')
                IF LEN(STMP)>4 THEN
                    STMP=STMP+(120000*(INDEX(INP.STRING,'P',1)>0))
                    HASH='R%%:%%:%%'
                END ELSE
                    IF STMP<99 THEN STMP=STMP*100
                    STMP=STMP+(1200*(INDEX(INP.STRING,'P',1)>0))
                    HASH='R%%:%%'
                END
                STMP=STMP HASH
                BEGIN CASE
                    CASE INP.STRING='.'; STMP=CURR$TIME
                    CASE NUM(INP.STRING) AND LEN(INP.STRING)<3; STMP=ICONV(INP.STRING:':00','MT')
                    CASE 1
                        STMP=ICONV(STMP,'MTS')
                END CASE
            END
            IF STMP#'' THEN INP.STRING=STMP ELSE ERRMSG=FG$ERROR.MSGS<79>
        CASE NUM.FIELD
            IF INP.STRING[1,1]='$' THEN INP.STRING=INP.STRING[2,MAXLENTH]
            IF NUM(INP.STRING) THEN
                IF TYPE[1,1]='M' THEN STMP=2 ELSE STMP=TYPE[2,2]+0
                IF STMP THEN
                    IF INP.STRING MATCHES "1N0N" THEN STMP=1
                END ELSE
                    IF INP.STRING=INP.STRING STMP THEN STMP=1 ELSE STMP=0
                END
                IF NOT(STMP) THEN
                    ERRMSG=STMP:FG$ERROR.MSGS<78>
                END
            END ELSE ERRMSG=FG$ERROR.MSGS<76>
        CASE TYPE='A'
            STMP=OCONV(INP.STRING,'MCA')
            IF STMP#INP.STRING THEN ERRMSG=FG$ERROR.MSGS<41>
        CASE TYPE='U'
            INP.STRING=OCONV(INP.STRING,'MCU')
        CASE TYPE='L'
            INP.STRING=OCONV(INP.STRING,'MCL')
        CASE TYPE='LC'
            INP.STRING=OCONV(INP.STRING,'MCT')
        CASE TYPE='YN'
            STMP = INP.STRING
            GOSUB STMP.ICONV
            GOSUB STMP.OCONV
            INP.STRING = STMP
    END CASE
    IF LEN(CHOICES) AND LEN(F_CHOICES) = 0 THEN
        LOCATE INP.STRING IN CHOICES<1> SETTING CPOS ELSE
            CRT BELL:
            RTN.KEY=FALSE
            RETURN
        END
    END
    IF ERRMSG#'' THEN
        RTN.KEY=FALSE
        INP.STRING=FG$OLD.FIELD
        INP.POS=1; WPCOL=ORIG.COL
        CRT @(0,23):ERRMSG:
    END
    RETURN
CRT.UNDERLINE: !
    IF HIDDEN THEN RETURN
    INCLUDE EB.OS.INCLUDES PC.OFF.CURSOR
    STMP=TRIM(INP.STRING[INP.POS,MAXLENTH],' ',"T")
    XX=LEN(STMP)
    IF SECRET THEN CRT STR('*',XX): ELSE CRT STMP:
    IF TRAIL THEN
        IF UNDERLINE.FLAG THEN CRT BG:DOTS[1,TRAIL]:FG: ELSE CRT SPACE(TRAIL):
    END
    XX+=TRAIL
    CRT STR(BACK,XX):
    TRAIL=0
    INCLUDE EB.OS.INCLUDES PC.RESET.CURSOR
    RETURN
!
CRT.LINE: !
    IF HIDDEN THEN RETURN
    INCLUDE EB.OS.INCLUDES PC.OFF.CURSOR
    IF JUST[1,2]#'MT' THEN
        IF SECRET THEN STMP=STR('*',LEN(INP.STRING)) ELSE STMP=INP.STRING
        IF INDEX(STMP,NULL.CHAR,1) THEN CONVERT NULL.CHAR TO HARD.RTN IN STMP
        CRT STR(BACK,INP.POS-1):STMP JUST:
    END ELSE CRT STR(BACK,INP.POS-1):OCONV(INP.STRING,JUST):
    INCLUDE EB.OS.INCLUDES PC.RESET.CURSOR
    RETURN
NEXT.WORD:!
    IF TRIM(INP.STRING[INP.POS,MAXLENTH])#'' THEN
        WPCOL-=INP.POS
        LOOP
            THIS.CHR=INP.STRING[INP.POS,1]
        UNTIL OCONV(THIS.CHR,'MCN')='' AND OCONV(THIS.CHR,'MCA')='' OR THIS.CHR=' ' OR INP.POS>=MAXLENTH DO
            INP.POS+=1
        REPEAT
        WPCOL+=INP.POS
        THIS.CHR=TRIM(INP.STRING[INP.POS,MAXLENTH])
        IF THIS.CHR#'' THEN
            WPCOL-=INP.POS
            LOOP
                THIS.CHR=INP.STRING[INP.POS,1]
            UNTIL OCONV(THIS.CHR,'MCN')#'' OR OCONV(THIS.CHR,'MCA')#'' OR TRIM(INP.STRING[INP.POS,MAXLENTH])='' OR INP.POS>=MAXLENTH DO
                INP.POS+=1
            REPEAT
            WPCOL+=INP.POS
        END
    END
    RETURN
FIND.WORD:!
    WPCOL-=INP.POS
    LOOP
        THIS.CHR=INP.STRING[INP.POS,1]
    UNTIL OCONV(THIS.CHR,'MCN')#'' OR OCONV(THIS.CHR,'MCA')#'' OR INP.POS=1 DO
        INP.POS-=1
    REPEAT
    WPCOL+=INP.POS
GET.WORD: !
    WPCOL-=INP.POS
    LOOP
        THIS.CHR=INP.STRING[INP.POS,1]
    UNTIL OCONV(THIS.CHR,'MCN')='' AND OCONV(THIS.CHR,'MCA')='' OR INP.POS=1 DO
        INP.POS-=1
    REPEAT
    IF OCONV(THIS.CHR,'MCN')='' AND OCONV(THIS.CHR,'MCA')='' THEN
        INP.POS+=1
    END
    WPCOL+=INP.POS
    NEW.POS=INP.POS+1
    LOOP
        THIS.CHR=INP.STRING[NEW.POS,1]
    UNTIL THIS.CHR=' ' OR (OCONV(THIS.CHR,'MCN')='' AND OCONV(THIS.CHR,'MCA')='' AND THIS.CHR#"'") OR NEW.POS>MAXLENTH DO
        NEW.POS+=1
    REPEAT
    WORD.LENGTH=NEW.POS-INP.POS
    WORD=INP.STRING[INP.POS,WORD.LENGTH]
    RETURN
1   !
    ACT.CODE=FALSE
    ACTION=LAST.CHR
    RETURN
2   !
    ACT.CODE=TRUE
    IF 0 THEN
        IF INP.POS=1 THEN WORD.WRAP=INP.STRING[1,1]
        INP.STRING=(INP.STRING[1,INP.POS-1]:WPCHR:INP.STRING[INP.POS+1,MAXLENTH])
        WPCOL+=1
        INP.POS+=1
    END
    RETURN
3   !
    DUMMY=TRIM(INP.STRING,' ',"T")
    CRT STR(FWD,LEN(DUMMY)-INP.POS+1):
    INP.POS=LEN(DUMMY)+1
    WPCOL=ORIG.COL+INP.POS-1
    IF PASTE THEN
        IF OLD.POS<=PASTE THEN ST=OLD.POS; FI=PASTE; GOSUB FOREGROUND
        IF INP.POS>PASTE THEN ST=PASTE; FI=INP.POS-1; GOSUB BACKGROUND
    END
    RETURN
4   !
    CRT STR(BACK,INP.POS-1):
    OLD.POS=INP.POS
    WPCOL=ORIG.COL
    INP.POS=1
    IF PASTE THEN
        IF OLD.POS>PASTE THEN ST=PASTE; FI=OLD.POS; GOSUB FOREGROUND
        IF INP.POS<PASTE THEN ST=INP.POS; FI=PASTE-1; GOSUB BACKGROUND
    END
    RETURN
5   !
    WPCHR=' '
    INP.STRING=INP.STRING[1,INP.POS-1]:' ':INP.STRING[INP.POS,MAXLENTH]
    GOSUB CRT.UNDERLINE
    IF INP.POS<=PASTE THEN
        ST=INP.POS; FI=PASTE; GOSUB BACKGROUND
        PASTE+=1
    END
    RETURN
6   !
    IF INSERTING THEN
        INSERTING=FALSE
        INCLUDE EB.OS.INCLUDES PC.BLOCK.CURSOR
    END ELSE
        INSERTING=TRUE
        INCLUDE EB.OS.INCLUDES PC.LINE.CURSOR
    END
    RETURN
7   !
    WPCOL-=1
    INP.POS-=1
    CRT BACK:
    IF WPCHR.NBR=SEQ(FG$BS.CH) THEN
        CRT SPC:BACK:
        TRAIL=1
        INP.STRING=INP.STRING[1,INP.POS-1]:INP.STRING[INP.POS+1,MAXLENTH]
        GOSUB CRT.UNDERLINE
    END
    IF NOT(INSERTING) THEN
        IF PASTE THEN
            IF OLD.POS>PASTE THEN
                ST=INP.POS; FI=INP.POS; GOSUB FOREGROUND
            END ELSE
                ST=INP.POS; FI=INP.POS; GOSUB BACKGROUND
            END
        END
    END
    RETURN
8   !
    FG$DELETE.LIST=INP.STRING[INP.POS,1]:AM:FG$DELETE.LIST
DEL.LABEL:!
    INP.STRING=(INP.STRING[1,INP.POS-1]:INP.STRING[INP.POS+1,MAXLENTH])[1,MAXLENTH]
    IF UNDERLINE.FLAG ELSE INP.STRING:=' '
    TRAIL=1
    GOSUB CRT.UNDERLINE
    IF INP.POS<=PASTE THEN
        PASTE-=1
        ST=INP.POS; FI=PASTE-1; GOSUB BACKGROUND
    END
    RETURN
9   !
    THIS.CHR=INP.STRING[INP.POS,1]
    IF OCONV(THIS.CHR,'MCA')#'' OR OCONV(THIS.CHR,'MCN')#'' THEN
        GOSUB GET.WORD
    END ELSE
        NEW.POS=INP.POS+1
        WORD=THIS.CHR
        WORD.LENGTH=1
    END
    NEXT.CHAR=INP.STRING[NEW.POS,1]
    IF INDEX(' ',NEXT.CHAR,1) THEN
        NEW.POS+=1
    END ELSE
        NEXT.CHAR=''
    END
    FG$DELETE.LIST=WORD:NEXT.CHAR:AM:FG$DELETE.LIST
    INP.STRING=(INP.STRING[1,INP.POS-1]:INP.STRING[NEW.POS,MAXLENTH])    ;! JUST
    TRAIL=WORD.LENGTH+1
    IF UNDERLINE.FLAG ELSE INP.STRING:=SPACE(TRAIL)
    GOSUB CRT.UNDERLINE
    IF OLD.POS<=PASTE THEN
        PASTE-=WORD.LENGTH
        ST=INP.POS; FI=PASTE-1; GOSUB BACKGROUND
    END
    RETURN
10  !
    TRAIL=TRIM(INP.STRING[INP.POS,MAXLENTH],' ',"T")
    FG$DELETE.LIST=INP.STRING[INP.POS,MAXLENTH]:AM:FG$DELETE.LIST
    TRAIL=LEN(TRAIL)
    IF UNDERLINE.FLAG ELSE CRT SPACE(TRAIL):STR(BACK,TRAIL):
    INP.STRING=INP.STRING[1,INP.POS-1]
    GOSUB CRT.UNDERLINE
    RETURN
11  !
    PASTE.STRING=FG$DELETE.LIST<1>
    DEL FG$DELETE.LIST<1>
    GOSUB INSERT.STRING
    RETURN
12  !
    INP.POS-=1
    WPCOL-=1-INP.POS
    LOOP
        THIS.CHR=INP.STRING[INP.POS,1]
    UNTIL OCONV(THIS.CHR,'MCN')#'' OR OCONV(THIS.CHR,'MCA')#'' OR INP.POS=1 DO
        INP.POS-=1
    REPEAT
    LOOP
        THIS.CHR=INP.STRING[INP.POS,1]
    UNTIL OCONV(THIS.CHR,'MCN')='' AND OCONV(THIS.CHR,'MCA')='' OR INP.POS=1 DO
        INP.POS-=1
    REPEAT
    IF OCONV(THIS.CHR,'MCN')='' AND OCONV(THIS.CHR,'MCA')='' THEN
        INP.POS+=1
    END
    WPCOL+=INP.POS
    CRT STR(BACK,OLD.POS-INP.POS):
    IF PASTE THEN
        IF INP.POS>PASTE THEN
            ST=INP.POS; FI=OLD.POS; GOSUB FOREGROUND
        END ELSE
            IF OLD.POS>PASTE THEN ST=PASTE; FI=OLD.POS; GOSUB FOREGROUND
            ST=INP.POS; FI=PASTE-1; GOSUB BACKGROUND
        END
    END
    RETURN
13  !
    RETURN
14  !
    GOSUB NEXT.WORD
    CRT STR(FWD,INP.POS-OLD.POS):
    IF PASTE THEN
        IF OLD.POS<=PASTE THEN
            ST=OLD.POS; FI=PASTE; GOSUB FOREGROUND
        END
        IF INP.POS>PASTE THEN
            ST=PASTE; FI=INP.POS-1; GOSUB BACKGROUND
        END ELSE
            ST=INP.POS; FI=PASTE-1; GOSUB BACKGROUND
        END
    END
    RETURN
15  !
    WPCOL+=1
    INP.POS+=1
    CRT FWD:
    IF PASTE THEN
        IF INP.POS<=PASTE THEN
            ST=OLD.POS; FI=OLD.POS; GOSUB FOREGROUND
        END ELSE
            ST=OLD.POS; FI=OLD.POS; GOSUB BACKGROUND
        END
    END
    RETURN
16  !
    IF INP.POS+8<=MAXLENTH THEN
        WPCOL+=8
        INP.POS+=8
    END ELSE
        INP.POS=MAXLENTH
        WPCOL+=(INP.POS-OLD.POS)
    END
    IF PASTE THEN
        IF OLD.POS<=PASTE THEN
            ST=OLD.POS; FI=PASTE; GOSUB FOREGROUND
        END
        IF INP.POS>PASTE THEN
            ST=PASTE; FI=INP.POS-1; GOSUB BACKGROUND
        END
    END ELSE
        IF TRIM(INP.STRING)#'' THEN
            IF INSERTING THEN
                IF Sub=16 THEN
                    INP.STRING=INP.STRING[1,OLD.POS-1]:TABS:INP.STRING[OLD.POS,MAXLENTH]
                END ELSE INP.STRING=TRIM(INP.STRING)
            END
            IF TRIM(INP.STRING[OLD.POS,MAXLENTH])='' THEN
                FI='L#':INP.POS
                INP.STRING=INP.STRING[1,INP.POS] FI
                GOSUB CRT.UNDERLINE
            END
        END ELSE
            CRT TABS:
            INP.STRING=TABS:INP.STRING
        END
    END
    RETURN
17  !
    IF INP.POS-8>=0 THEN
        WPCOL-=8
        INP.POS-=8
    END ELSE
        INP.POS=1
        WPCOL=ORIG.COL
    END
    IF PASTE THEN
        IF OLD.POS>PASTE THEN
            ST=PASTE; FI=OLD.POS; GOSUB FOREGROUND
        END
        IF INP.POS<PASTE THEN
            ST=INP.POS; FI=PASTE-1; GOSUB BACKGROUND
        END
    END
    RETURN
18  !
    INCLUDE EB.OS.INCLUDES PC.OFF.CURSOR
    IF INDEX(PREV.CHARS,FG$CASE.CH,1) AND LEN(FG$CASE.CH) THEN
        GOSUB FIND.WORD
    END ELSE
        NEW.POS=LENTH
        WORD=INP.STRING[INP.POS,NEW.POS]
        WORD.LENGTH=LEN(WORD)
    END
    OLD.WORD=WORD; NEW.WORD=''; GOSUB CASE.WORD
    ST=OLD.POS; FI=INP.POS-1; GOSUB FOREGROUND
    INP.STRING=(INP.STRING[1,INP.POS-1]:WORD:INP.STRING[NEW.POS,MAXLENTH])         ;! JUST
    IF PASTE THEN
        ST=INP.POS; FI=NEW.POS; GOSUB BACKGROUND
    END ELSE
        GOSUB CRT.UNDERLINE
    END
    INCLUDE EB.OS.INCLUDES PC.RESET.CURSOR
    RETURN
19  RETURN
20  !
    RETURN
21  !
    RETURN
22  !
    RETURN
23  !
    CRT INP.STRING[INP.POS,1] 'L#1':
    INP.POS++
    RETURN
INSERT.STRING: !
    WORD.LENGTH=LEN(PASTE.STRING)
    SAVE.POS=INP.POS
    SAVE.WPCOL=WPCOL
    LOOP
        DUMMY=TRIM(INP.STRING,' ',"T")
        FIELD.LENGTH=LEN(DUMMY)
        ORIG.LENGTH=WORD.LENGTH
        WORD.LENGTH=MAXLENTH-FIELD.LENGTH
        INP.STRING=TRIM(INP.STRING[1,INP.POS-1]:PASTE.STRING[1,WORD.LENGTH]:INP.STRING[INP.POS,MAXLENTH],' ',"T")
        INP.STRING=INP.STRING JUST
    UNTIL FIELD.LENGTH+ORIG.LENGTH<=MAXLENTH DO
        INP.POS+=WORD.LENGTH
        WPCOL+=WORD.LENGTH
        OLD.POS=INP.POS
        WPCHR=PASTE.STRING[WORD.LENGTH+1,1]
        PASTE.STRING=PASTE.STRING[WORD.LENGTH+2,MAXLENTH]
        WORD.LENGTH=LEN(PASTE.STRING)
    REPEAT
    INP.STRING=TRIM(INP.STRING,' ',"T")
    INP.POS=SAVE.POS
    WPCOL=SAVE.WPCOL
    GOSUB CRT.UNDERLINE
    PASTE.STRING=''
    RETURN
STMP.OCONV:
    BEGIN CASE
        CASE TYPE='D'
            STMP=INP.STRING JUST
        CASE TYPE='T'
            STMP=OCONV(INP.STRING,JUST)
        CASE TYPE='YN'
            STMP='NY'[INP.STRING+1,1]
        CASE LEN(JUST)
            STMP=INP.STRING JUST
        CASE 1
            STMP=INP.STRING
    END CASE
    RETURN
STMP.ICONV:
    BEGIN CASE
        CASE TYPE='T'
            INP.STRING=ICONV(STMP,JUST)
        CASE TYPE='YN'
            INP.STRING=(OCONV(STMP, 'MCU') = 'Y')
    END CASE
    RETURN
    INCLUDE EB.INCLUDES EB.WP.SUBROUTINES
