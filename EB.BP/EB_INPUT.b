    SUBROUTINE EB_INPUT(INP.STRING,INP.FLD,LENTH,UNDERLINE.FLAG,MAT EB$CHARS,MAT OTHER.PARAMS,FG$EB.PARAMS,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,INIT.FLAG,ROW,HELP.ID,EB.CMD)
    INCLUDE EB.OS.INCLUDES AT.MINUS.CODES
    DIM EB$CHARS(100), OTHER.PARAMS(100)
    DIM DAT(1), WIN.PANE(1), DRV.PARAMS(50), SCR.PARAMS(100), SCREEN.PARAMS(100)
    MAT DAT=''; MAT WIN.PANE=''; MAT DRV.PARAMS=''; MAT  SCR.PARAMS=''; MAT  SCREEN.PARAMS=''
    INCLUDE JBC.h
    DEFC INT JBASEEmulateGETINT(INT, INT)
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    vm_start=IF_COMPILED_PRIME
    FG$FLD=1
    COL=0; Sub=''
    IF INIT.FLAG='' THEN INP.STRING=''  ;!SPACE(LENTH)
    FG$OLD.FIELD=INP.STRING
    BACK=@(-AM.BACK)          ;!CHAR(21)
    FWD=@(-AM.FWD)  ;!CHAR(6)
    ERASE=@(-1)
    IF EB.CMD<1>='' THEN EB.CMD<1>='X'  ;* Abort cmd
    IF EB.CMD<1>=';; ' THEN EB.CMD<1>=''
    BG=''; FG=''; DIMM=''; BRIGHT=''
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS DRV.PARAMS
    INCLUDE EB.EQUS SCR.PARAMS
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS REFRESH.CODES
    INCLUDE EB.EQUS EB.CHARS
    INCLUDE EB.EQUS ACT.CODES
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
!
    MAX.JUST='R#':FG$MAX.CHARS
    MAX.LEN=FG$MAX.CHARS+FG$EXPECT.CR
    ERRS=0
    LAST.CHR=''
    WORD.WRAP=''
    PASTE.STRING=''
    OLD.POS=0
    SAVE.POS=1
    ACT.CODE=FALSE
    PASTE=FALSE
    INSERTING=FALSE
    NUM.FIELD=FALSE
    TT.FIELD=FALSE
    TT.LENTH=LENTH
    INDENT=FALSE; MARGIN=''
    SKIP.LIST=''
    TABS=SPACE(8)
    DO.SPELL=FALSE
! initialize Paragraph Reformat codes
    P=FG$ERROR.MSGS<46,1>
    D=FG$ERROR.MSGS<46,3>
!
! Display "WP Mode"
!
    WORD.PROCESSING=FALSE
    WRAP.FIELD=FALSE
    VALIDATE=FALSE
    IF INP.FLD='' THEN INP.FLD='AN'
    TYPE=INP.FLD<1>
    NUM.FIELD=INDEX('NM',TYPE[1,1],1)
    DFLD=0
    FG$ACT.CODE=''
    EXIT.CODE=FG$END.CODE
!
! Get length from SCR.PARAMS ?
!
    JUST="L#":LENTH
    CLEAR.FIELD=SPACE(LENTH)
    DOTS=STR('_',LENTH)
    LAST.COL=COL+LENTH
    IF LAST.COL>79 THEN
    LAST.POS=@(LAST.COL-80,ROW+1)
END ELSE LAST.POS=@(LAST.COL,ROW)
IF INP.FLD<2>#'' THEN CPOS=INP.FLD<2> ELSE CPOS=1
ORIG.COL=COL
COL+=CPOS-1
PREV.CHARS=''
CHR.NBR=0 ;!FIRST.ASCII
RTN.KEY=FALSE
NORMAL.FIELD=(TYPE#'LIT' AND TYPE#'MENU' AND TYPE#'ACT')
HIDDEN=(TYPE[1,1]='H')
TRAIL=LENTH-LEN(INP.STRING)
INCLUDE EB.OS.INCLUDES PC.OFF.CURSOR
STMP=TRIM(INP.STRING,' ',"T")
CRT STMP:; XX=LEN(STMP)
IF UNDERLINE.FLAG THEN
CRT BG:DOTS[1,TRAIL]:FG:      ;!ELSE CRT SPACE(TRAIL):
XX+=TRAIL
END
XX=XX-CPOS+1
CRT STR(BACK,XX):
TRAIL=0
INCLUDE EB.OS.INCLUDES PC.RESET.CURSOR
!
! Start of input logic
!
START:    !
LOOP
ECHO OFF
LAST.NBR=CHR.NBR
!
! If FG$TYPEAHEAD was not null, process any "normal" characters as input
!
FG$ACT.CODE=FALSE
FG$LAST.ACT.CODE=FALSE
PREV.CHARS=''
IF FG$TYPEAHEAD.BUFF#'' THEN
CHR=FG$TYPEAHEAD.BUFF[1,1]; FG$TYPEAHEAD.BUFF=FG$TYPEAHEAD.BUFF[2,999]
CHR.NBR=SEQ(CHR)
END ELSE
INCLUDE EB.OS.INCLUDES UT.INPUT.TIMEOUT
IF FG$TIMEDOUT THEN RETURN
END
IF CHR.NBR<FIRST.ASCII AND LAST.NBR>=FIRST.ASCII AND LAST.NBR<=LAST.ASCII THEN PREV.CHARS=''
IF COUNT(FG$INPUT.CODES,PREV.CHARS:CHR)>1 AND CHR.NBR#RTN.VAL AND (CHR.NBR<FIRST.ASCII OR CHR.NBR>LAST.ASCII) THEN
!
! following code tries to trap all characters from single key-stroke
!
INCLUDE EB.OS.INCLUDES SYSTEM.14
IF NOT(SYSTEM.14) THEN
IF INDEX(FG$INPUT.CODES,CHR,1) THEN
INCLUDE EB.OS.INCLUDES INPUT.DELAY
END
END
INCLUDE EB.OS.INCLUDES SYSTEM.12
NBR.CHARS=1
LOOP
INCLUDE EB.OS.INCLUDES INPUT.DIFF
INCLUDE EB.OS.INCLUDES SYSTEM.14
WHILE SYSTEM.14 AND DIFF<=0 AND NBR.CHARS<=MAX.LEN DO
PREV.CHARS:=CHR; CHR=''
IF NBR.CHARS<MAX.LEN THEN
INCLUDE EB.OS.INCLUDES INPUT.ZERO
INCLUDE EB.OS.INCLUDES SYSTEM.12
END
LAST.NBR=CHR.NBR
LAST.CHR=CHAR(CHR.NBR)
NBR.CHARS+=1
IF FG$EXPECT.CR AND CHR#'' THEN
LOCATE PREV.CHARS:CHR:CR IN FG$FUNC.CHARS<1,vm_start> SETTING POS THEN
INCLUDE EB.OS.INCLUDES SYSTEM.14
IF NOT(SYSTEM.14) THEN
INCLUDE EB.OS.INCLUDES INPUT.DELAY
INCLUDE EB.OS.INCLUDES SYSTEM.14
END
IF SYSTEM.14 THEN
PREV.CHARS:=CHR; CHR=''
INCLUDE EB.OS.INCLUDES INPUT.ZERO
END
END
END
IF FG$EXPECT.CR AND CHR=CR THEN CHR=''; CHR.NBR=LAST.NBR
REPEAT
END
IF HIDDEN THEN ECHO OFF ELSE ECHO ON
!
! Was the <RETURN> or <Line-Feed> key used ?
!
PROCESS.RTN:        !
IF CHR.NBR=RTN.VAL THEN
IF INP.STRING='' AND NUM(INP.FLD) THEN
DEFAULT.TYPE=FG$SCR.DEFAULT.TYPE<1,INP.FLD>
IF DEFAULT.TYPE[1,1]='[' THEN
CALL EB.SET.DFLT(DEFAULT.TYPE[2,THE.REST],INP.STRING)
END
END
RTN.KEY=TRUE
IF NOT(WRAP.FIELD OR TYPE='MENU' OR TYPE='LIT') THEN
IF VALIDATE AND TRIM(INP.STRING)#'' THEN
GOSUB VALID.INPUT.CHECK
IF RTN.KEY THEN VALIDATE=FALSE
END
END ELSE
IF NOT(WORD.PROCESSING) AND (FG$PANE=1 AND CPOS=1 OR (WRAP.FIELD AND FG$PANE>=FG$NBR.PANES<1,FG$WINDOW>)) THEN
CHR=''; PREV.CHARS=FG$INPUT.CODES<1,EXIT.CODE>
END
END
END
UNTIL RTN.KEY AND NOT(WRAP.FIELD) DO
OLD.POS=CPOS
Sub=0
!
! Has a valid function been keyed in
!
STMP=PREV.CHARS:CHR
IF (OCONV(STMP,'MCP')=STMP AND (OCONV(STMP,'MCA')=STMP OR OCONV(STMP,'MCN')=STMP)) OR TRIM(STMP)='' ELSE
IF FG$MAX.CHARS<3 AND OCONV(LAST.CHR,'MCA')#'' AND PREV.CHARS='' THEN IF CHR.NBR>=FIRST.ALPHA AND CHR.NBR<=LAST.ALPHA THEN GO ASCII.START
!        PREV.CHARS=TRIM((PREV.CHARS:CHR) MAX.JUST)
PREV.CHARS=PREV.CHARS:CHR
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
!
! Ignore certain functions if not at first position
!
IF FG$ACT.CODE THEN
BEGIN CASE
CASE FG$ACT.CODE=FG$ABT.CODE; RETURN
CASE FG$ACT.CODE=FG$BCK.CODE; RETURN
CASE FG$ACT.CODE=FG$SKP.CODE; RETURN
CASE FG$ACT.CODE=FG$SEARCH.CODE; RETURN
CASE FG$ACT.CODE=FG$OPT.CODE; RETURN
CASE FG$ACT.CODE=FG$END.CODE; RETURN
CASE FG$ACT.CODE=FG$TCL.CODE; RETURN
CASE FG$ACT.CODE=FG$SEL.CODE; RETURN
CASE FG$ACT.CODE=FG$HLP.CODE
CASE FG$ACT.CODE=FG$JMP.CODE
IF HELP.ID#'' THEN
CALL EB.BASIC.ZOOM(HELP.ID)
END
RETURN
CASE FG$ACT.CODE=FG$EXIT.LN.CODE; FG$ACT.CODE=FALSE
CASE FG$ACT.CODE=FG$RFR.CODE; RETURN
CASE 1; FG$LAST.ACT.CODE=FG$ACT.CODE; FG$ACT.CODE=FALSE; CHR.NBR=0
END CASE
END
IF FG$ACT.CODE=EXIT.CODE OR FG$ACT.CODE=FG$EXIT.LN.CODE THEN
FG$ACT.CODE=FG$EXIT.LN.CODE
END
IF FG$ACT.CODE THEN
IF FG$ACT.CODE=FG$HLP.CODE AND HELP.ID#'' THEN
CALL EB.UT.HELP(HELP.ID,FG$EB.PARAMS,TERM.TYPE)
FG$LAST.ACT.CODE=FG$ACT.CODE; FG$ACT.CODE=FALSE
GOTO START
END ELSE
LAST.CHR=''
IF JUST[1,2]='MT' THEN INP.STRING=ICONV(OCONV(INP.STRING,'MCU'),JUST)
GOTO FINISH
END
END
Sub=0
IF CHR.NBR>=FIRST.ASCII AND CHR.NBR<=LAST.ASCII AND LAST.NBR>=FIRST.ALPHA AND LAST.NBR<=LAST.ALPHA THEN
!
ASCII.START:        !
LAST.CHR=CHAR(LAST.NBR)
IF LAST.CHR=OCONV(LAST.CHR,'MCA') THEN
ASCII.INPUT:        !
IF COL<LAST.COL AND NOT(HIDDEN) THEN CRT CHR:
!
! Word wrap
!
IF INSERTING THEN
IF TRIM(INP.STRING[LENTH,1])#'' OR CPOS-1=LENTH THEN
CRT BELL:
END ELSE
INP.STRING=(INP.STRING[1,CPOS-1]:CHR:INP.STRING[CPOS,LENTH])
COL+=1
CPOS+=1
DUMMY=TRIM(INP.STRING[CPOS,LENTH],' ',"T")
IF DUMMY#'' THEN GOSUB CRT.UNDERLINE
END
END ELSE
IF CPOS-1=LENTH THEN
CRT BELL:
END ELSE
IF CPOS=1 AND TRIM(INP.STRING)#'' THEN
IF WORD.PROCESSING OR LAST.CHR=CHAR(8) ELSE
CPOS=2
COL+=1
GOSUB 10  ;! cleol
CPOS=1
COL-=1
END
END
INP.STRING=(INP.STRING[1,CPOS-1]:CHR:INP.STRING[CPOS+1,LENTH])
COL+=1
CPOS+=1
END
END
IF PASTE THEN
IF CPOS<=PASTE THEN
IF NOT(INSERTING) THEN
ST=OLD.POS; FI=CPOS-1; GOSUB FOREGROUND
END ELSE
PASTE+=1
ST=OLD.POS; FI=OLD.POS; GOSUB BACKGROUND
END
END
END
LAST.CHR=CHR
!!          PREV.CHARS=''
GO START
END
END
NEXT.PHASE:         !
!
BEGIN CASE
! <ctrl>E - End of Line
CASE FG$LAST.ACT.CODE=FG$EOL.CODE
Sub=3
! <ctrl>X - Start of Line
CASE FG$LAST.ACT.CODE=FG$SOL.CODE
Sub=4
! INS char
CASE FG$LAST.ACT.CODE=FG$INS.CODE
IF INDEX(SPC,INP.STRING[LENTH,1],1) OR (TT.FIELD AND LEN(INP.STRING)<=TT.LENTH) THEN Sub=5 ELSE CRT BELL:
! Set INS
CASE FG$LAST.ACT.CODE=FG$INSERT.CODE
Sub=6
! BackSpace or <==
CASE CHR.NBR=SEQ(FG$BS.CH) OR INDEX(PREV.CHARS,FG$LEFT.CH,1)
IF CPOS>1 THEN
Sub=7
END ELSE
CPOS=SAVE.POS; COL=CPOS+ORIG.COL-1
SAVE.POS=1
PREV.CHARS=''
END
CASE FG$LAST.ACT.CODE=FG$DEL.CHAR.CODE
STMP=TRIM(INP.STRING[CPOS,LENTH],' ',"T")
IF STMP#'' THEN Sub=8
! DEL
CASE FG$LAST.ACT.CODE=FG$DEL.WORD.CODE
Sub=9
! DEL line
CASE INDEX(PREV.CHARS,FG$DELLN.CH,1)
Sub=10
! <ctrl>DEL (undelete)
CASE FG$LAST.ACT.CODE=FG$UNDEL.CODE
Sub=11
! <ctrl>D back a word
CASE INDEX(PREV.CHARS,FG$BWORD.CH,1) AND CPOS>1
Sub=12
! <ctrl> next word
CASE FG$LAST.ACT.CODE=FG$FWORD.CODE
Sub=14
! => (skip)
CASE FG$LAST.ACT.CODE=FG$RIGHT.CODE OR INDEX(PREV.CHARS,FG$SKIP.CH,1)
IF CPOS<=LENTH THEN
IF TYPE='MENU' AND CPOS=1 THEN FG$ACT.CODE=FG$RIGHT.CODE; RETURN
Sub=15
END
! Back TABS
CASE FG$LAST.ACT.CODE=FG$BTAB.CODE OR INDEX(PREV.CHARS,FG$BTAB.CH,1)
IF TYPE='MENU' THEN FG$ACT.CODE=FG$LEFT.CODE; RETURN
Sub=17
! TABS
CASE FG$LAST.ACT.CODE=FG$TAB.CODE OR INDEX(PREV.CHARS,FG$TAB.CH,1)
IF TYPE='MENU' THEN FG$ACT.CODE=FG$SUS.CODE; RETURN
Sub=16
! <ctrl>C Change case of word
CASE FG$LAST.ACT.CODE=FG$CASE.CODE OR FG$LAST.ACT.CODE=FG$L.CASE.CODE
Sub=18
! <ctrl>] Cut
CASE FG$LAST.ACT.CODE=FG$CUT.CODE
Sub=20
! <ctrl>P Paste
CASE FG$LAST.ACT.CODE=FG$PASTE.CODE
Sub=21
CASE RTN.KEY
RTN.KEY=FALSE
INDENT=FALSE; MARGIN=''
DUMMY=TRIM(INP.STRING[CPOS,LENTH],' ',"T")
IF DUMMY='' THEN    ;! end-of-paragraph
INP.STRING=TRIM(INP.STRING,' ',"T")
IF NOT(INDEX(INP.STRING,NULL.CHAR,1)) THEN INP.STRING:=NULL.CHAR
END
PREV.CHARS=FG$DOWN.CH
CPOS=1
COL=ORIG.COL
Sub=13
!/|\ |
! | (up-arrow) and \|/ (down arrow)
CASE WORD.PROCESSING AND INDEX(PREV.CHARS,FG$UP.CH,1) OR INDEX(PREV.CHARS,FG$DOWN.CH,1) OR INDEX(PREV.CHARS,FG$TOP.CH,1)
Sub=13
! <ctrl>O Multi-function facility
! or
! INS Line
CASE TYPE='MENU' AND INDEX(PREV.CHARS,FG$MULTI.CH,1)
FG$ACT.CODE=FG$TAG.CODE; RETURN
CASE INDEX(PREV.CHARS,FG$PRV.CH,1) OR INDEX(PREV.CHARS,FG$NXT.CH,1)
IF NOT(NORMAL.FIELD) THEN
IF INDEX(PREV.CHARS,FG$NXT.CH,1) THEN
IF INP.FLD='ACT' THEN FG$ACT.CODE=FG$SKP.CODE ELSE FG$ACT.CODE=FG$NXTS.CODE
END ELSE
IF INP.FLD='ACT' THEN FG$ACT.CODE=FG$BCK.CODE ELSE FG$ACT.CODE=FG$PRVS.CODE
END
GOTO FINISH
END
CASE CHR.NBR>=FIRST.ASCII AND CHR.NBR<=LAST.ASCII AND CPOS-1<=LENTH
GOTO ASCII.INPUT
END CASE
IF Sub THEN
ON Sub GOSUB 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,1,1,16
PREV.CHARS=''
IF Sub>2 THEN CHR=''
END
LAST.CHR=CHR
REPEAT
FINISH:   !
IF CPOS=2 AND TRIM(INP.STRING)='' THEN INP.STRING=' ' ELSE
IF TYPE#'LIT' THEN INP.STRING=TRIM(INP.STRING,' ',"T")
END
IF HIDDEN ELSE
COL=ORIG.COL
INCLUDE EB.OS.INCLUDES PC.OFF.CURSOR
IF NORMAL.FIELD AND INP.STRING#'' THEN
IF JUST[1,2]#'MT' THEN
IF INDEX(INP.STRING,NULL.CHAR,1) THEN CONVERT NULL.CHAR TO HARD.RTN IN INP.STRING
CRT STR(BACK,CPOS-1):INP.STRING JUST:
END ELSE CRT STR(BACK,CPOS-1):OCONV(INP.STRING,JUST):
END ELSE
IF UNDERLINE.FLAG THEN CRT STR(BACK,CPOS-1):CLEAR.FIELD:
END
INCLUDE EB.OS.INCLUDES PC.BLOCK.CURSOR
END
RETURN
VALID.INPUT.CHECK:  !
INP.STRING=TRIM(INP.STRING)
ERRMSG=''
BEGIN CASE
CASE TYPE[1,1]='D'
BEGIN CASE
CASE INP.STRING='.'; STMP=TODAYS$DATE
CASE NUM(INP.STRING) AND LEN(INP.STRING)<3; STMP=ICONV(INP.STRING:' ':OCONV(TODAYS$DATE,DM.CONV),'D')
CASE 1
STMP=ICONV(INP.STRING,'D')
END CASE
IF STMP#'' THEN INP.STRING=STMP ELSE ERRMSG=FG$ERROR.MSGS<41>
!    CASE TYPE='T'
!      STMP=ICONV(OCONV(INP.STRING,'MCU'),'MT')
!      IF STMP#'' THEN INP.STRING=STMP ELSE ERRMSG=FG$ERROR.MSGS<79>
CASE TYPE='T'
INP.STRING=OCONV(INP.STRING,'MCU')
STMP=OCONV(INP.STRING,'MCN')
IF LEN(STMP)>4 THEN
STMP+=(120000*(INDEX(INP.STRING,'P',1)>0))
DUMMY='R%%:%%:%%'
END ELSE
IF STMP<99 THEN STMP=STMP*100
STMP+=(1200*(INDEX(INP.STRING,'P',1)>0))
DUMMY='R%%:%%'
END
STMP=STMP DUMMY
BEGIN CASE
CASE INP.STRING='.'; STMP=CURR$TIME
CASE NUM(INP.STRING) AND LEN(INP.STRING)<3; STMP=ICONV(INP.STRING:':00','MT')
CASE 1
STMP=ICONV(STMP,'MTS')
END CASE
IF STMP#'' THEN INP.STRING=STMP ELSE ERRMSG=FG$ERROR.MSGS<79>
CASE NUM.FIELD
IF INP.STRING[1,1]='$' THEN INP.STRING=INP.STRING[2,LENTH]
IF NUM(INP.STRING) THEN
IF TYPE[1,1]='M' THEN STMP=2 ELSE STMP=TYPE[2,2]+0
IF INP.STRING=INP.STRING STMP THEN STMP=1 ELSE
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
END CASE
IF ERRMSG#'' THEN
RTN.KEY=FALSE
INP.STRING=FG$OLD.FIELD
CPOS=1; COL=ORIG.COL
CRT @(0,23):ERRMSG:
END
RETURN
CRT.UNDERLINE:      !
IF HIDDEN THEN RETURN
INCLUDE EB.OS.INCLUDES PC.OFF.CURSOR
STMP=TRIM(INP.STRING[CPOS,LENTH],' ',"T")
CRT STMP:; XX=LEN(STMP)
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
STMP=INP.STRING
IF INDEX(STMP,NULL.CHAR,1) THEN CONVERT NULL.CHAR TO HARD.RTN IN STMP
CRT STR(BACK,CPOS-1):STMP JUST:
END ELSE CRT STR(BACK,CPOS-1):OCONV(INP.STRING,JUST):
INCLUDE EB.OS.INCLUDES PC.RESET.CURSOR
RETURN
NEXT.WORD:!
CALL EB.NEXT.WORD(COL,CPOS,INP.STRING,WORD,LENTH,0)
RETURN
FIND.WORD:!
COL-=CPOS
LOOP
THIS.CHR=INP.STRING[CPOS,1]
UNTIL OCONV(THIS.CHR,'MCN')#'' OR OCONV(THIS.CHR,'MCA')#'' OR CPOS=1 DO
CPOS-=1
REPEAT
COL+=CPOS
GET.WORD: !
CALL EB.GET.WORD(COL,CPOS,NEW.POS,INP.STRING,WORD,LENTH,WORD.LENGTH)
RETURN
1         !
ACT.CODE=FALSE
ACTION=LAST.CHR
RETURN
2         !
ACT.CODE=TRUE
IF 0 THEN
IF CPOS=1 THEN WORD.WRAP=INP.STRING[1,1]
INP.STRING=(INP.STRING[1,CPOS-1]:CHR:INP.STRING[CPOS+1,LENTH])
COL+=1
CPOS+=1
END
RETURN
3         !
DUMMY=TRIM(INP.STRING,' ',"T")
CRT STR(FWD,LEN(DUMMY)-CPOS+1):
CPOS=LEN(DUMMY)+1
COL=ORIG.COL+CPOS-1
IF PASTE THEN
IF OLD.POS<=PASTE THEN ST=OLD.POS; FI=PASTE; GOSUB FOREGROUND
IF CPOS>PASTE THEN ST=PASTE; FI=CPOS-1; GOSUB BACKGROUND
END
RETURN
4         !
CRT STR(BACK,CPOS-1):
OLD.POS=CPOS
COL=ORIG.COL
CPOS=1
IF PASTE THEN
IF OLD.POS>PASTE THEN ST=PASTE; FI=OLD.POS; GOSUB FOREGROUND
IF CPOS<PASTE THEN ST=CPOS; FI=PASTE-1; GOSUB BACKGROUND
END
RETURN
5         !
CHR=' '
INP.STRING=INP.STRING[1,CPOS-1]:' ':INP.STRING[CPOS,LENTH]
GOSUB CRT.UNDERLINE
IF CPOS<=PASTE THEN
ST=CPOS; FI=PASTE; GOSUB BACKGROUND
PASTE+=1
END
RETURN
6         !
IF INSERTING THEN
INSERTING=FALSE
INCLUDE EB.OS.INCLUDES PC.BLOCK.CURSOR
END ELSE
INSERTING=TRUE
INCLUDE EB.OS.INCLUDES PC.LINE.CURSOR
END
RETURN
7         !
COL-=1
CPOS-=1
CRT BACK:
IF CHR.NBR=8 THEN
CRT SPC:BACK:
TRAIL=1
INP.STRING=INP.STRING[1,CPOS-1]:INP.STRING[CPOS+1,LENTH]
GOSUB CRT.UNDERLINE
END
IF NOT(INSERTING) THEN
IF PASTE THEN
IF OLD.POS>PASTE THEN
ST=CPOS; FI=CPOS; GOSUB FOREGROUND
END ELSE
ST=CPOS; FI=CPOS; GOSUB BACKGROUND
END
END
END
RETURN
8         !
FG$DELETE.LIST=INP.STRING[CPOS,1]:AM:FG$DELETE.LIST
DEL.LABEL:!
INP.STRING=(INP.STRING[1,CPOS-1]:INP.STRING[CPOS+1,LENTH])[1,LENTH]
IF UNDERLINE.FLAG ELSE INP.STRING:=' '
TRAIL=1
GOSUB CRT.UNDERLINE
IF CPOS<=PASTE THEN
PASTE-=1
ST=CPOS; FI=PASTE-1; GOSUB BACKGROUND
END
RETURN
9         !
THIS.CHR=INP.STRING[CPOS,1]
IF OCONV(THIS.CHR,'MCA')#'' OR OCONV(THIS.CHR,'MCN')#'' THEN
GOSUB GET.WORD
END ELSE
NEW.POS=CPOS+1
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
INP.STRING=(INP.STRING[1,CPOS-1]:INP.STRING[NEW.POS,LENTH]) ;! JUST
TRAIL=WORD.LENGTH+1
IF UNDERLINE.FLAG ELSE INP.STRING:=SPACE(TRAIL)
GOSUB CRT.UNDERLINE
IF OLD.POS<=PASTE THEN
PASTE-=WORD.LENGTH
ST=CPOS; FI=PASTE-1; GOSUB BACKGROUND
END
RETURN
10        !
TRAIL=TRIM(INP.STRING[CPOS,LENTH],' ',"T")
FG$DELETE.LIST=INP.STRING[CPOS,LENTH]:AM:FG$DELETE.LIST
TRAIL=LEN(TRAIL)
IF UNDERLINE.FLAG ELSE CRT SPACE(TRAIL):STR(BACK,TRAIL):
INP.STRING=INP.STRING[1,CPOS-1]
GOSUB CRT.UNDERLINE
RETURN
11        !
PASTE.STRING=FG$DELETE.LIST<1>
DEL FG$DELETE.LIST<1>
GOSUB INSERT.STRING
RETURN
12        !
CPOS-=1
COL-=(1+CPOS)
LOOP
THIS.CHR=INP.STRING[CPOS,1]
UNTIL OCONV(THIS.CHR,'MCN')#'' OR OCONV(THIS.CHR,'MCA')#'' OR CPOS=1 DO
CPOS-=1
REPEAT
LOOP
THIS.CHR=INP.STRING[CPOS,1]
UNTIL OCONV(THIS.CHR,'MCN')='' AND OCONV(THIS.CHR,'MCA')='' OR CPOS=1 DO
CPOS-=1
REPEAT
IF OCONV(THIS.CHR,'MCN')='' AND OCONV(THIS.CHR,'MCA')='' THEN
CPOS+=1
END
COL+=CPOS
CRT STR(BACK,OLD.POS-CPOS):
IF PASTE THEN
IF CPOS>PASTE THEN
ST=CPOS; FI=OLD.POS; GOSUB FOREGROUND
END ELSE
IF OLD.POS>PASTE THEN ST=PASTE; FI=OLD.POS; GOSUB FOREGROUND
ST=CPOS; FI=PASTE-1; GOSUB BACKGROUND
END
END
RETURN
13        !
RETURN
14        !
GOSUB NEXT.WORD
CRT STR(FWD,CPOS-OLD.POS):
IF PASTE THEN
IF OLD.POS<=PASTE THEN
ST=OLD.POS; FI=PASTE; GOSUB FOREGROUND
END
IF CPOS>PASTE THEN
ST=PASTE; FI=CPOS-1; GOSUB BACKGROUND
END ELSE
ST=CPOS; FI=PASTE-1; GOSUB BACKGROUND
END
END
RETURN
15        !
COL+=1
CPOS+=1
CRT FWD:
IF PASTE THEN
IF CPOS<=PASTE THEN
ST=OLD.POS; FI=OLD.POS; GOSUB FOREGROUND
END ELSE
ST=OLD.POS; FI=OLD.POS; GOSUB BACKGROUND
END
END
RETURN
16        !
IF CPOS+8<=LENTH THEN
COL+=8
CPOS+=8
END ELSE
CPOS=LENTH
COL+=(CPOS-OLD.POS)
END
IF PASTE THEN
IF OLD.POS<=PASTE THEN
ST=OLD.POS; FI=PASTE; GOSUB FOREGROUND
END
IF CPOS>PASTE THEN
ST=PASTE; FI=CPOS-1; GOSUB BACKGROUND
END
END ELSE
IF TRIM(INP.STRING)#'' THEN
IF INSERTING THEN
IF Sub=16 THEN
INP.STRING=INP.STRING[1,OLD.POS-1]:TABS:INP.STRING[OLD.POS,LENTH]
END ELSE INP.STRING=TRIM(INP.STRING)
END
IF TRIM(INP.STRING[OLD.POS,LENTH])='' THEN
FI='L#':CPOS
INP.STRING=INP.STRING[1,CPOS] FI
GOSUB CRT.UNDERLINE
END
END ELSE
CRT TABS:
INP.STRING=TABS:INP.STRING
END
END
RETURN
17        !
IF CPOS-8>=0 THEN
COL-=8
CPOS-=8
END ELSE
CPOS=1
COL=ORIG.COL
END
IF PASTE THEN
IF OLD.POS>PASTE THEN
ST=PASTE; FI=OLD.POS; GOSUB FOREGROUND
END
IF CPOS<PASTE THEN
ST=CPOS; FI=PASTE-1; GOSUB BACKGROUND
END
END
RETURN
18        !
INCLUDE EB.OS.INCLUDES PC.OFF.CURSOR
IF INDEX(PREV.CHARS,FG$CASE.CH,1) THEN
GOSUB FIND.WORD
END ELSE
NEW.POS=LENTH
WORD=INP.STRING[CPOS,NEW.POS]
WORD.LENGTH=LEN(WORD)
END
OLD.WORD=WORD; NEW.WORD=''; GOSUB CASE.WORD
ST=OLD.POS; FI=CPOS-1; GOSUB FOREGROUND
INP.STRING=(INP.STRING[1,CPOS-1]:WORD:INP.STRING[NEW.POS,LENTH])      ;! JUST
IF PASTE THEN
ST=CPOS; FI=NEW.POS; GOSUB BACKGROUND
END ELSE
GOSUB CRT.UNDERLINE
END
INCLUDE EB.OS.INCLUDES PC.RESET.CURSOR
RETURN
19 RETURN
20        !
RETURN
21        !
RETURN
22        !
RETURN
23 RETURN
INSERT.STRING:      !
WORD.LENGTH=LEN(PASTE.STRING)
SAVE.POS=CPOS
SAVE.COL=COL
LOOP
DUMMY=TRIM(INP.STRING,' ',"T")
FIELD.LENGTH=LEN(DUMMY)
ORIG.LENGTH=WORD.LENGTH
WORD.LENGTH=LENTH-FIELD.LENGTH
INP.STRING=TRIM(INP.STRING[1,CPOS-1]:PASTE.STRING[1,WORD.LENGTH]:INP.STRING[CPOS,LENTH],' ',"T")
INP.STRING=INP.STRING JUST
UNTIL FIELD.LENGTH+ORIG.LENGTH<=LENTH DO
CPOS+=WORD.LENGTH
COL+=WORD.LENGTH
OLD.POS=CPOS
CHR=PASTE.STRING[WORD.LENGTH+1,1]
PASTE.STRING=PASTE.STRING[WORD.LENGTH+2,LENTH]
WORD.LENGTH=LEN(PASTE.STRING)
REPEAT
INP.STRING=TRIM(INP.STRING,' ',"T")
CPOS=SAVE.POS
COL=SAVE.COL
GOSUB CRT.UNDERLINE
PASTE.STRING=''
RETURN
INCLUDE EB.INCLUDES EB.WP.SUBROUTINES
