! Initialisation
! ==============
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(100),EB.FILE.LIST
    COM RDSP(100), CHANGES(100)
    COMMON /EB_LEXER/reservedWords, colors, comments, commentlen, incomment
    INCLUDE EB.INCLUDES lex.h
    INCLUDE EB.INCLUDES jbcReserved.h
    INCLUDE EB.INCLUDES cReserved.h
    INCLUDE EB.EQUS STD.EQUS
    DEFFUN initColors()
    DEFFUN GETFLNM()
    DEFFUN GET_CATALOG_FILE()
    DEFFUN EB_CLOSE()
    DEFFUN EB_EXPANDFLNM()
    DEFFUN SVN_DELETE()
    DEFFUN SVN_GETORIGPATH()
    DEFFUN SVN_GET_REPOSITORY()
    DEFFUN EBJSHOW()
    DEFFUN GETFULLPATH()
    DEFFUN EBGETHOME()
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS OTHER.PARAMS
    EQU MAX TO 999999999
    EQU Bslsh TO '\', Fslsh TO '/'
    shell = @IM:'k'
    shellend = ' 2>&1'
    DEBUG.CODES = ''
    rc = GETENV('EB_DEBUG', DEBUG.CODES)
    DEBUG.CODES = CHANGE(DEBUG.CODES, ',', @AM)
!
    MAT GEX=''; MAT EXTRAS=''
    DEFC INT JBASEEmulateGETINT(INT, INT)
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    vm_start=IF_COMPILED_PRIME
    INCLUDE EB.OS.INCLUDES GET.TCL.SENTENCE
    EDIT.MODE=FIELD(FG$SENTENCE:'(','(',2)
    IF INDEX(EDIT.MODE, 'V', 1) THEN
        READ.ONLY.MODE = @TRUE
        EDIT.MODE = CHANGE(EDIT.MODE,'V','')
    END ELSE READ.ONLY.MODE = @FALSE
    BINARY.MODE = INDEX(EDIT.MODE, 'B', 1)
    FG$SENTENCE=TRIM(FG$SENTENCE[1,COL1()-1])
    INCLUDE EB.OS.INCLUDES OS
    INCLUDE EB.OS.INCLUDES WHO
    IF GETENV('pwd',currdir) ELSE
        IF GETENV('PWD',currdir) THEN NULL
    END
    IF FIELD(FG$SENTENCE,SPC,DCOUNT(FG$SENTENCE,SPC)) = '*' THEN
        FG$SENTENCE = OCONV(FG$SENTENCE,'G1 ':COUNT(FG$SENTENCE,SPC)-1)
        EXECUTE 'SELECT ':FG$SENTENCE CAPTURING IO
        FG$SENTENCE = 'EB ':FG$SENTENCE
    END
    IF SYSTEM(11) THEN
        EOF=0
        LOOP
            READNEXT ITNM THEN
                FG$SENTENCE:=SPC:CHANGE(ITNM, SPC, '{sp}')
            END ELSE EOF=1
        UNTIL EOF DO REPEAT
    END
    FG$SENTENCE=TRIM(FG$SENTENCE)
    NBR.WORDS=DCOUNT(FG$SENTENCE,SPC)
    INCLUDE EB.OS.INCLUDES EB.INIT
    INCLUDE EB.OS.INCLUDES OS.ERRORS
    OPEN 'MD' TO F.MD THEN MD_flag = @TRUE ELSE MD_flag = @FALSE
    path = EBGETHOME()
    CALL EB_OPEN('','.',F.currdir,1,0)
    CALL EB_OPEN('',path:'EB.EQUS',FG$EQUS,1,0)
    CALL EB_OPEN('',path:'EB.SECURITY',FG$SECURITY,1,0)
    CALL EB_OPEN('',path:'EB.USERS',FG$USERS,1,0)
    CALL EB_OPEN('',path:'GLOBAL.BTREEDEFS',B$BTREEDEFS,1,0)
    CALL EB_OPEN('',path:'JET.PASTE',JET.PASTE,1,0)
    jutil_ctrl_pos = FALSE
    IF GETENV('JBCRELEASEDIR',jbcreleasedir) THEN
        CALL EB_OPEN('',jbcreleasedir:DIR_DELIM_CH:'tmp':DIR_DELIM_CH:'jutil_ctrl',JUTIL_CTRL,1,jutil_ctrl_pos)
        IF jutil_ctrl_pos THEN K.JUTIL_CTRL = 'jsh_o_':FG$TLINE
    END
    CALL EB_OPEN('','EB.CONTROL',FG$EB.CONTROL,0,POS)
    IF POS THEN
        READV APPLICATION.CODES FROM FG$EB.CONTROL,'APPLICATIONS',1 ELSE APPLICATION.CODES=''
        READV PATCH.TYPES FROM FG$EB.CONTROL,'PATCH.TYPES',1 ELSE STOP RDER,'EB.CONTROL, PATCH.TYPES'
        READ BP.FILES FROM FG$EB.CONTROL,'BP.FILES' ELSE BP.FILES=''
    END ELSE
        APPLICATION.CODES=''; FG$SEC.LEVEL=''
        PATCH.TYPES=''
    END

    CALL EB_OPEN('',path:'EB.PROCESSES',FG$PROCESSES,0,POS)

    IF POS ELSE FG$PROCESSES=JET.PASTE
    CALL EB_OPEN('',path:'EB.HELP.DOC,ENG',FG$HELP.DOC,0,POS)
    IF POS THEN
        OPEN.HELP=TRUE
        FG$EB.HELP=FG$HELP.DOC
        CALL EB_OPEN('',path:'EB.HELP.INDEX,ENG',FG$EB.HELP.INDEX,0,POS)
        IF POS ELSE FG$EB.HELP.INDEX=FG$EB.HELP
    END ELSE OPEN.HELP=FALSE

    ITNM=FG$SENTENCE
    CALL EB_UT_INIT
    FG$SENTENCE=ITNM
    READ FG$ERROR.MSGS FROM FG$EB.PARAMS,'ERROR.MSGS*ENG' ELSE NULL
    INCLUDE EB.EQUS SCREEN.PARAMS
    MATREAD SCREEN.PARAMS FROM FG$EB.PARAMS,'CRT@':FG$TERM.TYPE ELSE
        FG$TERM.TYPE=FIELD(FG$TERM.TYPE,'_',1)
        MATREAD SCREEN.PARAMS FROM FG$EB.PARAMS,'CRT@':FG$TERM.TYPE ELSE STOP "I don't know your CRT type (":FG$TERM.TYPE:")"
    END
    INCLUDE EB.OS.INCLUDES TERM.SETTINGS
    MAT SCR.CRT.PARAMS=''; MAT DRV.PARAMS=''; MAT SCR.PARAMS=''; MAT CRT.PARAMS=''
    FG$CRT.PAGES='RTED'
    FG$CRT.PAGE=1
    MATREAD COLOURS FROM FG$EB.PARAMS,'COLOUR@':FG$TERM.TYPE ELSE MAT COLOURS=''
    colors = initColors(MAT COLOURS)
    IF EMBED.ATTR<1,1> THEN HLON=RVON; HLOFF=RVOFF ELSE HLON=BG; HLOFF=FG
    INCLUDE EB.EQUS COLOURS
    IF WHITE<1,1>#'' THEN
        INCLUDE EB.EQUS COLOUR.CODES
        IF WHITE<1,1>#'' THEN CALL EB_CH.COLOUR(WHITE<1,1>,BLUE<1,2>)
        FG$CHOICE.COLOURS=WHITE<1,1>:VM:RED<1,2>:VM:YELLOW<1,1>
    END
    INCLUDE EB.EQUS EB.CHARS
    INCLUDE EB.EQUS ACT.CODES
    CYCLES=FG$OPT.CODE:AM:FG$SKP.CODE:AM:FG$BCK.CODE:AM:FG$NXT.KEY.CODE:AM:FG$PRV.KEY.CODE:AM:FG$SEL.CODE
    IF CURS.BLOCK<1,1>='' THEN CURS.INS=@(PWIDTH-5,(PDEPTH-1)):SPC:'Ins':CLEOL ELSE CURS.INS=CURS.BLOCK<1,1>
    IF CURS.LINE<1,1>='' THEN CURS.RPL=@(PWIDTH-5,(PDEPTH-1)):SPC:FLSH:'Rpl':NFLSH:CLEOL ELSE CURS.RPL=CURS.LINE<1,1>
    IF CURS.ON#CURS.LINE<1,1> THEN CURS.INS:=CURS.ON
    CURS.RPL:=CURS.ON
    INP.SUB=0
    INS.MODE=1
    DIM RPL.PARMS(3),RPL.PROMPTS(3),RPL.COLS(3)
    EQU WHOLE TO RPL.PARMS(1)
    EQU ALOC TO RPL.PARMS(2)
    EQU CONFIRM TO RPL.PARMS(3)
    RPL.PROMPTS(1)="Whole Words ? "
    RPL.PROMPTS(2)="All Occurences on a line ? "
    RPL.PROMPTS(3)="Confirm Each Replacement ? "
    FOR I=1 TO 3
        RPL.COLS(I)=LEN(RPL.PROMPTS(I))
    NEXT I
    READ DSPLY FROM JET.PASTE, '%DSPLY%' THEN
        DELETE JET.PASTE, '%DSPLY%'
        ERR.NOS=COMPER
    END ELSE
        DSPLY = ''
        ERR.NOS=241
    END
    NBR.DSPLY = DCOUNT(DSPLY, @AM)
    SECRET=0
    PR="--Press <RETURN>"
    CONV.TYPE="MCU"
    MSG.AKN=@(0,(PDEPTH-1))
    MSG.CLR=MSG.AKN:CLEOP
    EQU DELIMS TO ' ():;+-*/,&!^#=<>[]@'
    EQU OTHER.MARGIN TO CHAR(22)        ;*Control V
    EQU REP.STR TO CHAR(18)   ;* Control R
    BS.CH=FG$BS.CH
    SUB.CODES=BS.CH:VM:REP.STR:VM:OTHER.MARGIN:VM:CTRL.N
    READ ALPHA.CHARS FROM FG$EB.PARAMS,'ALPHA.CHARS' ELSE
        ALPHA.CHARS=''
        FOR I=32 TO 126; ALPHA.CHARS<1,-1>=CHAR(I); NEXT I
        WRITE ALPHA.CHARS ON FG$EB.PARAMS,'ALPHA.CHARS'
    END
    SUB.CODES<1,-1>=ALPHA.CHARS
    PROMPT ''
!  FG$TLINE=OCONV(FG$TLINE,'MCN')
    IF FG$TLINE='' THEN FG$TLINE=0
    READV FG$SEC.LEVEL FROM FG$SECURITY,FG$LOGNAME,4 ELSE FG$SEC.LEVEL=''
!    READV FG$USER.NAME FROM FG$USERS,FG$LOGNAME,1 ELSE FG$USER.NAME=''
    FG$USER.NAME=FG$LOGNAME
    READV SITE.NAME FROM FG$EB.CONTROL,'SITE',1 ELSE SITE.NAME='Site Unkown'
    CALL EB_OPEN('',FG$LOGNAME:'.PATCHES',F.PATCHFILE,0,POS)
    IF NOT(POS) THEN CALL EB_OPEN('',FG$TUSER:'.PATCHES',F.PATCHFILE,0,POS)
    IF POS THEN
        OPEN.PATCH=TRUE
    END ELSE OPEN.PATCH=FALSE
    WORKFILENAME='EB.WORK,':FG$LOGNAME
    CALL EB_OPEN('',path:WORKFILENAME,FG$WORK.FILE,0,POS)
    IF POS THEN
        READ EB.VARS FROM FG$WORK.FILE,'EB.VARS' ELSE EB.VARS=''
    END ELSE FG$WORK.FILE=JET.PASTE
    READ BASE.ITEM FROM FG$EQUS,'BASE' THEN
        DEL BASE.ITEM<1>
        A=1
        LOOP
            LINE=BASE.ITEM<A>
        WHILE LINE[1,1]='*' DO
            BEGIN CASE
                CASE INDEX(LINE,'Author',1)
                    LINE:=SPC:FG$USER.NAME
                CASE INDEX(LINE,'Date Written',1)
                    LINE:=SPC:TIMEDATE()
                CASE INDEX(LINE,'Standard program banner',1)
                    LINE='* <enter program function here>'
            END CASE
            BASE.ITEM<A>=LINE
            A+=1
        REPEAT
        LAM=DCOUNT(BASE.ITEM,AM)
        FOR I=A TO LAM; DEL BASE.ITEM<A>; NEXT I
    END ELSE BASE.ITEM=''
    INPTYPE='AN'
    DEL.LIST=''
    PREV.FLNM=""; PREV.ITNM=""; MITNM=''; MFLNM=''
    PREV.FLNM=""; PREV.ITNM=""; MITNM=''; MFLNM=''
    CUT.POS=''; MARKERS=''
    LMARGIN=''
    INCLUDE EB.OS.INCLUDES EB.LMARGIN
    READ COMPILE.VERB FROM FG$EB.PARAMS,'EB.COMPILE.VERB' ELSE
        COMPILE.VERB='BASIC':@AM:'CATALOG'
    END
    CATALOG.VERB=COMPILE.VERB<2>
    COMPILE.OPTS=COMPILE.VERB<3>
    IF COMPILE.OPTS#'' THEN COMPILE.OPTS=SPC:COMPILE.OPTS
    COMPILE.VERB=shell:COMPILE.VERB<1>
    ITAB = ''
    READ ITAB FROM FG$EB.PARAMS,'EB.ITAB' ELSE
        ITAB=2:@AM:8:@AM:3
    END
    MAT OPENED.FILES=''; OPEN.FILE.LIST=''
    IF MOD(FG$STERM,3) THEN
        CALL EB_AT.WINDOW.OPEN(PDEPTH+1,PWIDTH+1,1,1,1,'',ITNM,1)
        CALL EB_STERM.MENU('EB.MENU','','',1,'')
        CSI=ESC:'['; APC=ESC:'_'; ST=ESC:'\'
        LEADIN=CSI:'='
        CRT LEADIN:'1;6O':    ;! return co-ords on single click
        CRT APC:'6;0O':ST:
        CRT APC:'1O|AM':ST:
    END
    IF NOT(GETENV('EBACCUTERM',accuterm)) THEN accuterm = TRUE
    IF accuterm THEN CRT ESC:CHAR(2):1:
!
    EQU RTN.VAL TO 13
    DIM PATCH(11)
    EQU SPRNO TO PATCH(1)
    EQU APPL TO PATCH(2)
    EQU DESCRIPTION TO PATCH(3)
    EQU TYPE TO PATCH(4)
    EQU Checksum TO PATCH(8)
    EQU CHANGED.BY TO PATCH(9)
    EQU REL.NO TO PATCH(10)
    CODES=FG$INPUT.CODES
    SUB.CODE1=-1; SUB.CODE2=-1
    MATCH.SET=''
    READV FG$TIMEOUT FROM FG$EB.PARAMS,FG$LOGNAME:'.EB',1 ELSE FG$TIMEOUT=300
    FG$MONITOR.SECS=FG$TIMEOUT
    INCLUDE EB.OS.INCLUDES SET.TIMEOUT
    INCLUDE EB.OS.INCLUDES TIMEOUT.ON
!
    MAX.SUB=1
    SUB.JUST='R#1'
    CONV=0
    MAX.JUST='R#':FG$MAX.CHARS
    IF FG$MAX.CHARS>MAX.SUB THEN MAX.LEN=FG$MAX.CHARS+FG$EXPECT.CR*0 ELSE MAX.LEN=MAX.SUB
!
    PREV.CHARS=''
    PREV.SUBS=''
    DFLT.LINE=''
    LAST.DESC=''
    HEX.MODE=FALSE
!
    INCLUDE EB.OS.INCLUDES BACKGROUND.VERB
    WCNT=3
    ITNM=''
    FLNM=FIELD(FG$SENTENCE," ",2); ITNM=FIELD(FG$SENTENCE," ",WCNT)
    FLNM=CHANGE(FLNM, '{sp}', SPC)
    ITNM=CHANGE(ITNM, '{sp}', SPC)
    IF FLNM="DICT" THEN
        FLNM='DICT ':ITNM
        WCNT+=1
        ITNM=FIELD(FG$SENTENCE," ",WCNT)
        ITNM=CHANGE(ITNM, '{sp}', SPC)
    END
    ORIG_WCNT = WCNT
    END.WORDS='END':AM:'NEXT':AM:'UNTIL':AM:'WHILE'
    USEMODE=''; PASSWD=''
    UPG=FALSE
    ENCRYPT.HEADCNT=0; ENCRYPT.MESG=''
    INCLUDE EB.OS.INCLUDES BASIC.VERB
    CATL.LIST=''
    CALL EB_RSS(0)
    CALL EB_OPEN('',FLNM,FIL,0,FPOS)
    IF LEN(FLNM) AND FLNM NE '.' AND FLNM NE '..' AND (ITNM='' OR NOT(FPOS)) THEN
        SITNM = ITNM
        ITNM=FLNM
        INCLUDE EB.OS.INCLUDES GET.FLNM
        IF FLNM='' THEN
            FLNM=ITNM
            ITNM=''
        END
        IF LEN(SITNM) THEN
            FG$SENTENCE = SITNM:' ':FG$SENTENCE
        END
    END
    READ LAST.EB FROM FG$EB.CONTROL,FG$LOGNAME:'.LAST.EB' ELSE LAST.EB=''
    IF ITNM='' THEN
        BEGIN CASE
            CASE FLNM='.'
                POS=1
            CASE FLNM='..'
                POS=2
            CASE 1
                wdepth = (PDEPTH * .75) "0"
                IF FLNM#'' THEN
                    CALL EB_CHOICES(20,3,'',wdepth,FLNM,'',ITNM,1,1,0,'L#50','Records in file ':FLNM:SVM:'Item')
                END ELSE
                    WCNT = DCOUNT(LAST.EB<1>, VM)
                    FOR I = WCNT TO 1 STEP -1
                        LUK = LAST.EB<1, I>
                        Y = FIELD(LUK, '*', 1)
                        Z = FIELD(LUK, '*', 3)
                        CALL EB_OPEN('', Y, FIL, FALSE, POS)
                        IF POS THEN
                            READV LOC FROM FIL,Z,1 ELSE POS = FALSE
                            CLOSE FIL
                            Z = EB_CLOSE('', Y)
                        END
                        IF NOT(POS) THEN
                            IF I< WCNT THEN
                                FIL = LAST.EB<2, I>
                                DEL LAST.EB<1, I>
                                DEL LAST.EB<2, I>
                                LAST.EB<1, -1> = LUK
                                LAST.EB<2, -1> = FIL
                            END
                        END
                    NEXT I
                    CALL EB_CHOICES(20,3,'',wdepth,'',LAST.EB,ITNM,1,1,1:SVM:1,'R#50':CTRL.C:'G*1':SVM:'L#25':CTRL.C:'G2*99','Previous EB Sessions':SVM:'File':SVM:'Item')
                    FLNM=''
                END
                IF ITNM#ITNM<1,1,1> THEN
                    LIST=ITNM
                    LOC=0
                    LOOP
                        REMOVE ITNM FROM LIST AT LOC SETTING DELIM
                        FG$SENTENCE:=SPC:ITNM
                        NBR.WORDS++
                    WHILE DELIM DO REPEAT
                    ITNM=LIST<1,1,1>
                    WCNT = 2
                    ORIG_WCNT = WCNT
                END ELSE
                    IF ITNM#'' AND FLNM#'' THEN ITNM=FLNM:'**':ITNM
                END
                POS=0
        END CASE
        IF POS THEN ITNM = LAST.EB<1,POS>
        IF ITNM#'' THEN
            IF INDEX(ITNM, '*', 1) THEN
                FLNM=FIELD(ITNM,'*',1)
                EDIT.MODE=FIELD(ITNM,'*',2)
                ITNM=ITNM[COL2()+1,MAX]
            END
        END ELSE
            GO WRAPUP
        END
    END
    GOSUB GET.EDIT.MODE
    GOSUB SET.MODE
    IF FLNM='' THEN
        GO WRAPUP
    END ELSE
        CONVERT BS TO SPC IN FLNM
        CRT @(-1):
        GO 25
    END
    GO FIRST.ITEM
NEXT.ITEM:!
    GOSUB LAST.USED
FIRST.ITEM: !
    WCNT+=1
    ITNM=FIELD(FG$SENTENCE,SPC,WCNT)
    ITNM=CHANGE(ITNM, '{sp}', SPC)
    IF ITNM='' THEN
        GO WRAPUP
    END ELSE
        TMP=FIELD(ITNM,'*',1)
        IF TMP#ITNM THEN
            FLNM=TMP
            EDIT.MODE=FIELD(ITNM,'*',2)
            ITNM=ITNM[COL2()+1,MAX]
        END ELSE GOSUB GET.EDIT.MODE
        IF EDIT.MODE#'' THEN GOSUB SET.MODE
    END
    IF FLNM#'' THEN GO 20
!
! Prompt for File
!
5   CRT @(-1):
10  !
    CRT @(0,0):"File Name? ":CLEOP:
    L=46; Z=FLNM; INPTYPE='U'
    GOSUB INPT
    FLNM=FIELD(Z,SPC,1)
    FLNM=TRIM(FLNM) ;! FLNM=OCONV(FLNM,'MCU')
    IF FG$ACT.CODE=FG$ABT.CODE THEN GO WRAPUP
    IF FIELD(FLNM,SPC,1)='DICT' THEN DCT=FLNM[6,MAX] ELSE DCT=FLNM
    IF ITNM#'' THEN GO 25
!
! Prompt for next item
!
20  !
    IF FG$ACT.CODE=FG$NXT.KEY.CODE OR FG$ACT.CODE = FG$AMD.CODE ELSE
        CRT @(0,1):"Item Name? ":CLEOP:
        L=46; Z=ITNM
        GOSUB INPT
        ITNM=TRIM(ITNM)
        BEGIN CASE
            CASE FG$ACT.CODE=FG$ABT.CODE; GO WRAPUP
            CASE ITNM="^" OR FG$ACT.CODE=FG$BCK.CODE OR ITNM='<'; GO 10
            CASE ITNM=ESC; GO WRAPUP
            CASE FG$ACT.CODE=FG$SKP.CODE; GO NEXT.ITEM
        END CASE
        CRT
    END
25  !
    IF FLNM='.' THEN
        FLNM=currdir
    END
    IF NOT(FPOS) AND INDEX(ITNM,DIR_DELIM_CH,1) THEN
        FLNM=ITNM
        INCLUDE EB.OS.INCLUDES GET.FLNM
        IF FLNM='' THEN
            FLNM=ITNM
            ITNM=''
        END
    END
    FLNM = EB_EXPANDFLNM(FLNM)
    IF FIELD(FLNM,SPC,1)='DICT' THEN DCT=FLNM[6,MAX]; DCT<2>='DICT' ELSE DCT=FLNM
    CALL EB_OPEN(DCT<2>,DCT<1>,FIL,0,POS)
    IF POS THEN
        FLNM = DCT<1>
        GOSUB SETUP.SWITCH
    END ELSE
        ITNM=FLNM
        INCLUDE EB.OS.INCLUDES GET.FLNM
        IF FLNM#'' THEN FG$SENTENCE='EB ':FLNM:SPC:OCONV(FG$SENTENCE,'G1 ':NBR.WORDS); NBR.WORDS+=1; GO 25
        CRT MSG.CLR:"File does not exist! ":
        GO 10
    END
    BAS.OPTS = ''
    OPEN 'DICT',FLNM THEN
        READ ITAB FROM 'EB_INDENT' ELSE NULL
        READ BAS.OPTS FROM 'EB_BAS_OPTS' ELSE NULL
    END
READ.ITEM:!
    UPDATES=TRUE
    tempItem = 0 - (ITNM[1,1]='%' AND (ITNM 'R#1')='%')
    VersCheckedOut=tempItem
    lockvar=TRUE
    FIRST.READ=(SFLNM#HFLNM)
REREAD.ITEM: !
    IF BINARY.MODE THEN
        rc = IOCTL(FIL, JIOCTL_COMMAND_CONVERT, "RB,WB")
    END
!
! Tricky bit here....
! If multiple items are being processed from a particular file
! some (or all) of them could be in the user's home dir
!
    IF READ.ONLY.MODE THEN GO ALREADY.LOCKED
    READU REC FROM FIL,ITNM LOCKED
        INCLUDE EB.OS.INCLUDES LOCKED.BY
        CRT MSG.CLR:"Item locked by ":OSINC$LOCKED.BY:" (":OSINC$LOCKED.PORT:")! Enter 'Y' to enquire only ":
        YNC=65; YNR=(PDEPTH-1); YNCHRS='Y':VM:'N'; YNL=1; GOSUB GET.CHAR
        CRT MSG.CLR:
        IF Y#'Y' THEN GO 20
ALREADY.LOCKED: !
        UPDATES=FALSE
        READ REC FROM FIL,ITNM ELSE REC=''
        IF READ.ONLY.MODE THEN
            INCLUDE EB.INCLUDES CHECK.B
        END
    END THEN
! dodgy way of checking for updates
        CALL EB_VERS_CTRL(VersStat,lockvar, tempItem)
        BEGIN CASE
            CASE NOT(lockvar)
            CASE lockvar = 24576
                UPDATES=FALSE
                lockvar=0
        END CASE
        BEGIN CASE
            CASE lockvar<0
                CALL EB_OPEN(DCT<2>, FLNM, FIL, TRUE, POS)
                GOSUB SET.MSG
                GO READ.ITEM
            CASE lockvar
                GO ALREADY.LOCKED
        END CASE
    END ELSE
        SITNM = ITNM
        LOOP
            INCLUDE EB.INCLUDES CHECK.B
        UNTIL ITNM EQ UPCASE(ITNM) DO
            ITNM = UPCASE(ITNM)
            READV DUMMY FROM FIL,ITNM,1 THEN GO READ.ITEM
        REPEAT
        ITNM = SITNM
        IF FIRST.READ THEN
            GOSUB SWITCH.FILE
            GOSUB SET.MSG
            FIRST.READ = FALSE
            RELEASE FIL,ITNM
            GO REREAD.ITEM
        END ELSE
            GOSUB SWITCH.FILE
            GOSUB SET.MSG
        END
        CRT MSG.CLR:"New record ":PR:; RQM
        IF ITNM 'R#2'#'.b' AND ITNM 'R#6' # '.jabba' THEN
            IF INDEX('!*',COMMENT,1) THEN
                CRT MSG.CLR:"Is this a BASIC program (Y/N)?":
                YNC=35; YNR=(PDEPTH-1); YNCHRS='Y':VM:'N'; YNL=1; GOSUB GET.CHAR
                CRT MSG.CLR:
                Z=ITNM
            END ELSE Y='N'
        END ELSE
            Y='Y'
            Z=FIELD(ITNM, '.', 1, COUNT(ITNM,'.'))
        END
        IF Y='Y' THEN
            REC='! PROGRAM ':Z:AM:BASE.ITEM
        END ELSE REC=''
!        CALL EB_VERS_CTRL(VersAdd,lockvar, FALSE)
!        VersCheckedOut=(lockvar<1)
    END
    TAB.MODE=INDEX(REC,TAB,1)
    CRLF.MODE=NOT(INDEX(REC,AM,1)) AND INDEX(REC,CR:LF,1)
    IF CRLF.MODE THEN
        REC = CHANGE(REC, CR:LF, AM)
    END
    IF UPG THEN
        ORIG.REC=''
        LAST.AM=DCOUNT(REC,AM)-ENCRYPT.HEADCNT
        IF LAST.AM>0 THEN
            FOR I=1 TO ENCRYPT.HEADCNT; ORIG.REC<I>=REC<LAST.AM+I>; NEXT I
            CALL UPGCHKENCRYPT(REC,ENCRYPTED,CHKSUM,SIZE,VERSION)
!      IF REC<LAST.AM>#ORIG.REC<1> THEN INS ORIG.REC BEFORE REC<LAST.AM>
        END ELSE ENCRYPTED=''
    END ELSE ENCRYPTED=''
    PSTIME=TIME()
    READ ORIG.REC FROM JET.PASTE,ITNM:'.sav' THEN
        YNL='Use recovered ':ITNM:'.sav ? (Y/N) '
        CRT MSG.CLR:YNL:
        YNC=LEN(YNL); YNR=(PDEPTH-1); YNCHRS='Y':VM:'N':AM:AM:'Y'; YNL=1; GOSUB GET.CHAR
        CRT MSG.CLR:
        IF Y='Y' THEN
            PSTIME=ORIG.REC<1>
            DEL ORIG.REC<1>
            CHANGED = (REC#ORIG.REC)
            REC=ORIG.REC
        END
        IF FG$ACT.CODE THEN GO 20
    END ELSE
        IF ENCRYPTED THEN
            IF NOT(UPG) THEN
                CRT 'Encrypted program...required UPG.WORKFILE'
                GO 20
            END
        END
        IF ENCRYPTED THEN
            IF PASSWD='' THEN CALL UPGPASSWD (PASSWD,F.UPG.WORKFILE)
            CALL UPGCONVERT(FLNM,ITNM,REC,'DQ',USEMODE,F.UPG.WORKFILE,PASSWD,CONVOK)
            IF NOT(CONVOK) THEN STOP
        END
    END
    ORIG.REC=REC
    IF FLNM='EB.OS.INCLUDES' AND MD_flag THEN
        READ FLNM FROM F.MD,FLNM THEN
            IF FLNM<1>='Q' THEN FLNM=FLNM<3> ELSE
                FLNM=FIELD(FLNM<2>,DIR_DELIM_CH,DCOUNT(FLNM<2>,DIR_DELIM_CH))
            END
            DCT<1>=FLNM
        END
    END
    GOSUB SET.MSG
    DEL.LINES=''; CUT.TEXT=''; NEW.CHARS=''; PREV.TIME=TIME()
    MOUSEROW=0; MOUSECOL=0; MOUSESTATE='';
    LUK=FLNM:'*':EDIT.MODE:'*':ITNM
    LAST.AM = DCOUNT(REC, AM)
    LOCATE LUK IN LAST.EB<1> SETTING POS THEN
        COL = LAST.EB<2,POS>
    END ELSE COL = ''
    IF LEN(COL<1,1,1>) AND COL<1,1,3> <= LAST.AM THEN
        ROW = COL<1,1,2>
        INDROW = COL<1,1,3>
        OFFSET = COL<1,1,4>
        COL = COL<1,1,1>
    END ELSE
        COL=5; ROW=0; INDROW=1; OFFSET=0
    END
    STRT=0 ; SCR.UD=TRUE; SCR.LR=FALSE; CHANGED=FALSE; MAT CHANGES=FALSE; SCRL=0
    LAST.ROW=''
    READ HEADERS FROM F.currdir,'eb_headers' THEN
        DELETE F.currdir,'eb_headers'
    END ELSE
        HEADERS=''
        CALL EB_READHEADERS(REC, HEADERS)
    END
    UNDO_STACK = ''
    UNDO_POS = 1
    GOSUB ADD_TO_UNDO
    GO STRT         ;! Skip over subroutines
!==========
AUTO.SAVE:! time check
    CALL EB_AUTOSAVE
    RETURN
!==========
SCRN.TO.REC: ! Incorporate changed lines into dynamic array, REC.
    IF NEW.CHARS#'' THEN GOSUB ADD.CHARS
    GOSUB UPDATE.REC
    LAST.AM = DCOUNT(REC, AM)
    IF UPDATES AND (SCR.UD OR CHANGED) THEN GOSUB AUTO.SAVE ;! reminder msg.
    CHANGED=FALSE; MAT CHANGES=FALSE
    FG$TIMEDOUT=FALSE
    FG$ACT.CODE=FALSE
    FG$TIMEOUT=FG$MONITOR.SECS*10
    RETURN
UPDATE.REC:!
    FOR I=1 TO PDEPTH
        IF CHANGES(I) THEN
            CALL EB_TRIM(RDSP(I),RDSP(I):'',SPC,'T')
            IF TAB.MODE THEN CALL EB_TRIM(RDSP(I),RDSP(I):'',TAB,'T')
            REC<I+INDROW-1>=RDSP(I)
            CHANGED=TRUE
        END
    NEXT I
    RETURN
!==========
STRT: ! top of main loop
    Z = FG$ACT.CODE
    IF CHANGED THEN GOSUB SCRN.TO.REC
    CALL EB_REFRESH
    LAST.NBR=FIRST.ALPHA
    IF Z = FG$MULTI.CODE THEN FG$ACT.CODE = Z
TOP: !
    INCLUDE EB.INCLUDES VERS_CTRL
    IF COL<5 THEN GO STRT
    THIS.ROW=INDROW+ROW
    IF LAST.ROW#THIS.ROW THEN
        IF LAST.ROW > LAST.AM THEN
            DIMON = BG
            DIMOFF = FG
        END ELSE DIMON = ''; DIMOFF = ''
        LAST.ROW-=INDROW
        RR=PDEPTH-LAST.ROW
        IF RR>=0 THEN CRT @(0,LAST.ROW):DIMON:(LAST.ROW+INDROW) "R#4":DIMOFF:
        LAST.ROW=INDROW+ROW
        IF LAST.ROW > LAST.AM THEN
            DIMON = BG
            DIMOFF = FG
        END ELSE DIMON = ''; DIMOFF = ''
        CRT @(0,ROW):HLON:DIMON:LAST.ROW "R#4":DIMOFF:HLOFF:
    END
    READ autocmd FROM F.currdir,'eb_auto' THEN
        DELETE F.currdir,'eb_auto'
        FG$ACT.CODE=autocmd<1>
        IF autocmd<3> THEN
            FG$TYPEAHEAD.BUFF = autocmd<2>
        END ELSE
            LOCATE autocmd<2> IN SSS<am_start> SETTING POS THEN DEL SSS<POS>
            INS autocmd<2> BEFORE SSS<1>
        END
        GOTO CHECK.CODES
    END
    IF FG$ACT.CODE = FG$AMD.CODE OR FG$ACT.CODE = FG$MULTI.CODE THEN
        CHR=REP.STR
    END ELSE
        FG$ACT.CODE=FALSE
        CRT MSG.AKN:@(MSG.COL):(COL+OFFSET-4) 'R#3':
        CRT @(COL,ROW):
        CALL EB_GET_INPUT(CHR, CHR.NBR)
    END
    LOCATE FG$ACT.CODE IN DEBUG.CODES SETTING DPOS THEN DEBUG
    IF FG$TIMEDOUT THEN
        GOSUB SCRN.TO.REC
        CALL EB_TABCOL(RDSP(LROW),COL,LCOL,TRUE)
        GO TOP
    END

    IF FG$ACT.CODE=FG$EXIT.LN.CODE THEN FG$ACT.CODE=FALSE; CHR=FG$EXIT.LN
    IF FG$ACT.CODE OR CHR.NBR<32 OR CHR.NBR>250 OR CHR.NBR=SEQ(BS.CH) THEN
        IF NEW.CHARS#'' THEN GOSUB ADD.CHARS
    END
    IF NOT(FG$ACT.CODE) THEN
        IF CHR.NBR>31 AND CHR.NBR<251 AND CHR.NBR#SEQ(BS.CH) THEN
            INCLUDE EB.INCLUDES VERS_CTRL
            NEW.CHARS:=CHR
            IF NEW.CHARS='}' AND TAB.MODE THEN    ;! unindent?
                CALL EB_TRIM(CHECK.LINE,RDSP(LROW),TAB,'T')
                IF CHECK.LINE='' THEN
                    CHECK.LINE=RDSP(LROW)[1,LEN(RDSP(LROW))-1]
                    DUMMY=CHECK.LINE:'x'          ;! build a dummy line
                    GOSUB FORMAT
                    IF LCOL=I AND I>TABLEN THEN I-=TABLEN
                    RDSP(LROW)=STR(TAB,INT(I/TABLEN))       ;! CHECK.LINE
                    LCOL=I
                    CALL EB_TABCOL(RDSP(LROW),COL,LCOL,FALSE)
                    STRT=LCOL
                    GOSUB ADD.CHARS
                    COL+=1; LCOL+=1
                    CRT @(COL,ROW):'}':
                    COL+=1; LCOL+=1
                    GO STRT
                END
            END
            COL+=1
            IF COL>PWIDTH THEN
! paginate comments ?!?
                Z=TRIM(RDSP(LROW):NEW.CHARS)
                Y=Z[1,1]
                IF INDEX('*!',Y,1) AND OCONV(Z[2,10],'MCA')#'' THEN
                    GOSUB ADD.CHARS
                    CALL EB_TABCOL(RDSP(LROW),COL,LCOL,TRUE)
                    GOSUB BACK.WORD
                    RDSP(LROW)=RDSP(LROW)[1,I-1]:Y:RDSP(LROW)[I,MAX]
                    LCOL=I
                    CALL EB_TABCOL(RDSP(LROW),COL,LCOL,FALSE)
                    CRT @(COL,ROW):
                    GOSUB SPLIT.LINE; ROW+=1; LROW+=1
                    LLEN=LEN(RDSP(LROW)); GO GEOL
                END ELSE
                    GOSUB ADD.CHARS
                    GO STRT
                END
            END ELSE
                IF INS.MODE THEN
                    IF INS.CHAR#'' THEN CRT INS.CHAR: ELSE
                        TMP=RDSP(LROW)[LCOL,PWIDTH+1-COL]
                        CRT @(COL-1,ROW):
                        IF TRIM(TMP)#'' THEN CRT SPC:TMP:@(COL-1,ROW):
                    END
                END
                CRT CHR:
                GO TOP
            END
        END
    END
    BEGIN CASE
        CASE FG$ACT.CODE=FG$RIGHT.CODE
        CASE FG$ACT.CODE=FG$LEFT.CODE
        CASE FG$ACT.CODE=FG$BCK.CODE
        CASE FG$ACT.CODE=FG$SKP.CODE
        CASE FG$ACT.CODE=FG$FWORD.CODE
        CASE FG$ACT.CODE=FG$BWORD.CODE
        CASE FG$ACT.CODE=FG$TAB.CODE
        CASE FG$ACT.CODE=FG$ADD.CODE
        CASE FG$ACT.CODE=FG$LST.CODE
        CASE 1
            POS=DCOUNT(MARKERS<1>,VM)
            Z=INDROW+ROW:SVM:LCOL+4
            IF Z#MARKERS<2,POS> THEN
                POS+=1
                INS '<' BEFORE MARKERS<1,POS>
                INS Z BEFORE MARKERS<2,POS>
            END
    END CASE
!! new chars prev pos
    CALL EB_TABCOL(RDSP(LROW),COL,LCOL,TRUE)
    LLEN=LEN(RDSP(LROW))
    IF CHR=BS.CH THEN
        IF (LCOL+OFFSET)=1 THEN
            IF LROW>1 THEN LROW-=1; LLEN=LEN(RDSP(LROW)); ROW-=1; GO GEOL
        END ELSE
            IF LCOL>LLEN+1 THEN
                COL-=1
                CALL EB_TABCOL(RDSP(LROW),COL,LCOL,TRUE)
                GO TOP
            END ELSE
                GOSUB ADD_TO_UNDO
                IF INS.MODE THEN
                    SP1=""
                    IF COL>4 THEN
                        IF TAB.MODE THEN
                            CRTLN=RDSP(LROW)[1,LCOL-2]:SP1:RDSP(LROW)[LCOL,MAX]
                            COL-=7*(RDSP(LROW)[LCOL-1,1]=TAB)
                            CALL EB_TABS(CRTLN,PWIDTH)
                            CRT @(COL-1,ROW):CLEOL:CRTLN[COL-5,PWIDTH+1-COL]:
                        END ELSE
                            CRT BS.CH:
                            Y=RDSP(LROW)
                            CALL EB_TABS(Y,PWIDTH)
                            Y=Y[PWIDTH-4+OFFSET,2]
                            IF DEL.CHAR#'' THEN
                                CRT DEL.CHAR:
                                IF Y[2,1]#'' THEN CRT @(PWIDTH-1,ROW):Y:
                            END ELSE
                                CRT @(COL-1,ROW):CLEOL:Y[COL-4,PWIDTH+1-COL]:
                            END
                        END
                    END
                END ELSE SP1=SPC
                RDSP(LROW)=RDSP(LROW)[1,LCOL-2]:SP1:RDSP(LROW)[LCOL,MAX]
                IF LCOL=LLEN+1 THEN LLEN-=1; RDSP(LROW)=RDSP(LROW)[1,LLEN]
                COL-=1; GOSUB CHG.LROW
                IF COL>4 THEN CRT @(COL,ROW):SP1:
                CALL EB_TABCOL(RDSP(LROW),COL,LCOL,TRUE)
                GO TOP
            END
        END
    END
CHECK.CODES: !
    BEGIN CASE
        CASE FG$ACT.CODE=FG$RIGHT.CODE
            COL+=1
            IF COL<PWIDTH+1 THEN
                CALL EB_TABCOL(RDSP(LROW),COL,LCOL,TRUE)
                GO TOP
            END
        CASE FG$ACT.CODE=FG$LEFT.CODE
            COL-=1
            IF COL<5 THEN
                IF NOT(OFFSET) THEN
                    IF LROW>1 THEN
                        LROW-=1; LLEN=LEN(RDSP(LROW)); ROW-=1; GO GEOL
                    END
                END
            END ELSE
                CALL EB_TABCOL(RDSP(LROW),COL,LCOL,TRUE)
                CRT BACK:
                GO TOP
            END
        CASE FG$ACT.CODE=FG$BCK.CODE
            IF INDROW=1 AND LROW=1 THEN
                IF CHANGED THEN GOSUB SCRN.TO.REC
                CALL EB_TRIM(REC,REC:'',AM,'T')
                CNT=DCOUNT(REC,AM)
                Y=INDROW
                INDROW=CNT-(PDEPTH-2)
                IF INDROW<0 THEN INDROW=1
                COL=5; ROW=CNT-INDROW
                SCR.UD=Y#INDROW
            END ELSE
                ROW-=1
                IF LROW > 1 THEN LLEN=LEN(RDSP(LROW-1))         ;! else it's done in SCR.UD block
                LROW-=1
                IF LROW<1 THEN
                    IF INDROW>1 THEN
                        IF CHANGED THEN GOSUB SCRN.TO.REC
                        FOR I=(PDEPTH-1) TO 1 STEP -1
                            RDSP(I+1)=RDSP(I)
                        NEXT I
                        INDROW-=1
                        RDSP(1)=REC<INDROW>
                        IF DEL.LINE#'' THEN
                            CRT @(0,(PDEPTH-2)):DEL.LINE:@(0,0):INS.LINE:
                            IF INDROW > LAST.AM THEN
                                DIMON = BG
                                DIMOFF = FG
                            END ELSE DIMON = ''; DIMOFF = ''
                            CRT DIMON:INDROW 'R#4 ':DIMOFF:
                            CRTLN=RDSP(1);CRT.X=1+OFFSET;CRT.Y=PWIDTH-4
                            S=LROW; LROW=1; GOSUB CRT.LN; LROW=S
                            CRT MSG.DSP:
                        END ELSE SCR.UD=TRUE; SCR.LR=TRUE
                        ROW+=1; LROW+=1
                        IF SCR.UD THEN GO STRT
                    END
                    GO TOP
                END ELSE CRT UP:
            END
        CASE FG$ACT.CODE=FG$SKP.CODE
            IF LROW < (PDEPTH-1) THEN LLEN=LEN(RDSP(LROW+1))    ;! else its done in SCR.UD block
            IF LROW>(PDEPTH-2) THEN
                IF CHANGED THEN GOSUB SCRN.TO.REC
SCROLL.LINE:    !
                GOSUB SCROLL.DOWN
                ON Y GO STRT,TOP
            END ELSE
                ROW+=1
                LROW+=1; CRT LF:; GO TOP
            END
        CASE CHR=CR
            GOSUB ADD_TO_UNDO
            TABLEN=ITAB<ITABPOS>
            IF LROW=(PDEPTH-1) THEN
                IF CHANGED THEN GOSUB SCRN.TO.REC
                GOSUB SCROLL.DOWN; LROW-=1; ROW-=1
            END
            CHECK.LINE=RDSP(LROW)
            DUMMY=LROW+1
            TMP=RDSP(DUMMY)
            IF INS.MODE AND (TRIM(CHECK.LINE[LCOL,MAX])#'' OR TRIM(TMP)#'') THEN
                GOSUB SPLIT.LINE
                CHECK.LINE=RDSP(LROW+1)
            END ELSE
                ! breaks basic formatting - CALL EB_TRIM(CHECK.LINE, RDSP(LROW), ' ', 'L')
                DUMMY=CHECK.LINE; LNM=1; GOSUB FORMAT
            END
            Y=RDSP(LROW)[1,COMMENTLEN]; IF Y=TRIM(RDSP(LROW)) THEN Y=''
            IF CHECK.LINE='' AND INDEX('*!/',Y[1,1],1) AND Y#'' THEN
                CHECK.LINE=RDSP(LROW)[1,COMMENTLEN]; RDSP(LROW+1)=CHECK.LINE; I=TABLEN-1
            END ELSE Y=''
            LCOL=I
            IF TRIM(CHECK.LINE)='' OR INS.MODE THEN   ;! next line is blank
                IF OFFSET THEN
                    OFFSET=0
                    SCR.UD=TRUE
                    SCR.LR=1
                    SCRL=0
                    COL=5
                    CALL EB_TABCOL(RDSP(LROW),COL,LCOL,TRUE)
                END ELSE SCR.LR=1-2*(INS.LINE#'')
            END ELSE
                NLEN=LEN(CHECK.LINE)
                IF NLEN<LCOL THEN ;! next line is shorter
                    IF NLEN<PWIDTH-5 AND OFFSET THEN SCR.LR=1-2*(INS.LINE#''); OFFSET=0; SCRL=0
                    LCOL=NLEN+1
                    IF RDSP(LROW)='' AND TAB.MODE THEN RDSP(LROW)=STR(TAB,INT(LCOL/TABLEN))
                    CALL EB_TABCOL(RDSP(LROW),COL,LCOL,FALSE)
                END
            END
            ROW+=1
            IF CHECK.LINE='' THEN
                LLEN=0; LCOL=I
                CALL EB_TABCOL(RDSP(LROW),COL,LCOL,FALSE)
            END ELSE LLEN=LEN(CHECK.LINE)
            IF TMP='' AND (TRIM(CHECK.LINE)#'' OR TAB.MODE) THEN
                LROW=ROW+1
                IF TRIM(RDSP(LROW))='' THEN
                    IF TAB.MODE THEN
                        RDSP(LROW)=STR(TAB,INT(LCOL/TABLEN))
                    END ELSE RDSP(LROW)=SPACE(LCOL)
                END
                LCOL=LEN(RDSP(LROW))
                CALL EB_TABCOL(RDSP(LROW),COL,LCOL,FALSE)
                IF TAB.MODE THEN COL+=1
                IF SCR.UD ELSE
                    CRT @(5,ROW):RDSP(LROW):
!                COL = 4 + LEN(RDSP(LROW))
!                LCOL = COL
                    GO TOP
                END
            END
        CASE FG$ACT.CODE=FG$SEARCH.CODE OR FG$ACT.CODE=FG$BSEARCH.CODE OR FG$ACT.CODE=FG$MULTI.CODE OR FG$ACT.CODE=FG$CASE.CODE
            IF FG$ACT.CODE=FG$CASE.CODE THEN
                FG$ACT.CODE=FG$BSEARCH.CODE
                FG$LAST.ACT.CODE=FG$ACT.CODE
            END
            SAVE.CODE=FG$ACT.CODE
            IF CHANGED THEN GOSUB SCRN.TO.REC
            FG$ACT.CODE=SAVE.CODE
            IF FG$ACT.CODE=FG$SEARCH.CODE OR FG$ACT.CODE=FG$BSEARCH.CODE THEN
                GOSUB GET.WORD
                IF TRIM(WORD[1,1])#'' AND WORD[1,1]#TAB THEN
                    WORD=TRIM(WORD)
                    LOCATE 'V/':WORD IN SSS<am_start> SETTING POS THEN DEL SSS<POS>
                    INS 'V/':WORD BEFORE SSS<1>
                END
            END
            Z=INDROW
            CALL EB_SEARCH
            GOTO TOP
        CASE FG$ACT.CODE=FG$DEL.CHAR.CODE
            GOSUB ADD_TO_UNDO
            IF TRIM(RDSP(LROW)[LCOL,MAX])='' THEN
                IF TRIM(RDSP(LROW))#'' THEN
                    CALL EB_TRIM(RDSP(LROW),RDSP(LROW):'',SPC,'T')
                    DUMMY=0
                END ELSE RDSP(LROW)=SPACE(LCOL-2); DUMMY=1
                CRT @(0,ROW+1):DEL.LINE
                LCOL+=1
                J.LINE=1; GO 2210
            END ELSE
                IF NOT(TAB.MODE) AND DEL.CHAR#'' THEN
                    CRT @(COL,ROW):DEL.CHAR:
                    CRTLN=RDSP(LROW);CRT.X=PWIDTH-4+OFFSET;CRT.Y=2
                    IF CRTLN[CRT.X+1,1]#'' THEN CRT @(PWIDTH-1,ROW):; GOSUB CRT.LN
                END ELSE
                    CRT @(COL,ROW):CLEOL:; CRTLN=RDSP(LROW);CRT.X=LCOL+1;CRT.Y=PWIDTH+1-COL; GOSUB CRT.LN
                END
!                INS RDSP(LROW)[LCOL,1] BEFORE DEL.LIST<1>
                RDSP(LROW)=(RDSP(LROW)[1,LCOL-1]:RDSP(LROW)[LCOL+1,MAX])
                GOSUB CHG.LROW; LLEN-=1; GO TOP
            END
        CASE FG$ACT.CODE=FG$INS.CODE
            GOSUB ADD_TO_UNDO
            IF INS.CHAR#'' THEN CRT @(COL,ROW):INS.CHAR: ELSE
                CRT @(COL,ROW):SPC:CLEOL:; CRTLN=RDSP(LROW);CRT.X=LCOL;CRT.Y=PWIDTH-COL; GOSUB CRT.LN
            END
            RDSP(LROW)=(RDSP(LROW)[1,LCOL-1]:SPC:RDSP(LROW)[LCOL,MAX])
            GOSUB CHG.LROW; LLEN+=1; GO TOP
        CASE FG$ACT.CODE=FG$TAG.CODE
            GOSUB ADD_TO_UNDO
            IF RDSP(LROW)[1,COMMENTLEN]#COMMENT<1,1,1> THEN
                RDSP(LROW)=COMMENT<1,1,1>:RDSP(LROW)[1,MAX]:COMMENT<1,1,2>
                Y=1
            END ELSE
                Y=-1
                RDSP(LROW)=RDSP(LROW)[1+COMMENTLEN,MAX]
                IF RDSP(LROW)[LEN(RDSP(LROW))-1,COMMENTLEN]=COMMENT<1,1,2> THEN
                    RDSP(LROW)=RDSP(LROW)[1,LEN(RDSP(LROW))-2]
                END
            END
            CRT @(5,ROW):CLEOL:; CRTLN=RDSP(LROW);CRT.X=1;CRT.Y=PWIDTH-5; GOSUB CRT.LN
            IF COL>5 THEN
                COL+=Y
                CALL EB_TABCOL(RDSP(LROW),COL,LCOL,TRUE)
            END
            GOSUB CHG.LROW; LLEN+=Y; GO TOP
        CASE FG$ACT.CODE=FG$END.CODE
            CRT MSG.DSP:
            GO 999
        CASE FG$ACT.CODE=FG$FWORD.CODE
            LLEN1=LLEN+1
! first search for the next non-alpha character
            Y=LCOL+1
            FOR I=Y TO LLEN1 UNTIL NOT(ICONV(RDSP(LROW)[I,1],PC)#''); NEXT I
! then search for the next letter
            Y=I+1
            FOR I=Y TO LLEN1 UNTIL ICONV(RDSP(LROW)[I,1],PC)#''; NEXT I
            LCOL=I
            CALL EB_TABCOL(RDSP(LROW),COL,LCOL,FALSE)
            IF (COL-OFFSET)>PWIDTH THEN COL-=OFFSET; OFFSET+=(PWIDTH-5); SCR.LR=1
            IF COL>PWIDTH THEN COL-=OFFSET
            LOOP WHILE COL<0 DO COL+=(PWIDTH-5) REPEAT
            CALL EB_TABCOL(RDSP(LROW),COL,LCOL,TRUE)
        CASE FG$ACT.CODE=FG$BWORD.CODE
            IF LCOL>1 THEN GOSUB BACK.WORD
        CASE FG$ACT.CODE=FG$DEL.LINE.CODE OR FG$ACT.CODE=FG$CUT.CODE OR FG$ACT.CODE=FG$SEL.CODE
            SREC = REC
            CALL EB_CUT(G60)
            IF CHANGED THEN
                GOSUB PRE_ADD_TO_UNDO
            END ELSE
                SAVE.CODE = REC
                REC = SREC
                GOSUB ADD_TO_UNDO
                REC = SAVE.CODE
                SCR.UD = TRUE
            END
            IF SCR.UD THEN GO STRT
            IF G60 THEN GO TOP
        CASE FG$ACT.CODE=FG$INS.LINE.CODE OR FG$ACT.CODE=FG$PASTE.CODE OR FG$ACT.CODE=FG$ADD.CODE
            GOSUB ADD_TO_UNDO
            IF FG$ACT.CODE=FG$ADD.CODE THEN
                IF LROW>(PDEPTH-2) THEN
                    IF CHANGED THEN GOSUB SCRN.TO.REC
                    GOSUB SCROLL.DOWN
                END ELSE ROW+=1; LROW+=1
            END
            CALL EB_PASTE(G60)
            IF G60 THEN GO TOP
        CASE FG$ACT.CODE=FG$ALT.CODE OR FG$ACT.CODE=FG$MENU.CODE
!      IF MOD(FG$STERM,3) THEN
            ECHO OFF
            INPUT FTYP
            ECHO ON
            IF NOT(NUM(FTYP)) THEN FTYP=OCONV(FTYP,'MCA')
!      END ELSE
!        CALL EB_AMEND.MENU(0,0,'HD','EB.MENU',STMP,FTYP,2)
!        SCR.LR=1
!      END
            FG$ACT.CODE=FG$ALT.CODE
            Y=FTYP[1,1]; FTYP=FTYP[2,MAX]
            BEGIN CASE
                CASE FTYP MATCHES "1N0N"
                    FG$ACT.CODE=FTYP; GO CHECK.CODES
                CASE FTYP='R'
                    CHR=REP.STR; FG$ACT.CODE=FALSE; GO CHECK.CODES
                CASE Y='O'
                    GO 1000
                CASE Y='S'
                    Y=FTYP[1,1]
                    GO 999
                CASE Y='H'
                    GO GET.HELP
            END CASE
        CASE FG$ACT.CODE=FG$OPT.CODE; GO 1000
        CASE FG$ACT.CODE=FG$NXTS.CODE
            SCRL=0
            IF CHANGED THEN GOSUB SCRN.TO.REC
            INDROW+=(PDEPTH-2)
            COL=5; ROW=1
            SCR.UD=TRUE
        CASE FG$ACT.CODE=FG$PRVS.CODE
            SCRL=0
            IF CHANGED THEN GOSUB SCRN.TO.REC
            INDROW-=(PDEPTH-2)
            IF INDROW<1 THEN INDROW=1
!            CRT @(0,0):CLEOP:
            COL=5; ROW=0
            SCR.UD=TRUE
        CASE FG$ACT.CODE=FG$GOTO.CODE
            CRT MSG.CLR:"Line Number ":
            L=20; Z=DFLT.LINE; INPTYPE='U'
            GOSUB INPT
            LNM=Z
            CRT MSG.AKN:
            IF FG$ACT.CODE=FG$BCK.CODE THEN LNM='<'
            IF LNM=ESC OR LNM='' THEN CRT MSG.DSP:
            IF CHANGED THEN GOSUB SCRN.TO.REC
            CRT MSG.DSP:
            Y=DCOUNT(MARKERS<1>,VM)
            IF NOT(NUM(LNM)) THEN
                IF LNM='<' THEN
                    Z=INDROW+ROW:SVM:LCOL+4
                    POS=Y
                    LOOP UNTIL MARKERS<1,POS>='<' AND MARKERS<2,POS>=Z OR POS=0 DO POS-=1 REPEAT
                    IF POS THEN
                        FOR I=POS TO Y
                            DEL MARKERS<1,POS>
                            DEL MARKERS<2,POS>
                        NEXT I
                        LOOP UNTIL MARKERS<1,POS>='<' OR POS=0 DO POS-=1 REPEAT
                        IF POS THEN
                            Y=MARKERS<2,POS,1>; COL=MARKERS<2,POS,2>
                            DEL MARKERS<1,POS>
                            DEL MARKERS<2,POS>
                        END
                    END
                END ELSE
                    LOCATE LNM IN MARKERS<1,vm_start> BY 'AL' SETTING POS THEN
                        Y=MARKERS<2,POS,1>; COL=MARKERS<2,POS,2>
                        IF LNM MATCHES "'R'1X0X" THEN
                            DEL MARKERS<1,POS>
                            DEL MARKERS<2,POS>
                            DFLT.LINE=MARKERS<1,1>
                        END
                    END ELSE POS=FALSE
                END
                IF POS THEN
                    SCR.UD=TRUE; OFFSET=0
                    ROW=Y-INDROW
                    IF ROW<0 OR ROW>PDEPTH THEN
                        INDROW=Y
                        ROW=0
                    END
                    GO STRT
                END ELSE LNM=INDROW
            END
            CNT=DCOUNT(REC,AM); IF LNM>CNT THEN LNM=CNT-(PDEPTH-2)
            IF LNM<1 THEN LNM=INDROW
            IF LNM>0 THEN INDROW=LNM; SCR.UD=TRUE; OFFSET=0; COL=5; ROW=0
        CASE FG$ACT.CODE=FG$INSERT.CODE
            INS.MODE=NOT(INS.MODE); CHR=''
            GOSUB SET.MSG.DSP
            CRT @(COL,ROW):
        CASE CHR=OTHER.MARGIN
!      IF OFFSET THEN OFFSET=0 ELSE OFFSET=PWIDTH-5
            OFFSET+=5
            SCR.LR=1; COL=5
        CASE FG$ACT.CODE=FG$FUNK.CODE
            GOSUB REV_UNDO
            GO TOP
        CASE FG$ACT.CODE=FG$IND.CODE
            GOSUB CHG.LROW
            GOSUB SCRN.TO.REC
            GOSUB INDENT
        CASE FG$ACT.CODE=FG$SUS.CODE
            RDSP(LROW)=REC<INDROW+ROW>
            CRT @(5,ROW):CLEOL:; CRTLN=RDSP(LROW);CRT.X=1+OFFSET;CRT.Y=PWIDTH-4; GOSUB CRT.LN
            CHANGES(LROW)=FALSE
        CASE FG$ACT.CODE=FG$TOP.CODE
            IF ROW#0 THEN
                ROW=0
            END ELSE
                IF CHANGED THEN GOSUB SCRN.TO.REC
                INDROW=1; OFFSET=0
                COL=5; ROW=0
                SCR.UD=TRUE
            END
        CASE FG$ACT.CODE=FG$BOT.CODE OR FG$ACT.CODE=FG$APP.CODE
            IF FG$ACT.CODE=FG$BOT.CODE AND ROW#(PDEPTH-2) THEN
                ROW=(PDEPTH-2)
            END ELSE
                SAVE.CODE=FG$ACT.CODE
                IF CHANGED THEN GOSUB SCRN.TO.REC
                FG$ACT.CODE=SAVE.CODE
                CALL EB_TRIM(REC,REC:'',AM,'T')
                CNT=DCOUNT(REC,AM)
                Y=INDROW
                INDROW=CNT-(PDEPTH-2)+(FG$ACT.CODE=FG$APP.CODE)
                IF INDROW<0 THEN INDROW=1
                COL=5
                ROW=(FG$ACT.CODE=FG$APP.CODE)*(PDEPTH-2)
                IF ROW>CNT THEN ROW=CNT
                SCR.UD=Y#INDROW
            END
        CASE FG$ACT.CODE=FG$SOL.CODE
            IF OFFSET THEN OFFSET=0; SCR.LR=1
            COL=5
        CASE FG$ACT.CODE=FG$EOL.CODE
GEOL:       !
            LCOL=LLEN
            CALL EB_TABCOL(RDSP(LROW),COL,LCOL,FALSE)
            COL+=1
            I=OFFSET
            IF OFFSET AND (COL>PWIDTH) THEN COL-=(PWIDTH-5)
            LOOP WHILE (COL>PWIDTH) DO
                LCOL-=(PWIDTH-5)
                OFFSET+=(PWIDTH-5)
                CALL EB_TABCOL(RDSP(LROW),COL,LCOL,FALSE)
            REPEAT
            IF I#OFFSET THEN
                LCOL++
                COL++
                SCR.LR=1
            END
        CASE FG$ACT.CODE=FG$DEL.WORD.CODE
            GOSUB ADD_TO_UNDO
            CALL EB_TABCOL(RDSP(LROW),COL,LCOL,TRUE)
            GOSUB GET.WORD
!            INS WORD BEFORE DEL.LIST<1>
            RDSP(LROW)=RDSP(LROW)[1,LCOL-1]:RDSP(LROW)[I,MAX]
            CRT @(5,ROW):CLEOL:; CRTLN=RDSP(LROW);CRT.X=1+OFFSET;CRT.Y=PWIDTH-4; GOSUB CRT.LN; CRT @(COL,ROW):
            GOSUB CHG.LROW
        CASE FG$ACT.CODE=FG$HLP.CODE
GET.HELP:   !
            GOSUB GET.WORD
            CALL EB_HELP(WORD,Z)
            IF Z THEN
                IF CHANGED THEN GOSUB SCRN.TO.REC
                SCR.LR=1
                CALL EB_REFRESH
            END
            IF OPEN.HELP THEN
                CALL EB_HELP('EB',0,0,0,0)
                INCLUDE EB.OS.INCLUDES PC.ON.CURSOR
                IF MOD(FG$STERM,3) ELSE SCR.UD=TRUE
            END
        CASE FG$ACT.CODE=FG$L.CASE.CODE
            GOSUB ADD_TO_UNDO
            INCLUDE EB.INCLUDES EB.CASE
        CASE FG$ACT.CODE=FG$ABT.CODE
            IF CHANGED THEN GOSUB SCRN.TO.REC
            GOSUB Abort
        CASE FG$ACT.CODE=FG$UNDEL.CODE
            GOSUB POP_UNDO
!            I=FALSE; Z=DEL.LIST<1>; GOSUB INS.TXT; DEL DEL.LIST<1>
            GO TOP
        CASE FG$ACT.CODE=FG$TAB.CODE
            TABLEN=ITAB<ITABPOS>
            IF TRIM(RDSP(LROW))#'' THEN
                GOSUB ADD_TO_UNDO
                TABSPC=SPACE(TABLEN)
                IF TAB.MODE THEN
                    TMP=TAB
                END ELSE
                    TMP=TABSPC
                END
                IF LCOL>LEN(RDSP(LROW)) THEN
                    RDSP(LROW):=TAB
                    LCOL+=1
                    CALL EB_TABCOL(RDSP(LROW),COL,LCOL,FALSE)
                END ELSE
                    IF INS.CHAR#'' THEN CRT @(COL,ROW):STR(INS.CHAR,TABLEN): ELSE
                        CRT @(COL,ROW):TABSPC:CLEOL:; CRTLN=RDSP(LROW);CRT.X=LCOL+OFFSET;CRT.Y=PWIDTH-1-COL; GOSUB CRT.LN
                    END
                    RDSP(LROW)=(RDSP(LROW)[1,LCOL-1]:TMP:RDSP(LROW)[LCOL,MAX])
                END
                LLEN+=LEN(TMP)
            END ELSE
                Y=LROW+(LROW<3)
                LOOP WHILE Y>2 AND RDSP(Y-1)[1,COMMENTLEN]=COMMENT DO Y-=1 REPEAT
                IF Y>1 THEN CHECK.LINE=RDSP(Y-1) ELSE CHECK.LINE=REC<INDROW-Y>
                IF TAB.MODE THEN CALL EB_TABS(CHECK.LINE,PWIDTH)
                LLEN=LEN(CHECK.LINE)
                DUMMY=CHECK.LINE; LNM=1; GOSUB FORMAT
                IF LCOL=I AND I>TABLEN THEN I-=TABLEN
                IF TAB.MODE THEN
                    RDSP(LROW)=STR(TAB,INT(I/TABLEN))
                END ELSE RDSP(LROW)=SPACE(I)
                LCOL=I
                CALL EB_TABCOL(RDSP(LROW),COL,LCOL,FALSE)
            END
            GOSUB CHG.LROW
            GO TOP
        CASE FG$ACT.CODE=FG$RFR.CODE
            IF CHANGED THEN GOSUB SCRN.TO.REC
            SCR.LR=1
        CASE FG$ACT.CODE=FG$TCL.CODE
            GOSUB TCL
        CASE CHR=REP.STR
            IF CHANGED THEN GOSUB SCRN.TO.REC
            SREC = REC
            GOSUB ADD_TO_UNDO
            CALL EB_GETRPL(MAT RPL.PARMS,MAT RPL.PROMPTS,MAT RPL.COLS)
            IF FG$ACT.CODE = FG$AMD.CODE THEN
                GOSUB SAVE.ITEM
                FG$ACT.CODE = FG$AMD.CODE
                GO NEXT.ITEM
            END
            IF REC = SREC THEN GOSUB POP_UNDO
            SREC = ''
        CASE FG$ACT.CODE=FG$JMP.CODE
            IF CHANGED THEN GOSUB SCRN.TO.REC
            LLEN1=LLEN+1
! first search for the next non-alpha character
            Y=LCOL+1; LOOP WHILE RDSP(LROW)[Y,1]=SPC DO Y+=1 REPEAT
            FOR I=Y TO LLEN1 UNTIL NOT(ICONV(RDSP(LROW)[I,1],PC)#'' OR RDSP(LROW)[I,1]='.'); NEXT I
            DUMMY=FIELD(TRIM(RDSP(LROW)[LCOL,I-LCOL]),SPC,1)
            IF DUMMY[1,2]='GO' THEN
                DUMMY=FIELD(TRIM(RDSP(LROW)[LCOL,MAX]),SPC,2)
                DUMMY=FIELD(DUMMY:';',';',1)
                DFLT.LINE='R':OCONV(DUMMY,'MCU')
                LOCATE DFLT.LINE IN MARKERS<1,vm_start> BY 'AL' SETTING POS ELSE
                    INS DFLT.LINE BEFORE MARKERS<1,POS>
                    INS '' BEFORE MARKERS<2,POS>
                END
                MARKERS<2,POS>=INDROW+ROW:SVM:LCOL+4
                INDROW=DCOUNT(REC[1,INDEX(REC,AM:LMARGIN:DUMMY,1)],AM)
                IF NOT(INDROW) THEN
                    INDROW = LAST.AM - PDEPTH + 3
                    IF NUM(DUMMY) THEN Y = '' ELSE Y = ':'
                    ROW = PDEPTH-2
                    REC<LAST.AM+1> = DUMMY:Y:' !'
                END ELSE ROW=1
                SCR.UD=TRUE
            END ELSE
                BEGIN CASE
                    CASE DUMMY='CALL' OR DUMMY='EXECUTE' OR DUMMY='PERFORM'
                        callopt = DUMMY EQ 'CALL'
                        DUMMY=FIELD(TRIM(RDSP(LROW)[LCOL,MAX]),SPC,2,99)
                        DUMMY=FIELD(DUMMY:'(','(',1)
                        DUMMY=FIELD(DUMMY:';',';',1)
                        IF NOT(callopt) THEN
                            DUMMY = FIELD(DUMMY, DUMMY[1,1], 2)
                        END
                        DUMMY := ' (!'
                    CASE INDEX(OCONV(DUMMY,'MCU'),'INCLUDE',1)
                        DUMMY=RDSP(LROW)[I+1,MAX]
                        CONVERT TAB TO SPC IN DUMMY
                        DUMMY=FIELD(DUMMY,SPC,1)
                        IF DUMMY#'' THEN
                            DUMMY=RDSP(LROW)[LCOL,MAX]
                            CONVERT TAB TO SPC IN DUMMY
                            CALL EB_READINCL(HEADERS, DUMMY:'', DUMMY, HEADER, FALSE)
                        END
                    CASE 1
                        READ tags FROM F.currdir,'tags' THEN
                            POS=INDEX(tags,DUMMY:TAB,1)
                        END ELSE POS = FALSE
                        IF POS THEN
                            DUMMY=tags[POS,99]
                            DUMMY=FIELD(DUMMY,TAB,3,99)
                            DUMMY=FIELD(DUMMY,DIR_DELIM_CH,2)
                            WRITE FG$MULTI.CODE:AM:DUMMY ON F.currdir,'eb_auto'
                            DUMMY=FIELD(tags[POS,99],TAB,2)
! Assume that the item names are all that matters
                            IF FIELD(DUMMY, DIR_DELIM_CH, DCOUNT(DUMMY, DIR_DELIM_CH))=FIELD(ITNM, DIR_DELIM_CH, DCOUNT(ITNM, DIR_DELIM_CH)) THEN
                                DUMMY=FIELD(DUMMY:';',';',1)
                                DFLT.LINE='R':OCONV(DUMMY,'MCU')
                                LOCATE DFLT.LINE IN MARKERS<1,vm_start> BY 'AL' SETTING POS ELSE
                                    INS DFLT.LINE BEFORE MARKERS<1,POS>
                                    INS '' BEFORE MARKERS<2,POS>
                                END
                                MARKERS<2,POS>=INDROW+ROW:SVM:LCOL+4
                                DUMMY=''
                            END ELSE DUMMY='.':DIR_DELIM_CH:DUMMY
                        END ELSE
                            DUMMY=FIELD(DUMMY:'(','(',1)
                            IO = EBJSHOW('-c ':DUMMY)
                            IF LEN(IO) = 0 THEN DUMMY = ''
                        END
                END CASE
                IF DUMMY#'' THEN
                    prog=FIELD(DUMMY,' ',1)
                    IF prog = 'RUN' THEN
                        prog=FIELD(DUMMY,' ',2)
                        DUMMY=DUMMY[COL2()+1, 99]
                    END ELSE
                        IF DCOUNT(DUMMY, ' ') = 1 THEN
                            prog=GET_CATALOG_FILE(prog)
                        END ELSE prog = ''
                    END
                    DUMMY='EB ':TRIM(prog:' ':DUMMY); GOSUB EB.SUB
                END
            END
        CASE FG$ACT.CODE=FG$LST.CODE
            CRT MSG.CLR:"Type a letter from A-Z":
            YNC=26; YNR=(PDEPTH-1); YNCHRS=SUB.CODES; YNL=1; GOSUB GET.CHAR
            IF FG$ACT.CODE THEN GO TOP
            Y=OCONV(Y,'MCU')
            LOCATE Y IN MARKERS<1,vm_start> BY 'AL' SETTING POS ELSE
                INS Y BEFORE MARKERS<1,POS>
                INS '' BEFORE MARKERS<2,POS>
            END
            MARKERS<2,POS>=INDROW+ROW:SVM:LCOL+4
        CASE FG$ACT.CODE=FG$PRV.KEY.CODE AND WCNT=ORIG_WCNT; CRT BELL:
        CASE FG$ACT.CODE=FG$NXT.KEY.CODE AND WCNT=NBR.WORDS; CRT BELL:
        CASE FG$ACT.CODE=FG$PRV.KEY.CODE OR FG$ACT.CODE=FG$NXT.KEY.CODE
            IF CHANGED THEN GOSUB SCRN.TO.REC
            IF FG$ACT.CODE=FG$PRV.KEY.CODE THEN
                WCNT-=2
                FG$ACT.CODE=FG$NXT.KEY.CODE
            END
            IF HEX.MODE THEN HEX.MODE=FALSE; GOSUB CONV.HEX
            IF ORIG.REC#REC THEN
                GOSUB FILE.ITEM
                FG$ACT.CODE=FG$NXT.KEY.CODE
                IF Y#'' THEN GO 10000
                IF ENCRYPTED='Y' THEN GOSUB ENCRYPT.IT ELSE GOSUB CHKSUM
            END ELSE
                RELEASE
                DELETE JET.PASTE,ITNM:'.sav'
            END
            GO NEXT.ITEM
        CASE FG$ACT.CODE=FG$MOUSE.CODE
            CALL EB_GETMOUSE(FG$TYPEAHEAD.BUFF, EVENT, C, R)
            IF R#'' THEN
                IF EVENT=SPC THEN ;! down
                    IF MOUSESTATE='' THEN
                        MOUSECOL=C
                        MOUSEROW=R
                        MOUSESTATE='LD'
                    END
                    IF C=MOUSECOL AND R=MOUSEROW THEN
                        BEGIN CASE
                            CASE MOUSESTATE='LD'
                                COL=C; ROW=R
                            CASE MOUSESTATE='LR'
                        END CASE
                    END
                    MOUSESTATE=''
                END ELSE
                    MOUSECOL=C
                    MOUSEROW=R
                    BEGIN CASE
                        CASE EVENT='#'          ;! left up
                            MOUSESTATE='LD'
                        CASE EVENT='"'          ;! rightt down
                            MOUSESTATE='RD'
                    END CASE
                END
            END
        CASE 1
            IF FG$ACT.CODE THEN CRT @(10,(PDEPTH-1)):FG$ACT.CODE:
            GO TOP
    END CASE
    GO STRT
    INCLUDE EB.INCLUDES CRT.LN
ADD.CHARS:!
    GOSUB ADD_TO_UNDO
    IF NOT(STRT) THEN
        CALL EB_TABCOL(RDSP(LROW),COL-LEN(NEW.CHARS),LCOL,TRUE)
        STRT=LCOL
    END
    LLEN=LEN(RDSP(LROW))
    IF LLEN<STRT AND NOT(TAB.MODE) THEN RDSP(LROW):=SPACE(STRT-LLEN)
    IF INS.MODE THEN LLEN=0 ELSE LLEN=LEN(NEW.CHARS)
    RDSP(LROW)=(RDSP(LROW)[1,STRT-1]:NEW.CHARS:RDSP(LROW)[STRT+LLEN,MAX])
    STRT=0; NEW.CHARS=''; GO CHG.LROW
SCROLL.DOWN: !
    Y=PDEPTH-1
    FOR I=1 TO Y
        RDSP(I)=RDSP(I+1)
    NEXT I
    INDROW+=1
    RDSP(PDEPTH)=REC<INDROW+(PDEPTH-1)>
    IF DEL.LINE#'' THEN CRT @(0,0):DEL.LINE:@(0,(PDEPTH-2)):INS.LINE: ELSE CRT @(PWIDTH,PDEPTH)
    I = INDROW+(PDEPTH-2)
    IF I > LAST.AM THEN
        DIMON = BG
        DIMOFF = FG
    END ELSE DIMON = ''; DIMOFF = ''
    CRT @(0,(PDEPTH-2)):CLEOL:DIMON:I 'R#4 ':DIMOFF:
    CRTLN=RDSP(PDEPTH-1);CRT.X=1+OFFSET;CRT.Y=PWIDTH-4
    GOSUB CRT.LN
    IF DEL.LINE='' THEN CRT MSG.DSP:
    Y=2
    RETURN
!==========
EB.SUB: !
    CALL EB_RSS(1)
    WRITE HEADERS ON F.currdir,'eb_headers'
    IF accuterm THEN CRT ESC:CHAR(2):0:
    EXECUTE DUMMY
    IF accuterm THEN CRT ESC:CHAR(2):1:
    CALL EB_RSS(0)
    IF MOD(FG$STERM,3) THEN
        CALL EB_STERM.MENU('EB.MENU','','',1,'')
        CRT CURS.ON:
        SCR.LR=DUMMY[1,2]='ED'
    END ELSE SCR.LR=1
    GOSUB LAST.USED
    RETURN
999 !
    IF ITNM[LEN(ITNM)-1,2] MATCHES "1N'%'" THEN
        IF CHANGED THEN GOSUB SCRN.TO.REC
        WRITE REC ON JET.PASTE,ITNM
        DELETE JET.PASTE,ITNM:'.sav'
        GO WRAPUP
    END ELSE
        IF FG$ACT.CODE#FG$ALT.CODE THEN
            IF UPDATES THEN
                YNCHRS='S'; Y = '(S)ave'
                Z = GETFLNM(FLNM)
                IF Z # 'tmp' AND Z # ('JET.PASTE]D'[1, LEN(Z)]) THEN
                    Y = "(F) to file, (E)ncrypt, ":Y
                    YNCHRS<1,-1>='F':VM:'E'
                END
                CRT MSG.CLR:Y:
            END ELSE
                CRT MSG.CLR:"Save (A)s, (C)ompile, e(X)it":
                YNCHRS='A':VM:'C':VM:'X'
            END
            IF YNCHRS#'S' THEN
                CRT ", (D) to delete, (N)ew item, (R)ename, (v)ersion Control":
                YNCHRS:=VM:'D':VM:'V':VM:'R':VM:'N':VM:ESC
            END
            YNC=COL; YNR=ROW
            YNL=1; GOSUB GET.CHAR
            CRT MSG.AKN:
            Y=OCONV(Y,"MCU")
        END
        IF Y='E' THEN Y='F'; ENCRYPTED='Y' ELSE ENCRYPTED=''
        BEGIN CASE
            CASE Y=ESC OR FG$ACT.CODE=FG$ABT.CODE
                CRT MSG.DSP:
                GO STRT
            CASE Y='V'
                LUK=FLNM:'*':EDIT.MODE:'*':ITNM
                CALL EB_VERSION(Y)
                IF Y THEN
                    READ LAST.EB FROM FG$EB.CONTROL,FG$LOGNAME:'.LAST.EB' THEN
                        LOOP
                            LOCATE LUK IN LAST.EB<1> SETTING POS THEN
                                DEL LAST.EB<1, POS>
                                DEL LAST.EB<2, POS>
                            END ELSE POS = FALSE
                        WHILE POS DO REPEAT
                        WRITE LAST.EB ON FG$EB.CONTROL,FG$LOGNAME:'.LAST.EB'
                    END
                    GOSUB SETUP.SWITCH
                    GOSUB SET.MSG
                    CRT MSG.DSP:
                    GO REREAD.ITEM
                END
                GO TOP
            CASE Y='C'
                Y='Y'
                GO 10000
            CASE Y='X'
                GO NEXT.ITEM
            CASE Y="F" OR Y='R' OR Y='A'
                IF CHANGED THEN GOSUB SCRN.TO.REC
                IF HEX.MODE THEN HEX.MODE=FALSE; GOSUB CONV.HEX
                IF CRLF.MODE THEN
                    REC = CHANGE(REC, AM, CR:LF)
                END
                CALL EB_TRIM(REC,REC:'',AM,'T')
                IF Y='R' OR Y='A' THEN
                    LOOP
                        CRT MSG.CLR:"Item Name? ":CLEOP:
                        L=40; Z=ITNM
                        GOSUB EDIT.INP
                    WHILE TRIM(Z)='' DO REPEAT
                    IF NOT(FG$ACT.CODE) AND Z#ESC AND Z#ITNM THEN
                        WRITE REC ON FIL,Z
                        IF Y='R' THEN
                            DELETE FIL,ITNM
                            YNC=FLNM:'*':EDIT.MODE:'*':Z
                            GOSUB LAST.USED
                            LAST.EB<1,1>=YNC
                            WRITE LAST.EB ON FG$EB.CONTROL,FG$LOGNAME:'.LAST.EB'
                            Z=YNC
                        END
                    END ELSE Z = ITNM
                END ELSE
                    IF ORIG.REC=REC THEN
                        CRT MSG.CLR:"Make patch? ":CLEOP:
                        YNC=15; YNR=(PDEPTH-1); YNCHRS='Y':VM:'N'; YNL=1; GOSUB GET.CHAR
                        CRT MSG.CLR:
                    END ELSE Y='Y'
                    IF Y='Y' THEN Z=ITNM ELSE Z=''
                END
                DELETE FIL,ITNM:".BAK"
                ITNM=Z
                IF ITNM#'' THEN
                    GOSUB FILE.ITEM
                    READ REC FROM FIL,ITNM ELSE REC=''
                    IF TYPE='BASIC' OR TYPE='RECOMPILE' OR TYPE='DEBUG' OR TYPE='SCRN' OR TYPE='SQL' THEN GO 10000 ELSE
                        IF ENCRYPTED='Y' THEN GOSUB ENCRYPT.IT ELSE GOSUB CHKSUM
                        CRT; GO NEXT.ITEM
                    END
                END ELSE CRT; GO NEXT.ITEM
            CASE Y="D"; GO 11000
            CASE Y='S'
                GOSUB SAVE.ITEM
                CRT
                GO NEXT.ITEM
            CASE Y='N'
                CRT MSG.CLR:'Not yet implemented':; RQM
        END CASE
        FG$ACT.CODE=FALSE
    END
    GO 999
!==================================================!
SAVE.ITEM: !
    IF CHANGED THEN GOSUB SCRN.TO.REC
    IF HEX.MODE THEN HEX.MODE=FALSE; GOSUB CONV.HEX
    CALL EB_TRIM(REC,REC:'',AM,'T')
    DELETE FIL,ITNM:".BAK"
    WRITE REC ON FIL,ITNM
    DELETE JET.PASTE,ITNM:'.sav'
    RETURN
!==================================================!
GET.CHAR: !
    CALL EB_UT_INPUT_ZERO(Y,MAT EB$CHARS,FG$ACT.CODE,YNC,YNR,FG$INPUT.CODES,YNCHRS,YNL,FG$TIMEOUT:AM:FG$MONITOR.SECS)
    RETURN
!==================================================!
INPT: ! Field entry and/or editing subroutine.
    POS=1
    EDITED=FALSE
!
EDIT.INP: ! Redisplay the field
!
    ECHO ON
    CALL EB_UT_WP(Z,INPTYPE,L,1,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',ESC)
    INPTYPE='AN'
    RETURN
CHG.LROW:
    INCLUDE EB.INCLUDES VERS_CTRL
    CHANGED=TRUE; CHANGES(LROW)=TRUE; RETURN
!==================================================!
1000 !
    IF FG$ACT.CODE=FG$ALT.CODE THEN
        FG$ACT.CODE=FALSE
    END ELSE
        CRT MSG.CLR:"<R>st,<M>ge,<D>up,<S>av,<E>d,Ed<V>al,<P>rt,Si<z>,<B>asicErrs,Ro<t>ate ?":
        YNC=COL; YNR=ROW;
        YNCHRS='.':VM:'A':VM:'B':VM:'C':VM:'D':VM:'E':VM:'F':VM:'G':VM:'H':VM:'I':VM:'K':VM:'M':VM:'N':VM:'O':VM:'P':VM:'R':VM:'S':VM:'T':VM:'U':VM:'V':VM:'W':VM:'X':VM:'Z'
        YNL=1; GOSUB GET.CHAR
        CRT MSG.DSP:
        IF FG$ACT.CODE=FG$OPT.CODE THEN Y='.'; FG$ACT.CODE=FALSE
        IF FG$ACT.CODE=FG$HLP.CODE THEN
            CALL EB_HELP('EBOPTS', Z)
            IF Z THEN
                IF CHANGED THEN GOSUB SCRN.TO.REC
                SCR.LR=1
                CALL EB_REFRESH
            END
        END
        IF FG$ACT.CODE THEN GO STRT
        FTYP=Y
        CRT MSG.AKN:
        FTYP=OCONV(FTYP,"MCU")
    END
    IF CHANGED THEN GOSUB SCRN.TO.REC
    BEGIN CASE
        CASE FTYP='.'
            GOSUB GET.PREVWORD
            CALL EB_SHOWMEMBERS(WORD)
            GOSUB CHG.LROW
            CRT MSG.DSP:; GO STRT
        CASE FTYP='A'   ;! Insert date/time
            Z=OCONV(DATE(),'D4'):SPC:OCONV(TIME(),'MTS'):SPC:UserName; I=TRUE; GOSUB INS.TXT
        CASE FTYP='B'
            IF DSPLY#'' AND ERR.NOS#COMPER THEN GO 2700 ELSE CRT MSG.CLR:'No errors ':PR:; INPUT FTYP:
        CASE FTYP='C'
            CALL EB_COMPARE(MAT RDSP,FIL,REC,CHANGED,MREC,POS,READ.AGAIN,LCOL,LROW,ROW,INDROW,PR,MSG.CLR,MSG.AKN,FLNM,MFLNM,ITNM,MITNM,DCT,MDCT)
            SCR.UD=TRUE ;!SCRL=0
            MREC=''
            CRT MSG.DSP:
            GO STRT
        CASE FTYP='D'; GO 7000    ;! duplicate line-replace
        CASE FTYP='E'
            Y='%':ITNM:'%'
            WRITE REC ON JET.PASTE,Y
            IF accuterm THEN CRT ESC:CHAR(2):0:
            ECHO ON
            DATA INDROW+LROW-1
            DUMMY=jbcreleasedir:DIR_DELIM_CH:'bin':DIR_DELIM_CH:'ED JET.PASTE ':Y; GOSUB EB.SUB
            ECHO OFF
            READ NEW.REC FROM JET.PASTE,Y THEN
                DELETE JET.PASTE,Y
                IF NEW.REC#REC THEN REC=NEW.REC; SCR.UD=TRUE
                NEW.REC=''
            END
            IF accuterm THEN CRT ESC:CHAR(2):1:
        CASE FTYP='F'
            GOSUB INDENT
        CASE FTYP='G'
            IF CHANGED THEN GOSUB SCRN.TO.REC
            TAB.MODE=NOT(TAB.MODE)
            SCR.UD=TRUE; CALL EB_REFRESH
        CASE FTYP='H'
            IF CHANGED THEN GOSUB SCRN.TO.REC
            CNT=DCOUNT(REC,AM)
            HEX.MODE=NOT(HEX.MODE)
            GOSUB CONV.HEX
            SCR.UD=TRUE; CALL EB_REFRESH
        CASE FTYP='I'
            DIM merge(3)
            EQU m.X TO merge(1)
            EQU m.Y TO merge(2)
            EQU m.Z TO merge(3)
            m.X='<<<<<<<'
            m.Y='======='
            m.Z='>>>>>>>'
            STMP=m.Z
            I=INDROW+ROW
            CHECK.LINE=REC<I>
            FOR m = 1 TO 3
                IF CHECK.LINE[1, LEN(merge(m))] EQ merge(m) THEN BREAK
            NEXT m
            IF m GT 3 THEN
                CRT MSG.CLR:'Position cursor on a merge line (e.g. <<<<)':
                GOSUB GET.CHAR
                GO STRT
            END
            CRT MSG.CLR:"<O>rigonal,<Y>ours,<T>heirs,<C>ompare ?":
            YNC=COL; YNR=ROW; YNCHRS='.':VM:'C':VM:'O':VM:'Y':VM:'T'; YNL=1; GOSUB GET.CHAR
            CRT MSG.DSP:
            IF FG$ACT.CODE THEN GO STRT
            CRT MSG.AKN:
            FTYP=INDEX('OYT',OCONV(Y,"MCU"),1)
! Find the ORIG line if we're not there
            LOOP
                IF CHECK.LINE[1, LEN(m.X)] EQ m.X THEN BREAK
            UNTIL I=1 DO
                I-=1
                CHECK.LINE=REC<I>
            REPEAT
            Y=I
            ROW=I-INDROW; SCRL=ROW
! We found ORIG so now delete up to the section we want
            STMP = m.Y
            m.Y = ''
            LOOP
                DEL REC<I>
                CHECK.LINE=REC<I>
                IF CHECK.LINE[1, LEN(STMP)] EQ STMP THEN BREAK
                m.Y<-1> = CHECK.LINE
            REPEAT
! Now found the end of this bit
            STMP = m.Z
            m.Z = ''
            LOOP
                DEL REC<I>
                CHECK.LINE=REC<I>
                IF CHECK.LINE[1, LEN(STMP)] EQ STMP THEN BREAK
                m.Z<-1> = CHECK.LINE
            REPEAT
            REC<I> = merge(FTYP)''
            CHANGED=FALSE; SCR.UD=TRUE
            GO STRT
!      CALL EB_CHOICES(STRT,12,'','','',EB.VARS,Z,1,'',1,'L#31','Variables')
!      IF Z#'' THEN I=TRUE; GOSUB INS.TXT
        CASE FTYP='K'
            Z=ITNM; I=TRUE; GOSUB INS.TXT
        CASE FTYP='O'
            CALL EB_MOVE(FIL,REC,MSG.CLR,MSG.AKN,PR,FLNM,MFLNM,ITNM,MITNM,DCT,MDCT)
            FG$ACT.CODE=FG$NXT.KEY.CODE
            GO NEXT.ITEM
        CASE FTYP='P'; CALL EB_PRINT
        CASE FTYP='M'; GO 5000    ;! merge
        CASE FTYP='R'
            IF ORIG.REC#REC THEN
                YNL='Restore original ':ITNM:' ? (Y/N) '
                CRT MSG.CLR:YNL:
                YNC=LEN(YNL); YNR=(PDEPTH-1); YNCHRS='Y':VM:'N':AM:AM:'Y'; YNL=1; GOSUB GET.CHAR
                CRT MSG.CLR:
                IF Y='Y' THEN
                    REC=ORIG.REC
                    SCR.UD=TRUE
                END
            END
        CASE FTYP='W'
            CRT MSG.CLR:'Changed item being filed. ':MSG.AKN:
            IF HEX.MODE THEN
                STMP=REC
                HEX.MODE=FALSE
                GOSUB CONV.HEX
                HEX.MODE=TRUE
            END
            WRITEU REC ON FIL,ITNM
            IF HEX.MODE THEN REC=STMP
            PREV.TIME=TIME()
        CASE FTYP='T'
            CALL EB_ROTATE(REC)
            SCR.UD=TRUE
        CASE FTYP='U'
            GOSUB UNINDENT
        CASE FTYP='V'
            STMP=RDSP(LROW)
            CONVERT VM:SVM TO AM:VM IN STMP
            Y='%':ITNM:'.':INDROW+LROW-1:'%'
            WRITE STMP ON JET.PASTE,Y
            DUMMY='EB JET.PASTE ':Y
            GOSUB EB.SUB
            READ STMP FROM JET.PASTE,Y ELSE NULL
            DELETE JET.PASTE,Y
            CONVERT VM:AM TO SVM:VM IN STMP
            IF STMP#RDSP(LROW) THEN RDSP(LROW)=STMP; GOSUB CHG.LROW
            IF MOD(FG$STERM,3) THEN
                CRT @(5,ROW):CLEOL:; CRTLN=RDSP(LROW);CRT.X=1+OFFSET;CRT.Y=PWIDTH-4; GOSUB CRT.LN
            END ELSE SCR.UD=1
        CASE FTYP='S'
            STMP=RDSP(LROW)
            CALL EB_SWAP(STMP,POS)
            IF POS THEN
                RDSP(LROW)=STMP
                GOSUB CHG.LROW
                CRT @(5,ROW):CLEOL:; CRTLN=RDSP(LROW);CRT.X=1+OFFSET;CRT.Y=PWIDTH-4; GOSUB CRT.LN
            END
        CASE FTYP='X'
!      GOSUB ABORT
            IF COL.80#'' AND COL.132#'' THEN
                IF PWIDTH=79 THEN
                    CRT COL.132:
                    PWIDTH=131
                END ELSE
                    CRT COL.80:
                    PWIDTH=79
                END
                IF CHANGED THEN GOSUB SCRN.TO.REC
                SCR.LR=1
            END
        CASE FTYP='Z'; GO 2500    ;! size
        CASE FTYP=ESC; NULL
    END CASE
    CRT MSG.DSP:
    GO STRT
!============
SPLIT.LINE: ! Break a line in two, at the cursor position.
! First move all subsequent lines down 1 to allow for the new line.
    IF CHANGED THEN GOSUB SCRN.TO.REC
    FOR I=(PDEPTH-1) TO LROW STEP -1
        RDSP(I+1)=RDSP(I)
    NEXT I
    INS RDSP(LROW) BEFORE REC<INDROW+ROW>
    CALL EB_MARKADJ(INDROW+ROW,1,1)
! This will have inserted an extra copy of the line to be split.
! Now discard the unwanted bits of each.
    RDSP(LROW)=RDSP(LROW)[1,LCOL-1]
    RDSP(LROW+1)=RDSP(LROW+1)[LCOL,MAX]
    DUMMY=RDSP(LROW); LNM=1; GOSUB FORMAT
    COL=I+4
    CALL EB_TRIM(TMP,RDSP(LROW+1),SPC,'L')
    IF FIELD(TMP,SPC,1)='ELSE' THEN
        TMP='END ':TMP
        IF I>3 THEN I-=ITAB<1>
    END
    RDSP(LROW+1)=SPACE(I-1):TMP
    CHANGES(LROW+1)=TRUE
    IF LROW<(PDEPTH-1) THEN
        SCR.LR=1-2*(INS.LINE#''); SCRL=ROW
        CRT CLEOL:@(0,ROW+1):INS.LINE:@(5,ROW+1):; CRTLN=RDSP(LROW+1);CRT.X=1;CRT.Y=PWIDTH-5; GOSUB CRT.LN
        GOSUB CHG.LROW
    END ELSE
        GOSUB CHG.LROW
        GOSUB SCRN.TO.REC; RETURN TO SCROLL.LINE
    END
    CRT MSG.DSP:
    RETURN
!============
2200 ! Join a line onto the end of preceeding or following line.
    CRT MSG.CLR:"Attach line ":INDROW+ROW:" to end of <P>receeding or <F>ollowing line? ":
    INPUT J.LINE,1:
    CRT MSG.AKN:
    J.LINE=OCONV(J.LINE,"MCU")
    BEGIN CASE
        CASE J.LINE="P"; J.LINE=-1
        CASE J.LINE="F"; J.LINE=1
        CASE J.LINE=ESC
            CRT MSG.DSP:
            GO STRT     ;! Abort function
        CASE 1; GO 2200
    END CASE
    CRT @(0,ROW+1):DEL.LINE:
! Now add current line onto end of selected line, with one space between.
2210 ! del char at end of line
    IF J.LINE<0 THEN
        CALL EB_TRIM(TMP,RDSP(LROW),SPC,'L')
    END ELSE
        CALL EB_TRIM(TMP,RDSP(LROW+J.LINE),SPC,'L')
        IF TAB.MODE THEN CALL EB_TRIM(TMP,TMP,TAB,'L')
    END
    NWORD = FIELD(TMP,SPC,1)
    BEGIN CASE
        CASE NWORD='END' ; TMP=TMP[COL2()+1,MAX]
        CASE (NWORD='CASE' OR NWORD='IF') AND TRIM(RDSP(LROW+J.LINE))#''
            TMP=TMP[COL2(),MAX]
            IF INDEX(TMP,'#',1) THEN TMP='AND':TMP ELSE TMP='OR':TMP
    END CASE
    IF J.LINE<0 THEN
        RDSP(LROW+J.LINE):=" ":TMP
    END ELSE
        COL = TRIM(LEN(RDSP(LROW))) + 5
        LCOL=LEN(RDSP(LROW+J.LINE))+1
        RDSP(LROW+J.LINE)=RDSP(LROW):SPC:TMP
    END
    IF TAB.MODE THEN
        CALL EB_TABCOL(RDSP(LROW+J.LINE),0,LCOL,FALSE)
        CRTLN=RDSP(LROW+J.LINE)
        CRT.X=1;CRT.Y=PWIDTH+1-COL
        CRT @(5,ROW):
    END ELSE
        CRT @(COL-DUMMY,ROW):SPC:CLEOL:
        CRTLN=TMP;CRT.X=1;CRT.Y=PWIDTH+1-COL
    END
    GOSUB CRT.LN
    CHANGES(LROW+J.LINE)=TRUE
! Then move all the other lines up 1 to fill up the gap.
    CALL EB_MARKADJ(INDROW+ROW,1,-1)
    DEL REC<INDROW+ROW>
    FOR I=LROW TO (PDEPTH-1)
        RDSP(I)=RDSP(I+1)
        CHANGES(I)=TRUE
    NEXT I
! Finally get another line from REC as the new 24thd line on the screen.
    RDSP(PDEPTH)=REC<INDROW+(PDEPTH-1)> ;! we use (PDEPTH-1) here as one line has been deleted from REC
    CHANGED=TRUE; CHANGES(PDEPTH)=TRUE; SCR.LR=1-2*(DEL.LINE#''); SCRL=ROW-1; IF SCRL<0 THEN SCRL=0
    CRT MSG.DSP:
    GO STRT
!==========! 4
2500 ! Size of record
    RLEN=LEN(REC)+LEN(ITNM)+6
    CRT MSG.CLR:"Record length is ":RLEN:" bytes, ":DCOUNT(REC,AM):' attributes'          ;!There are ":32267-RLEN:" bytes available.":
    GO STRT
!==========!
2600 ! Compare against another version
2700 ! Display errors from last compile
    CRT @(0,0):CLEOP:
    FOR I=1 TO NBR.DSPLY
        CRT DSPLY<I>
        IF MOD(I,22) ELSE
            CRT 'Press <return> to continue':
            INPUT FLD:
            IF OCONV(FLD,'MCU')='X' THEN I=NBR.DSPLY ELSE CRT @(0,0):CLEOP:
        END
    NEXT I
    CRT MSG.CLR:"That's all ! ":PR:; INPUT FLD:; CRT MSG.DSP:
    SCR.UD=TRUE; SSTR=''
    GO STRT
!============
TCL: !
    IF MOD(FG$STERM,3) ELSE SCR.LR=1; CRT @(-1)
    IF accuterm THEN CRT ESC:CHAR(2):0:
    CALL EB_RSS(1)
!  CALL EB_TCL
!  EXECUTE shell:'jsh'
    IF jutil_ctrl_pos THEN
        READ jsh_restore FROM JUTIL_CTRL, K.JUTIL_CTRL ELSE jsh_restore = ''
        jsh_prompt = ' "EB_jsh (ctrl-D) > "'
    END ELSE jsh_prompt = ''
    EXECUTE 'jsh -p':jsh_prompt
    IF jutil_ctrl_pos THEN
        IF LEN(jsh_restore) THEN
            WRITE jsh_restore ON JUTIL_CTRL, K.JUTIL_CTRL
        END ELSE
            DELETE JUTIL_CTRL, K.JUTIL_CTRL
        END
    END
    INCLUDE EB.OS.INCLUDES CLEARSELECT
    IF MOD(FG$STERM,3) THEN CALL EB_STERM.MENU('EB.MENU','','',1,'')
    IF accuterm THEN CRT ESC:CHAR(2):1:
    CALL EB_RSS(0)
    CRT CURS.ON:
    RETURN
!================
5000 ! Merge lines from an item
    CALL EB_MERGE(MAT RDSP,FIL,REC,CHANGED,MREC,POS,READ.AGAIN,LCOL,LROW,ROW,INDROW,PR,MSG.CLR,MSG.AKN,FLNM,MFLNM,ITNM,MITNM,DCT,MDCT)
    SCR.UD=TRUE; OFFSET=0; SCRL=0
    COL=5
    MREC=''
    CRT MSG.DSP:
    GO STRT
!====================
7000 ! Duplicate Line (insert)
    CRT MSG.DSP:
    IF CHANGED THEN GOSUB SCRN.TO.REC
    SCRL=REC<INDROW+ROW-1>
    BEGIN CASE
        CASE TRIM(SCRL) MATCHES "'EQU '0X" AND INDEX(SCRL,'(',1)
            Z=FIELD(SCRL,'(',1):'('; Y=SCRL[COL2()+1,MAX]
            SCRL=FIELD(Y,')',1); Y=Y[COL2(),MAX]
            IF NUM(SCRL) THEN SCRL+=1
            SCRL=Z:SCRL:Y
        CASE SCRL MATCHES "0X'<'1N0N','0X"
            Z=FIELD(TRIM(SCRL),SPC,1)
            IF INDEX(Z,'=',1) OR Z='INS' OR Z='DEL' THEN
                YNC="Increment attribute (Y/N) "
                CRT MSG.CLR:YNC:
                YNC=LEN(YNC); YNR=(PDEPTH-1); YNCHRS='Y':VM:'N'; YNL=1; GOSUB GET.CHAR
                CRT MSG.CLR:
                IF Y='Y' THEN
                    Z=FIELD(SCRL,'<',1):'<'; Y=SCRL[COL2()+1,MAX]
                    STMP=FIELD(Y,'>',1); Y=Y[COL2(),MAX]
                    SCRL=FIELD(STMP,',',1); Y=STMP[COL2(),MAX]:Y
                    IF NUM(SCRL) THEN SCRL+=1
                    SCRL=Z:SCRL:Y
                END
            END
        CASE SCRL[1,ITAB<1>]=SPACE(ITAB<1>)
            LOCATE FIELD(TRIM(SCRL),SPC,1) IN END.WORDS BY 'AL' SETTING POS THEN SCRL=SCRL[ITAB<1>+1,MAX]
    END CASE
    INS SCRL BEFORE REC<INDROW+ROW>
    CALL EB_MARKADJ(INDROW+ROW,1,1)
    SCR.UD=TRUE; SCRL=ROW
    GO STRT
!======================
10000 CRT
    IF INDEX('YBCO',Y[1,1],1) ELSE
        IF ENCRYPTED='Y' THEN GOSUB ENCRYPT.IT ELSE GOSUB CHKSUM
        REC=''; GO NEXT.ITEM
    END
    IF 0 THEN
        Z=Y
        CRT MSG.CLR:"<j>BASE,<P>rime ?":
        YNC=COL; YNR=ROW; YNCHRS='J':VM:'P'; YNL=1; GOSUB GET.CHAR
        CRT MSG.DSP:
        IF FG$ACT.CODE THEN GO NEXT.ITEM
        CRT MSG.AKN:
        BEGIN CASE
            CASE Y='P'
                rc = PUTENV('JBCEMULATE=prime')
            CASE Y='J'
                rc = PUTENV('JBCEMULATE=jbase')
        END CASE
        Y=Z
    END
10010 !
    IF UPDATES THEN WRITE REC ON FIL,ITNM
    INCLUDE EB.OS.INCLUDES BASIC.OPTS
    BEGIN CASE
        CASE ITNM[-4, 4] = '.src'
            TYPE='DL4'
            IF FLNM=currdir THEN
                DL4FNAME = '.'
            END ELSE
                DL4FNAME = FIELD(FLNM, DIR_DELIM_CH, DCOUNT(FLNM, DIR_DELIM_CH))
            END
            PCPERFORM 'jsh -s sh -c "loadsave -o temp.dl4 ':DL4FNAME:'/':ITNM:' 2>dl4.errs"' SETTING ERR.NOS
!            INCLUDE EB.OS.INCLUDES DL4.BASIC
            READ DSPLY FROM F.currdir,'dl4.errs' THEN
                DELETE F.currdir,'dl4.errs'
            END ELSE DSPLY = ''
            CONVERT CR TO '' IN DSPLY
            DSPLY = TRIM(DSPLY)
            NBR.DSPLY=DCOUNT(DSPLY,AM)
            FOR I=1 TO NBR.DSPLY UNTIL SYSTEM(14)
                CRT DSPLY<I>
            NEXT I
            CRT
            IF Y#'B' AND Y#'P' AND LEN(DSPLY) THEN
                TXT="re-edit"
                basloc=0
                LOOP
                    REMOVE basline FROM DSPLY AT basloc SETTING basdelim
                    INDROW=TRIM(FIELD(basline, ' ', 1))
                UNTIL INDROW MATCHES "1N0N" OR NOT(basdelim) REPEAT
                IF NOT(NUM(INDROW)) OR INDROW < 1 THEN INDROW = 1
                INDROW-=11
                COL=5
                ROW=11
                IF INDROW<1 THEN ROW=ROW+INDROW-1; INDROW=1
                CRT MSG.CLR:
                SCR.UD=1
            END
!            INCLUDE EB.OS.INCLUDES DL4.BASIC
        CASE TYPE='SQL' OR (TYPE='DEBUG' AND COMMENT='--')
            TYPE='SQL'
            LAST.AM=COUNT(REC,AM)
            Y=OCONV(REC<LAST.AM>,'MCU')
            MREC=REC
            IF Y#'SHOW ERRORS' THEN
                IF OCONV(REC<LAST.AM+1>,'MCU')='EXIT' THEN
                    INS 'SHOW ERRORS' BEFORE REC<LAST.AM+1>
                END ELSE
                    REC<-1>='SHOW ERRORS':AM:'EXIT'
                END
                WRITE REC ON FIL,ITNM
            END
            EXECUTE 'jSQL ':FLNM:SPC:ITNM CAPTURING DSPLY
            WRITE MREC ON FIL,ITNM
            INCLUDE EB.OS.INCLUDES EB.SQL
        CASE 1
            BP.FILE=FLNM; SCR.VALIDATE=ITNM
            INCLUDE EB.OS.INCLUDES DECATALOG
            IF Y[2,1]='F' THEN BAS.ARGS:='OF'
            Y=Y[1,1]
            IF Y='B' THEN
                BAS.ARGS:='C'
                IF ENCRYPTED='Y' THEN BAS.ARGS:='X' ELSE BAS.ARGS:='K'
                EXECUTE BACKGROUND.VERB:SPC:COMPILE.VERB:SPC:FLNM:SPC:ITNM:BAS.ARGS CAPTURING DSPLY RETURNING ERR.NOS
            END ELSE
                IF INDEX('YO',Y,1) THEN
                    CRT 'Compiling...'
                    IF NOT(GETENV('JEDIFILEPATH',jedifilepath)) THEN
                        jedifilepath = '.'
                    END
                    IF INDEX(jedifilepath,'/',1) THEN dirsep=':' ELSE dirsep=';'
                    CONVERT dirsep TO @AM IN jedifilepath
                    IF GETENV('INCLUDE',includepath) ELSE includepath='.'
                    IF INDEX(includepath,'/',1) THEN dirsep=':' ELSE dirsep=';'
                    incpath=includepath
                    CONVERT dirsep TO @AM IN incpath
                    jedifilepath<-1>=incpath
                    loc=0
                    LOOP
                        REMOVE inc FROM jedifilepath AT loc SETTING delim
                        IF NOT(INDEX(inc, SPC, 1)) AND LEN(inc) THEN BAS.ARGS:=' -I':inc
                    WHILE delim DO REPEAT
                    IF DIR_DELIM_CH = '\' THEN
                        FULLPATH = CHANGE(FLNM, '\', '\\')
                    END ELSE FULLPATH=FLNM
                    EXECUTE COMPILE.VERB:BAS.ARGS:SPC:FULLPATH:SPC:ITNM:SPC:BAS.OPTS:shellend CAPTURING DSPLY RETURNING ERR.NOS
                    INCLUDE EB.OS.INCLUDES RTED.BASIC
                END
            END
    END CASE
    IF UPDATES THEN
        IF NOT(SCR.UD) AND ENCRYPTED='Y' THEN
            GOSUB ENCRYPT.IT
        END ELSE
            WRITE REC ON FIL,ITNM
        END
    END
    DELETE JET.PASTE,'%':ITNM:'%'
    DELETE FIL,'%':ITNM:'%'
    IF SCR.UD=1 THEN GO STRT
    IF ENCRYPTED='Y' OR TYPE='DEBUG' ELSE GOSUB CHKSUM
    IF TYPE='BASIC' OR TYPE='RECOMPILE' OR TYPE='DEBUG' AND NOT(INDEX('BO',Y,1)) THEN
        IF FG$OSTYPE='JB' THEN
            LOCATE FLNM IN CATL.LIST<1> SETTING FPOS ELSE
                INS FLNM BEFORE CATL.LIST<1,FPOS>
                INS '' BEFORE CATL.LIST<2,FPOS>
            END
            LOCATE ITNM IN CATL.LIST<2, FPOS> SETTING IPOS ELSE
                INS ITNM BEFORE CATL.LIST<2, FPOS, IPOS>
            END
        END ELSE
            INCLUDE EB.OS.INCLUDES CATALOG
            EXECUTE CATALOG.CMD
        END
    END
    GO NEXT.ITEM
!==============================================
INS.TXT: !
    IF LCOL>1 THEN
        HASH='L#':LCOL-1
        Y=RDSP(LROW)[1,LCOL-1] HASH
    END ELSE Y=''
    RDSP(LROW)=Y:Z:RDSP(LROW)[LCOL,MAX]
    Y=LEN(Z)
    GOSUB CHG.LROW; LLEN+=Y
    CRT @(COL,ROW):; CRTLN=RDSP(LROW);CRT.X=LCOL;CRT.Y=PWIDTH-COL; GOSUB CRT.LN
    IF I THEN
        LCOL+=Y
        CALL EB_TABCOL(RDSP(LROW),COL,LCOL,FALSE)
    END
    RETURN
FORMAT: !
    CALL EB_FORMAT(DUMMY,I,LNM)
    RETURN
INDENT: !
    Y='%':FLNM:'%':ITNM:'%'
    WRITE REC ON JET.PASTE,Y
    CRT MSG.CLR:'Formatting program...':
    ECHO OFF
    DUMMY='jEDIfmt JET.PASTE ':Y
    IF TYPE='SQL' THEN DUMMY:=' (Q'
    EXECUTE DUMMY
    ECHO ON
    READ REC FROM JET.PASTE,Y ELSE NULL
    DELETE JET.PASTE,Y
    SCR.UD=TRUE
    RETURN
UNINDENT: !
    Y='%':ITNM:'%'
    WRITE REC ON JET.PASTE,Y
    CRT MSG.CLR:'Unformatting program...':
    ECHO OFF
    EXECUTE 'EBUFMT JET.PASTE ':Y
    ECHO ON
    READ REC FROM JET.PASTE,Y ELSE NULL
    DELETE JET.PASTE,Y
    SCR.UD=TRUE
    RETURN
BACK.WORD:!
    LCOL-=1
    FOR I=LCOL TO 1 STEP -1 UNTIL ICONV(RDSP(LROW)[I,1],PC)#''; NEXT I
! first search for the previous non-alpha character
    LCOL=I
    FOR I=LCOL TO 1 STEP -1 UNTIL RDSP(LROW)[I,1]=SPC OR NOT(ICONV(RDSP(LROW)[I,1],PC)#''); NEXT I
    LCOL=I+1
    IF OFFSET AND LCOL-OFFSET<0 THEN
        OFFSET-=20; SCR.LR=1
        IF OFFSET<0 THEN OFFSET=0
    END
    CALL EB_TABCOL(RDSP(LROW),COL,LCOL,FALSE)
    CALL EB_TABCOL(RDSP(LROW),COL,LCOL,TRUE)
    RETURN
GET.WORD: !
    LLEN1=LLEN+1
! first search for the next non-alpha character
    I=LCOL
    IF INDEX(TAB:SPC,RDSP(LROW)[I,1],1) THEN
        LOOP
            I+=1
            Y=RDSP(LROW)[I,1]
        UNTIL Y='' OR NOT(INDEX(TAB:SPC,RDSP(LROW)[I,1],1)) DO REPEAT
    END ELSE
        IF ICONV(RDSP(LROW)[LCOL,1],PC)#'' THEN
            LOOP WHILE ICONV(RDSP(LROW)[I,1],PC)#'' DO I+=1 REPEAT
            IF INDEX(TAB:SPC,RDSP(LROW)[I,1],1) THEN I+=1
        END ELSE
            LOOP Y=RDSP(LROW)[I,1] UNTIL Y='' OR ICONV(Y,PC)#'' DO I+=1 REPEAT
        END
    END
    WORD=RDSP(LROW)[LCOL,I-LCOL]
    RETURN
GET.PREVWORD: !
    SLCOL=LCOL
    SCOL=COL
    GOSUB BACK.WORD
    GOSUB GET.WORD
    LCOL=SLCOL
    COL=SCOL
    RETURN
    LLEN1=LLEN+1
! first search for the next non-alpha character
    I=LCOL
    IF INDEX(TAB:SPC,RDSP(LROW)[I,1],1) THEN
        LOOP
            I-=1
            Y=RDSP(LROW)[I,1]
        UNTIL I<2 OR NOT(INDEX(TAB:SPC,RDSP(LROW)[I,1],1)) DO REPEAT
    END ELSE
        IF ICONV(RDSP(LROW)[LCOL,1],PC)#'' THEN
            LOOP UNTIL ICONV(RDSP(LROW)[I,1],PC)='' OR I=1 DO I-=1 REPEAT
            IF INDEX(TAB:SPC,RDSP(LROW)[I,1],1) THEN I-=1
        END ELSE
            LOOP Y=RDSP(LROW)[I,1] UNTIL I<2 OR ICONV(Y,PC)='' DO I-=1 REPEAT
        END
    END
    WORD=RDSP(LROW)[I, LCOL-I]
    RETURN
11000 !
    CRT MSG.CLR:"To delete the item, type in the word DELETE, else press <RETURN> ":
    Z=''; L=7; GOSUB INPT; Y=Z
    IF Y#"DELETE" THEN GO 999
!
    Z = FALSE
    OPEN FLNM:',OBJECT' TO F.BP THEN
        Z = TRUE
    END ELSE
        IF FLNM 'R#2' = 'BP' THEN
            F.BP = FIL
            Z = TRUE
        END
    END
    IF Z THEN
        CLOSE F.BP
        CALL SPLITFILEPATH(FLNM, Y, Z)
        Z := SPC:ITNM
        YNL='Decatalog ':Z:' ? (Y/N) '
        CRT MSG.CLR:YNL:
        YNC=LEN(YNL); YNR=(PDEPTH-1); YNCHRS='Y':VM:'N':AM:AM:'Y'; YNL=1; GOSUB GET.CHAR
        CRT MSG.CLR:
        IF Y='Y' THEN
            PROG = ITNM
            GOSUB GET.CATL
            GOSUB PARSE.CATL
            EXECUTE "DECATALOG ":CAT.OPTIONS:SPC:Z
        END
    END
!
    RELEASE FIL,ITNM
    Z = SVN_DELETE(TRUE, FLNM, ITNM)
    Y = (FIELD(Z,SPC,1) = 'D')
    IF LEN(Z) = 0 OR Y THEN
        Z = ITNM:" deleted!"
        IF Y THEN
            Z := ' Awaiting commit to repository'
        END
    END
    CRT MSG.CLR:Z:
    Z = ''; L=1; GOSUB INPT
    GO NEXT.ITEM
Abort: !
    FG$ACT.CODE = FALSE
    changed = (ORIG.REC#REC)
    IF changed THEN
        Z = 'Are you sure you want to'
    END ELSE
        IF SYSTEM(16) OR NOT(ConfirmExit) THEN
            Z = ''
            Y = 'Y'
        END ELSE Z = 'Confirm'
    END
    IF LEN(Z) THEN
        Z := " exit (Y/N/FI{B{C}}? "
        CRT MSG.CLR:Z:
        YNC=LEN(Z); YNR=(PDEPTH-1); YNCHRS='Y':VM:'N':VM:'F'; YNL=1; GOSUB GET.CHAR
    END
    IF Y = 'Y' THEN
        IF NOT(changed) AND VersCheckedOut>0 THEN
            CALL EB_VERS_CTRL(VersRevert,lockvar, FALSE)
        END
    END
    BEGIN CASE
        CASE Y='F'
            Z='F'; INPTYPE='U':AM:AM:2; L=4; GOSUB INPT
            BEGIN CASE
                CASE Z='FI' ; FG$TYPEAHEAD.BUFF='N'
                CASE Z='FIB' ; FG$TYPEAHEAD.BUFF='O'
                CASE Z='FIBC' ; FG$TYPEAHEAD.BUFF='Y'
                CASE Z='FIC' ; FG$TYPEAHEAD.BUFF='C'
            END CASE
            FG$TYPEAHEAD.BUFF:=CR
            FG$ACT.CODE=FG$END.CODE
            X = OPEN.PATCH
            IF NOT(changed) THEN
                OPEN.PATCH = FALSE
            END
            IF INDEX(Z, 'C', 1) THEN
                CALL EB_FILE(X,K.PATCHFILE,MAT PATCH,Y,ENCRYPTED,UPG)
                GOSUB 10010
            END ELSE
                GOSUB FILE.ITEM
            END
            OPEN.PATCH = X
            RETURN TO NEXT.ITEM
        CASE Y='Y' OR (Y=ESC AND NOT(changed))
            RELEASE     ;! shh don't tell Dan
            DELETE JET.PASTE,ITNM:'.sav'
            RETURN TO NEXT.ITEM
        CASE Y='N' OR FG$ACT.CODE
            CRT MSG.DSP:
    END CASE
    RETURN
FILE.ITEM:!
    CALL EB_FILE(SKIP.PATCH,K.PATCHFILE,MAT PATCH,Y,ENCRYPTED,UPG)
    IF SKIP.PATCH THEN
        IF ENCRYPTED='Y' THEN GOSUB ENCRYPT.IT
        RETURN TO STRT
    END
    RETURN
ENCRYPT.IT: !
    IF PASSWD='' THEN CALL UPGPASSWD (PASSWD,F.UPG.WORKFILE)
    CALL UPGCONVERT(FLNM,ITNM,REC,'E',USEMODE,F.UPG.WORKFILE,PASSWD,CONVOK)
    IF NOT(CONVOK) THEN
        CRT 'Error encrypting'
    END ELSE
        WRITE REC ON FIL,ITNM
    END
    RETURN
CHKSUM: !
    RETURN
    IF UPG ELSE RETURN
    CALL UPGFILETYPE(FLNM,FILETYPE)
    IF FILETYPE='BP' OR FILETYPE='INCL' THEN
        IF REC<1,1>=ENCRYPT.MESG<1> THEN
            FOR I=1 TO ENCRYPT.HEADCNT; DEL REC<1,1>; NEXT I
        END
        IF FG$OSTYPE='AP' THEN
            WRITE REC ON FIL,ITNM
            CRT MSG.CLR:'Calculating check-sum in background...':
            EXECUTE 'Z CHKSUM ':FLNM:SPC:ITNM
        END ELSE
            CALL UPGCHKSUM(REC,CHKSUM,SIZE,F.UPG.WORKFILE,USEMODE,FILETYPE,VERSION)
            MESG=ENCRYPT.MESG
            MESG<3>=MESG<3>:CHKSUM
            MESG<4>=MESG<4>:SIZE
            MESG<5>=MESG<5>:VERSION
            MESG<6>=MESG<6>:TRIM(TIMEDATE())
            MESG<7>=MESG<7>:FILETYPE
            REC<-1>=MESG
            WRITE REC ON FIL,ITNM
        END
    END
    RETURN
CONV.HEX: !
    IF HEX.MODE THEN
        CRT MSG.CLR:'Converting to HEX...'
        FOR I=1 TO CNT
            REC<I>=OCONV(REC<I>,'MX')
        NEXT I
    END ELSE
        CRT MSG.CLR:'Converting to ASCII...'
        FOR I=1 TO CNT
            REC<I>=ICONV(REC<I>,'MX')
        NEXT I
    END
    RETURN
GET.EDIT.MODE: !
    BEGIN CASE
        CASE ITNM 'R#4'='.sql'; EDIT.MODE='Q'
        CASE ITNM 'R#4'='.cpp'; EDIT.MODE='cpp'
        CASE ITNM 'R#4'='.sqc'; EDIT.MODE='c'
        CASE ITNM 'R#5'='.java'; EDIT.MODE='c'
        CASE ITNM 'R#3'='.pc'; EDIT.MODE='c'
        CASE ITNM 'R#2'='.c'; EDIT.MODE='c'
        CASE ITNM 'R#2'='.b'; EDIT.MODE='!'
        CASE ITNM 'R#6'='.jabba'; EDIT.MODE='!'
        CASE FLNM 'R#2'='BP'; EDIT.MODE='!'
        CASE FLNM='MD' OR FLNM='VOC'; EDIT.MODE='C'
        CASE 1
            CALL EB_OPEN('',FLNM:',OBJECT', F.OBJECT, FALSE, POS)
            IF POS THEN
                CLOSE F.OBJECT
                EDIT.MODE = '!'
            END
    END CASE
    RETURN
SET.MODE: !
    PC='P(1N);(1A)'
    BEGIN CASE
        CASE COUNT(EDIT.MODE,'C')
            COMMENT='C '
            COMMENTLEN=2
            TYPE='PROC'
            ITABPOS=1
            PC:=';("_")'
        CASE COUNT(EDIT.MODE,'Q')
            COMMENT='--'
            COMMENTLEN=2
            TYPE='SQL'
            ITABPOS=3
            PC:=';("_")'
        CASE COUNT(EDIT.MODE,'S')
            COMMENT='# '
            COMMENTLEN=2
            TYPE='SCRIPT'
            ITABPOS=2
            PC:=';("_")'
        CASE COUNT(EDIT.MODE,'*')
            COMMENT='*'
            COMMENTLEN=1
            TYPE='BASIC'
            ITABPOS=1
            PC:=';("_");(".")'
        CASE COUNT(EDIT.MODE,'cpp')
            COMMENT='//'
            COMMENTLEN=2
            TYPE='C++'
            ITABPOS=2
            PC:=';("_")'
        CASE COUNT(EDIT.MODE,'c')
            COMMENT='/*':@SVM:'*/'
            COMMENTLEN=2
            TYPE='C'
            ITABPOS=2
            PC:=';("_")'
        CASE 1
            COMMENT='!'
            COMMENTLEN=1
            TYPE='BASIC'
            ITABPOS=2
            PC:=';("_");(".")'
    END CASE
    BEGIN CASE
        CASE COUNT(EDIT.MODE,'c')
            reservedWords = cReserved
            comments='//':@VM:'/*'          ;! crude for now
            comments<-1>=@VM:'*/'           ;! end of comment
        CASE 1
            reservedWords = jbcReserved
            comments='!':@VM:'*'
    END CASE
    commentlen=COMMENTLEN
    RETURN
LAST.USED:!
    IF ITNM[LEN(ITNM)-1,2] MATCHES "1N'%'" THEN RETURN
    IF (ITNM 'R#4')='.tmp' THEN RETURN
    IF tempItem THEN RETURN
    READ LAST.EB FROM FG$EB.CONTROL,FG$LOGNAME:'.LAST.EB' ELSE LAST.EB=''
    LUK=FLNM:'**':ITNM
    LOCATE LUK IN LAST.EB<1,vm_start> SETTING POS THEN
        DEL LAST.EB<1,POS>
        DEL LAST.EB<2,POS>
    END
    LUK=FLNM:'*':EDIT.MODE:'*':ITNM
    LOCATE LUK IN LAST.EB<1,vm_start> SETTING POS THEN
        DEL LAST.EB<1,POS>
        DEL LAST.EB<2,POS>
    END
    INS LUK BEFORE LAST.EB<1,1>
    INS COL:SVM:ROW:SVM:INDROW:SVM:OFFSET BEFORE LAST.EB<2,1>
    I = DCOUNT(LAST.EB<1>, VM)
    LOOP WHILE I > 500 DO
        DEL LAST.EB<1, I>
        DEL LAST.EB<2, I>
        I--
    REPEAT
    WRITE LAST.EB ON FG$EB.CONTROL,FG$LOGNAME:'.LAST.EB'
    RETURN
SET.MSG: !
    MSG.DFLT= (FLNM:'/':ITNM) 'R#45 Started: ':OCONV(PSTIME,'MTS')
SET.MSG.DSP:
    MSG.DSP=MSG.DFLT:' (Col=   )'
    IF NBR.WORDS GT 3 THEN
        IDPOS = '(':WCNT-2:'/':NBR.WORDS-2:')'
        MSG.DSP[1,LEN(IDPOS)]=IDPOS
    END
    MSG.COL=LEN(MSG.DSP)-4
    MSG.DSP=MSG.CLR:MSG.DSP
    IF INS.MODE THEN
        MSG.DSP:=CURS.INS
        CRT CURS.INS:
    END ELSE
        MSG.DSP:=CURS.RPL
        CRT CURS.RPL:
    END
    RETURN
SETUP.SWITCH: !
    CALL EB_SETUPSWITCH(HFLNM, SFLNM)
    RETURN
SWITCH.FILE: !
    IF FLNM = HFLNM THEN
        FIL = SFIL
        FLNM = SFLNM
    END ELSE
        FIL = HFIL
        FLNM = HFLNM
    END
    RETURN
GET.CATL: !
    CALL EB_TRIM(firstProg, PROG, '.b', 'T')
    CALL EB_TRIM(firstProg, PROG, '.jabba', 'T')
    FLNM.CAT.OPTIONS = EBJSHOW('-c ':firstProg)
    IF NOT(LEN(FLNM.CAT.OPTIONS)) AND firstProg EQ UPCASE(firstProg) THEN
        FLNM.CAT.OPTIONS = EBJSHOW('-c ':LOWCASE(firstProg))
    END
    IF LEN(FLNM.CAT.OPTIONS) THEN
        SOP = @FALSE
        POS = INDEX(FLNM.CAT.OPTIONS, 'Executable:', 1)
        IF NOT(POS) THEN
            POS = INDEX(FLNM.CAT.OPTIONS, 'Subroutine:', 1)
            SOP = POS
        END
        IF POS THEN
            POS = DCOUNT(FLNM.CAT.OPTIONS[1,POS], @AM)
            IF SOP THEN
                A = DCOUNT(FLNM.CAT.OPTIONS, @AM)
                POS--
                LOOP
                    POS++
                    LINE1 = FLNM.CAT.OPTIONS<POS>
                    SOP = INDEX(LINE1, DIR_DELIM_CH:'lib', 1)
                UNTIL SOP OR POS = A DO REPEAT
                IF NOT(SOP) THEN POS = FALSE
            END
        END
        IF POS THEN
            FLNM.CAT.OPTIONS = FLNM.CAT.OPTIONS<POS>
            FLNM.CAT.OPTIONS = TRIM(FIELD(FLNM.CAT.OPTIONS, ':',DCOUNT(FLNM.CAT.OPTIONS, ':')))
            FLNM.CAT.OPTIONS = FIELD(FLNM.CAT.OPTIONS, DIR_DELIM_CH, 1, COUNT(FLNM.CAT.OPTIONS, DIR_DELIM_CH)-1)
            FLNM.CAT.OPTIONS = '-L':FLNM.CAT.OPTIONS:DIR_DELIM_CH:'lib':@AM:'-o':FLNM.CAT.OPTIONS:DIR_DELIM_CH:'bin'
        END ELSE FLNM.CAT.OPTIONS = ''
        CRT MSG.CLR:'CATALOG ':CHANGE(FLNM.CAT.OPTIONS, @AM, ' | '):' ':
    END
    RETURN
PARSE.CATL: !
    CAT.OPTIONS = FLNM.CAT.OPTIONS
    IF CAT.OPTIONS#'' THEN
        READ CAT.OPTIONS FROM FG$EB.PARAMS,FLNM:'_':PROG:'_lib' ELSE CAT.OPTIONS = FLNM.CAT.OPTIONS
    END
    IF LEN(CAT.OPTIONS) THEN
        A = 1
        LOOP
            LINE1 = TRIM(REC<A>)
        WHILE INDEX('!*', LINE1[1,1], 1) AND LINE1 NE '' DO A++ REPEAT
        fword = FIELD(LINE1,' ',1)
        IF fword EQ 'SUBROUTINE' OR fword EQ 'FUNCTION' THEN
            CAT.OPTIONS=CAT.OPTIONS<1>
        END ELSE CAT.OPTIONS=CAT.OPTIONS<2>
    END ELSE CAT.OPTIONS = ' '
    RETURN
ADD_TO_UNDO:
    GOSUB UPDATE.REC
PRE_ADD_TO_UNDO:
    SREC = RAISE(UNDO_STACK<1>)
    DEL SREC<1>
    IF REC # SREC THEN
        INS COL:SVM:ROW:SVM:INDROW:VM:LOWER(REC) BEFORE UNDO_STACK<1>
    END
    RETURN
POP_UNDO:
    IF DCOUNT(UNDO_STACK, @AM) GT UNDO_POS THEN
        UNDO_POS++
        GOSUB DO_UNDO
    END ELSE CRT BELL:
    RETURN
REV_UNDO:
    IF UNDO_POS GT 1 THEN
        UNDO_POS--
        GOSUB DO_UNDO
    END ELSE CRT BELL:
    RETURN
DO_UNDO:
    REC = RAISE(UNDO_STACK<UNDO_POS>)
    COL=REC<1,1>; ROW = REC<1,2>; INDROW = REC<1,3>; DEL REC<1>
    SCR.UD = 1
    CALL EB_REFRESH
    RETURN
WRAPUP: !
    IF MOD(FG$STERM,3) THEN
        CALL EB_STERM.MENU('EB.MENU','','',-1,'')
        CALL EB_AT.WINDOW.CLOSE(1)
    END ELSE CRT @(0,PDEPTH)
    IF accuterm THEN CRT ESC:CHAR(2):0:
    ECHO ON
    IF COL.80#'' AND COL.132#'' THEN
        IF PWIDTH=131 THEN
            CRT COL.80:
        END
    END
    INCLUDE EB.OS.INCLUDES TIMEOUT.OFF
    INCLUDE EB.OS.INCLUDES PC.BLOCK.CURSOR
    CALL EB_RSS(1)
    IF CATL.LIST#'' THEN
        NBR.FILES = DCOUNT(CATL.LIST<1>, VM)
        FOR F = 1 TO NBR.FILES
            FLNM = CATL.LIST<1, F>
            FULLFLNM=FLNM
            IF INDEX(FLNM,DIR_DELIM_CH,1) THEN
                FLNM = FIELD(FLNM,DIR_DELIM_CH,DCOUNT(FLNM,DIR_DELIM_CH))
                FLNMO = GETFULLPATH(FLNM)
                IF FIELD(TRIM(FLNMO),SPC,2)#FULLFLNM THEN FLNM=FULLFLNM
            END
            ORIG_PATH = SVN_GETORIGPATH(FLNM)
            Repository = FIELD(SVN_GET_REPOSITORY(FLNM), '/', 1)
            ORIG_PATH = ORIG_PATH[INDEX(ORIG_PATH, DIR_DELIM_CH:Repository, 1) + 1, MAX]
            PROGS = CATL.LIST<2, F>
            READ FLNM.CAT.OPTIONS FROM FG$EB.PARAMS,ORIG_PATH:'_lib' ELSE
                PROG = PROGS<1,1,1>
                GOSUB GET.CATL
            END
            NBR.PROGS = DCOUNT(PROGS, @SVM)
            CAT.OPTS = ''
            FOR P = 1 TO NBR.PROGS
                PROG = PROGS<1, 1, P>
                GOSUB PARSE.CATL
                LOCATE CAT.OPTIONS IN CAT.OPTS<1> SETTING POS THEN
                    CAT.OPTS<2,POS,-1> = PROG
                END ELSE
                    INS CAT.OPTIONS BEFORE CAT.OPTS<1,POS>
                    INS PROG BEFORE CAT.OPTS<2,POS>
                END
            NEXT P
            NBR.CATS = DCOUNT(CAT.OPTS<1>, @VM)
            FOR C = 1 TO NBR.CATS
                PROGS = CHANGE(CAT.OPTS<2, C>, @SVM, SPC)
                CAT.OPTIONS = CAT.OPTS<1, C>
                EXECUTE TRIM(CATALOG.VERB:SPC:CAT.OPTIONS):SPC:FULLFLNM:SPC:PROGS
            NEXT C
        NEXT F
    END
