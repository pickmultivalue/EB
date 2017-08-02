    SUBROUTINE EB_CHOICES(C.COL,C.ROW,WIDTH,DEPTH,C.FILE,C.ID,VALUE,FLD.NBRS,MV,ATTRS,JUSTS,HEADER)
* @(#) EB_CHOICES.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.CHOICES Ported to jBASE 16:15:14  27 JUL 2001
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    GO MAIN$
!
!=============================================
! GALA 4GL
!
! Copyright (C) GENERAL AUTOMATION AUSTRALASIA Pty. Ltd.
!
! Written by Peter Falson - November 1991
!
!=========== Program's Purpose ===============
!
! Displays valid choices in a scroll-bar window
!
!=========== Mods/Fixes History ==============
!
! 06/11/92 - Sorted Display
!
!=============================================
! Please mod doco if not correct or other little gems are found
!
!   C.COL      X pos.   Can be Blank
!   C.ROW      Y pos.   Can be Blank
!   WIDTH      Width of display.   Can be Blank
!   DEPTH      DEPTH of display.   Can be Blank
!   C.FILE     String of File name from which data is to be retrieved and
!              displayed can be Blank
!   C.ID       Id's of choices. can be MV or Blank
!
!   If C.FILE is null C.ID contains MV list of choices
!
!   VALUE      The returned data can be Multi Attributed if retrieval
!              Params say so.
!              Can be multi-valued if multiple lines "tagged"
!   FLD.NBRS   The Columns to be returned. (Normally "1")
!              Eg if want Columns 1 and 3 put in 1:SVM:SVM:1
!   MV         Row (POS) selected
!              (MUST be initialised or use '' if not required)
!   ATTRS      Display Attributes to display Attr 2 and 3 put in 2:SVM:3
!              To translate to other files each subvalue can be of form:
!              attr:char(6):file_name:char(12):key_line
!              if key_line exists then key_of_file_name=VALUES<key_line,MV>
!              else key_of_file_name=VALUES<1,MV>
!              value is then record_read<attr>
!              eg. 0:SVM:1:SVM:1:CTRL.F:'file_name':CTRL.L:2
!              An "L" returns the line number.
!
!              Attr 2 can be a multivalued list of flags to "dim"
!              certain lines.
!   JUSTS      Display Formats. For above put 'L#20':SVM:'L#5'
!              If CONVersion required use form:
!              fmt:CHAR(3):conv:SVM:fmt...
!              eg. 'R#10':CTRL.C:'MD2':SVM:'L#10'...
!   HEADER     Header for box
!              First sub-value=box title
!              Remaining sub-values are column headings
!=============================================
!
!
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS SCR.PARAMS
    INCLUDE EB.EQUS CRT.PARAMS
    INCLUDE EB.EQUS SCR.CRT.PARAMS
    INCLUDE EB.EQUS DRV.PARAMS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS COLOURS
    INCLUDE EB.EQUS REFRESH.CODES
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS EB.CHARS
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.OS.INCLUDES USER.EXITS
    EQU OPERANDS TO '=#<>'
    EQU DQ TO '"',SQ TO "'"
    EQU NEXT.SEL TO CHAR(14), PREV.SEL TO CHAR(16)
!
    MAIN$:!
!
    DEFC INT JBASEEmulateGETINT(INT, INT)
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    vm_start=IF_COMPILED_PRIME
    WINTEGRATE=FG$STERM=3
    GUI=FG$STERM=6
    BLD.SCRN=((C.COL:C.ROW:WIDTH:DEPTH)='0000')
    EOF=TRUE
    IF FG$TERM.TYPE[2,1]='V' OR FG$STERM THEN FG$KEEP.CHOICES=FALSE
    POPUP.LIST.BOX=NOT(C.COL<2>); C.COL=C.COL<1>
!
    SAVE.COLOURS=FG$CURR.COLOURS:AM:FG$PREV.COLOURS
    HILINE=''       ;! used for tagged lines
    FOOTER=''
    TRANSLATE=FALSE
    RV.EMBED=(1-EMBED.ATTR<1,1>)
    GR.EMBED=(1-EMBED.ATTR<1,2>)
    IF UNASSIGNED(FG$SCR.CO.ORDS) THEN FG$SCR.CO.ORDS=''
    CC=C.COL+FG$SCR.CO.ORDS<1,1>*(FG$TERM.TYPE[2,1]='V')
    RR=C.ROW+FG$SCR.CO.ORDS<1,2>*(FG$TERM.TYPE[2,1]='V')
    COL.HDS=DELETE(HEADER,1,1,1); HEADER=HEADER<1,1,1>
    ID=C.ID
    KOFFSET=0       ;!(C.ID#'')
    B$LIST=''
    R.TABLE=''
    BEGIN CASE
    CASE ID MATCHES "'GLOBAL('1N0N')'"
        ID=GEX(OCONV(ID,'MCN'))
    CASE ID MATCHES "'EXTRAS('1N0N')'"
        ID=EXTRAS(OCONV(ID,'MCN'))
    END CASE
    MULT.IDS=(INDEX(ID,VM,1)) ;! OR C.FILE<1,2>#'')
    IF MULT.IDS THEN B$LIST=ID; EOF=FALSE
    BOX.CLEAR=''
    INIT.VALUE=MV<2>; DEL MV<2>
    UNSORTED=MV<2>; DEL MV<2>
    OFFSET=MV<2>; DEL MV<2>
    VALIDATE=MV<2>; DEL MV<2>
    EXCEPTIONS=MV<2>; DEL MV<2>
    IF MV#'' THEN
        I=2
        LOOP UNTIL INDEX(OPERANDS,MV[I,1],1) DO I+=1 REPEAT
        OPER.ATTR=MV[1,I-1]
        OPERAND=MV[I,1]
        OPER.TEST=MV[I+1,999]
        OPER.LEN=LEN(OPER.TEST)
        IF OPER.TEST[OPER.LEN,1]=']' THEN
            OPER.TEST=OPER.TEST[1,OPER.LEN-1]
            OPER.LEN-=1
        END ELSE OPER.LEN=999
    END ELSE OPERAND=''
    MV=1
!
    IF ATTRS='' THEN
        IF ID#'' THEN ATTRS=1 ELSE ATTRS=0
    END
    DIMMED=ATTRS<2>; ATTRS=ATTRS<1>
    NBR.ATTRS=DCOUNT(ATTRS,SVM)
    NBR.FLDS=DCOUNT(FLD.NBRS,SVM)
    FIRST.VAL=0
    FIRST.ATTR=''
    CONVS=''
    COL.JUSTS=''
    DISP.ATTRS=''
    FOR A=1 TO NBR.ATTRS
        JUST=JUSTS<1,1,A>
        IF INDEX(JUST,CTRL.C,1) THEN
            CONVS<1,1,A>=FIELD(JUST:CTRL.C,CTRL.C,2)
            JUST=JUST[1,COL1()-1]
            JUSTS<1,1,A>=JUST
        END
        IF JUST='L#0' THEN JUSTS<1,1,A>=''; JUST=''
        IF JUST#'' AND NOT(FIRST.VAL) THEN FIRST.VAL=A
        IF COL.HDS#'' THEN
            IF JUST#'' THEN
                JUST.LEN=LEN(1 JUST)
                IF JUST[1,1]='R' THEN JUST='R#':JUST.LEN ELSE JUST='L#':JUST.LEN
                IF A<NBR.ATTRS THEN JUST:=SPC
            END ELSE JUST='L#0'
            COL.JUSTS<1,1,A>=JUST
        END
        ATTR=ATTRS<1,1,A>
        IF ATTR[1,1] = 'S' THEN
            SRT = ATTR[1,3]
            ATTR = ATTR[4, -1]
        END ELSE SRT = ''
        IF FIRST.ATTR='' AND ATTR#'L' AND ATTR#'N' THEN     ;* ace
            FIRST.ATTR=A
            IF ATTR=0 AND ID#'' AND ID=ID<1,1> AND B$LIST='' THEN
                SAVE.ATTRS=ATTRS
                VALUES=ID
                DISP.VALUES=VALUES
                VALUE=ID
                GO EXIT.2
            END
        END
        IF NOT(NUM(ATTR)) AND ATTR#'L' AND ATTR#'N' THEN    ;* ace
            FILE=FIELD(ATTR,CTRL.F,1)
            ATTR=ATTR[COL2(),99]
            GOSUB OPEN.FILE
            ATTR=POS:ATTR
        END
        IF NUM(ATTR) THEN
            ATTR = A
        END ELSE
            ATTRS<1,1,A>=ATTR
        END
        DISP.ATTRS<1,1,A>=SRT:ATTR
    NEXT A
    IF RR<1 THEN RR=8
    IF WIDTH='' THEN
        IF JUSTS#'' THEN
            WIDTH=2
            GAP=0
            FOR A=1 TO NBR.ATTRS
                JUST=JUSTS<1,1,A>
                IF JUST#'' THEN
                    WIDTH+=(GAP+LEN(1 JUST))
                    GAP=1
                END ELSE GAP=0
            NEXT A
        END ELSE WIDTH=40
    END
    IF CC+WIDTH>78 THEN
        CC=78-WIDTH
        IF CC<1 THEN CC=1
        RR+=1
    END
    IF DEPTH='' THEN
        DEPTH=19-RR
        IF DEPTH < 10 THEN DEPTH=10
    END
    IF DEPTH<4 THEN DEPTH=4
!  IF RR+DEPTH>18 THEN RR=19-DEPTH
    HASH='L#':WIDTH-2
    WIDTH+=4
    IF JUSTS='' THEN JUSTS=HASH
    VALUE=''
    SORT.BY=JUSTS<1,1,FIRST.VAL>[1,1]
    IF INDEX('LR',SORT.BY,1) ELSE SORT.BY='L'
!  SORT.BY='A':SORT.BY
    IF JUSTS<2> # '' THEN SORT.BY=JUSTS<2> ELSE SORT.BY='A':SORT.BY
    WARN.FLAG=TRUE
    FILE=C.FILE
    IF FILE#FILE<1,1> THEN LAST.CHOICE=DELETE(FILE,1,1,0); FILE=FILE<1,1> ELSE LAST.CHOICE=''
    ORIG.ATTRS=ATTRS
    IF FILE='' THEN
        VALUES=ID
        EOF=TRUE
        IF INDEX(ATTRS,CTRL.L,1) THEN CALL EB_CHOICE.BLD(VALUES,ATTRS:AM:OFFSET)
        IF ATTRS<1,1,1>='N' THEN        ;* ace
            VMCNT=DCOUNT(VALUES<1>,VM)  ;* ace
            LATTR=DCOUNT(VALUES,AM)+1
!      INS '' BEFORE VALUES<1>; * ace
            EOICNT=VMCNT
            NBRS=''
            FOR VMNO = 1 TO VMCNT       ;* ace
                NBRS<1,VMNO>=VMNO       ;* ace
            NEXT VMNO         ;* ace
            ATTRS<1,1,1>=LATTR          ;* ace
            VALUES<LATTR>=NBRS          ;* ace
        END         ;* ace
        SAVE.ATTRS=ATTRS
    END ELSE
        SAVE.ATTRS=ATTRS
        BEGIN CASE
        CASE FILE='EB.WORK'; F.FILE=FG$WORK.FILE
        CASE FILE='EB.HELP.INDEX'; F.FILE=FG$EB.HELP        ;!.INDEX
        CASE NUM(FILE)
            IF FG$SCR.PAGING<1,FILE>=FG$WINDOW THEN FILE=WIN.PANE(FILE) ELSE FILE=DAT(FILE)
            GOSUB OPEN.FILE
        CASE NUM(FILE); F.FILE=FILES(FILE)
        CASE 1
            GOSUB OPEN.FILE
        END CASE
        IF ID#'' AND ATTRS<1,1,1>#0 AND NOT(MULT.IDS) THEN
            READ VALUES FROM F.FILE,ID ELSE VALUES=''
            SORT.BY = ''
            FOR A=1 TO NBR.ATTRS
                ATTR = ATTRS<1,1,A>
                IF ATTR[1,1] = 'S' THEN
                    SORT.BY = ATTR[2,2]
                    ATTR = ATTR[4,-1]
                    ATTRS<1,1,A> = ATTR
                    BREAK
                END
            NEXT A
            IF LEN(SORT.BY) THEN
                USATTR = VALUES<ATTR>
                USVALUES = ''
                FOR A = 1 TO NBR.ATTRS
                    USVALUES<A> = VALUES<ATTRS<1,1,A>>
                    ATTRS<1,1,A> = A
                NEXT A
                VALUES = ''
                VMCNT=DCOUNT(USATTR,VM)
                ATTR = ''
                FOR V = 1 TO VMCNT
                    VALUE = USATTR<1, V>
                    LOCATE VALUE IN ATTR<1> BY SORT.BY SETTING POS ELSE NULL
                    INS VALUE BEFORE ATTR<1,POS>
                    FOR A = 1 TO NBR.ATTRS
                        INS USVALUES<A, V> BEFORE VALUES<A, POS>
                    NEXT A
                NEXT V
            END
            TRANSLATE=TRUE
        END ELSE
            TRANSLATE=INDEX(ATTRS,CTRL.F,1)
            FOR A=1 TO NBR.ATTRS
                IF INDEX(ATTRS<1,1,A>,CTRL.F,1) OR ATTRS<1,1,A>='L' ELSE ATTRS<1,1,A>=A
            NEXT A
            BEGIN CASE
            CASE MULT.IDS
                VALUES=''
                IDCNT=1; EOICNT=DCOUNT(B$LIST<1>,VM)
                GOSUB RESUME.SEARCH
            CASE ID#'' AND SAVE.ATTRS<1,FIRST.ATTR>=0
                VALUES=ID
            CASE 1
                EOICNT=0
                GOSUB BUILD.CHOICES
                ATTRS=DISP.ATTRS
                SAVE.ATTRS=ATTRS
            END CASE
        END
    END
    K.ATTR=ATTRS<1,1,FIRST.ATTR>
    IF LAST.CHOICE#'' THEN VALUES<K.ATTR,-1>=LAST.CHOICE
    IF EXCEPTIONS#'' THEN CALL EB_EXCEPTIONS(VALUES,EXCEPTIONS,ATTRS<1,1,1>)
    IF INDEX(ATTRS,CTRL.F,1) THEN
        DISP.VALUES=VALUES<1>
    END ELSE DISP.VALUES=VALUES<K.ATTR>
    NBR.VALS=1
    K.ATTR=ATTRS<1,1,1>
    VALUE=''
    IF DISP.VALUES='' THEN GO EXIT.2
    USE.ATTR=(ATTRS=ORIG.ATTRS)
    IF K.ATTR#'L' THEN
        IF VALUES<K.ATTR>=VALUES<K.ATTR,1> THEN
            VALUE=VALUES<K.ATTR>
        END
    END ELSE
        IF DISP.VALUES<1>=DISP.VALUES<1,1> THEN
            VALUE=1
        END
    END
    IF VALUE#'' THEN
        IF BLD.SCRN THEN
            DISP.ATTRS=''
        END ELSE
            GOSUB GET.LINE
            GO EXIT.2
        END
    END
!
    IF FG$STERM OR WINTEGRATE THEN
        CALL EB_ERRMSG(SPC, 0)
        NBR.VALS=DCOUNT(DISP.VALUES<1>,VM)
        PGE=1
        ST=1
!    IF NBR.VALS>50 THEN
!      INCR=50; FI=50; NBR.PGES=INT((NBR.VALS-1)/INCR)+1
!    END ELSE
        FI=NBR.VALS; INCR=NBR.VALS; NBR.PGES=1
!    END
        TRANSLATE=(SAVE.ATTRS#DISP.ATTRS AND NOT(MULT.IDS) OR CONVS#'')
!    TRANSLATE+=INDEX(SAVE.ATTRS,CTRL.F,1)
        IF TRANSLATE ELSE DISP.ATTRS=ATTRS
STERM.RESUME:       !
        VALUE=INIT.VALUE
        PAGE.ITEMS=''
        LOOP
            IF PAGE.ITEMS<PGE>='' THEN
                EOICNT=NBR.VALS
                IF TRANSLATE OR GUI THEN
                    ATTRS=SAVE.ATTRS
                    FOR MV=1 TO FI
                        GOSUB GET.LINE
                    NEXT MV
                END ELSE
                    DISP.VALUES=VALUES
                END
                ATTRS=DISP.ATTRS
            END
            IF GUI THEN
                DISP.VALUES=DISP.VALUES<1>
                CONVERT VM TO FB IN DISP.VALUES
            END
            IF BLD.SCRN THEN
                VALUE=EOF:FB:DISP.VALUES          ;!<1>
                RETURN
            END
            IF FG$DIALOG.BOX AND FG$FLD MATCHES "1N0N" THEN
                CC=FG$SCR.COL<1,FG$FLD>
                IF FG$SCR.PAGING<1,FG$FLD> THEN
                    FIRST.PAGE=FG$SCR.ST.PAGE<1,FG$WINDOW>
                    FIRST.ROW=FG$SCR.ROW<1,FIRST.PAGE>
                    SKIP=FG$SCR.NBR.LINES<1,FG$WINDOW>
                    THIS.ROWS=FG$SCR.NBR.ROWS<1,FG$WINDOW>
                    RR=FIRST.ROW+MOD(FG$PANE-1,THIS.ROWS)*SKIP
                END ELSE
                    RR=FG$SCR.ROW<1,FG$FLD>
                END
                IF CC=C.COL AND RR=C.ROW AND FG$ACT.CODE=FG$OPT.CODE THEN       ;! normal lookup ?
                    STD.LKUP=TRUE
                END ELSE STD.LKUP=FALSE
            END ELSE STD.LKUP=TRUE
            R.TABLE=HEADER:AM:COL.HDS:AM:JUSTS:AM:DEPTH:AM:CC:VM:RR:VM:STD.LKUP
            IF VALIDATE ELSE
                IF FG$STERM THEN
                    R.TABLE<6>=0
                    IF DISP.VALUES<1,NBR.VALS>=FG$ERROR.MSGS<105> THEN DEL DISP.VALUES<1,NBR.VALS>; R.TABLE<6>=1
                    IF DISP.VALUES<1,NBR.VALS>=FG$ERROR.MSGS<51> THEN DEL DISP.VALUES<1,NBR.VALS>; R.TABLE<6>=1
                END
            END
            R.TABLE<7>=(ST>INCR):VM:(EOICNT#NBR.VALS)
            DLG.NAME=FG$DBX.NAME:'_':FG$SCREEN.PAGE
            IF FG$DBX.NAME#'' THEN
                R.TABLE<8>=DLG.NAME
                IF NOT(POPUP.LIST.BOX) THEN
                    R.TABLE<9>='L':FG$FLD
                END ;!ELSE R.TABLE<10>=DROPDOWN*FG$SCR.NBR.PAGES
            END
            MV=''; VALUE=INIT.VALUE
            CALL EB_WIN.CHOICES(R.TABLE,DISP.VALUES,VALUE,1,ATTRS,FG$STERM,FG$W.COL,FG$W.ROW)
        WHILE VALUE=NEXT.SEL OR VALUE=PREV.SEL DO
            DIRECTION=1-2*(VALUE=PREV.SEL)
            ST+=(INCR*DIRECTION)
        REPEAT
        IF FG$DBX.NAME#'' AND NOT(GUI) THEN
            IF VALUE[1,1]=STX THEN
                FG$NEXT.EVENT=VALUE
                CALL EB_WIN.COMSUB('DB ShowControl ':R.TABLE<8>:',':R.TABLE<9>:',0')
                VALUE=''
            END
        END
        IF VALUE='' AND ((NBR.VALS>50 AND NOT(ST=1 AND FI=NBR.VALS)) OR NOT(EOF)) THEN
            CALL EB_MERRMSG('',FG$ERROR.MSGS<124>,'',ANS,'Y':VM:'N')
            IF ANS='Y' THEN
                IF EOICNT=NBR.VALS AND ST=1 ELSE
                    IDCNT=1; EOICNT=NBR.VALS
                    ATTRS=SAVE.ATTRS
                    DISP.VALUES=''
                    VALUES=''
                    EOF=FALSE
                    GOSUB RESUME.SEARCH
                END
                ORIG.COUNT=NBR.VALS
                GOSUB REFINE
                IF NBR.VALS#ORIG.COUNT THEN
                    IF NBR.VALS=0 THEN
                        CALL EB_MERRMSG('',FG$ERROR.MSGS<125>,'',ANS,'Y':VM:'N')
                        IF ANS='Y' THEN
                            ST=1
                            NBR.VALS=ORIG.COUNT
                            GO STERM.RESUME
                        END
                    END
                    IF NBR.VALS THEN
                        FI=NBR.VALS
                        GO STERM.RESUME
                    END
                END
            END
        END
        IF NOT(GUI) THEN
            IF VALUE<2> THEN
                VALUE=DISP.VALUES<FLD.NBRS<1,1,1>,VALUE<2>>
!VALUE=VALUE<2>
                TRANSLATE=FALSE
!IF VALUE AND K.ATTR#'L' THEN VALUE=VALUES<K.ATTR,VALUE>
            END
        END
        GO EXIT.2
    END
!
*ace   NBR.VALS=DCOUNT(VALUES<K.ATTR>,VM)
    IF K.ATTR = 'L' THEN      ;* ace
        NBR.VALS=DCOUNT(VALUES<ATTRS<1,1,FIRST.ATTR>>,VM)   ;* ace
    END ELSE        ;* ace
        NBR.VALS=DCOUNT(VALUES<K.ATTR>,VM)        ;* ace
    END   ;* ace
    IF NBR.VALS<DEPTH THEN DEPTH=NBR.VALS+(COL.HDS#'')
!
    WIDTH+=GR.EMBED
    CC-=GR.EMBED
!
    FG.COLOUR=FG$COLOURS<1,1,1>; BG.COLOUR=FG$COLOURS<1,1,2>
    BOX.COLOUR=FG$COLOURS<1,1,3>
    IF UNASSIGNED(WHITE) THEN MAT COLOURS=''
    IF WHITE<1,1>#'' THEN CALL EB_CH.COLOUR(FG.COLOUR,BG.COLOUR)
    MENU.COLOURS=FG$CURR.COLOURS
    IF WHITE<1,1>#'' THEN CALL EB_CH.COLOUR(BOX.COLOUR,BG.COLOUR)
    BOX.COLOURS=FG$CURR.COLOURS
    RNET=INDEX(CRT.BOX,'\\box',1)
    BEGIN CASE
    CASE RNET
        IF SCREEN.SAVE<1,1>#'' THEN
            CRT SCREEN.SAVE<1,1>:FG$DISK.DRIVE:FG$CRT.PAGE:SCREEN.SAVE<1,2>:
        END
        BOX.CLEAR=ESC:'\\box ':CC:',':RR:',':CC+WIDTH-3:',':DEPTH+RR:',0,1':CHAR(127)
        BOX.DRAW=ESC:'\\box ':CC:',':RR:',':CC+WIDTH-3:',':DEPTH+RR+1:',1,1':CHAR(127)
        CRT BOX.DRAW:
        IF HEADER#'' THEN
            HEADER=TRIM(HEADER HASH,' ',"T")
            CRT @(CC+INT((WIDTH-LEN(HEADER))/2)-2-GR.EMBED,RR):GROFF:HEADER:GRON:@(CC+WIDTH,RR):GROFF:
        END
    CASE FG$TERM.TYPE[2,1]='V'
        BOX.CLEAR=ESC:CHAR(8):'WCLOSE':CHAR(0)
        BOX.DRAW=ESC:CHAR(8):'WOPEN /S ':CC+2:',':RR:',':WIDTH-5:',':DEPTH+0:CHAR(0):ESC:CHAR(8):'WFRAME 1':CHAR(0):CLS
        CRT BOX.DRAW:
        IF HEADER#'' THEN CRT ESC:CHAR(8):'WTITLE /C ':HEADER:CHAR(0):
        CC=-2; RR=-1
    CASE 1
        IF SCREEN.SAVE<1,1>#'' THEN CALL EB_SCREEN.SAVE(FG$DISK.DRIVE:FG$CRT.PAGE)
!
        CALL EB_BOX(CC,RR,WIDTH,DEPTH+1,1,BOX.CLEAR,BOX.DRAW)
        IF HEADER#'' THEN
            HEADER=TRIM(HEADER HASH,' ',"T")
            CRT @(CC+INT((WIDTH-LEN(HEADER))/2)-2-GR.EMBED,RR):GROFF:HEADER:GRON:@(CC+WIDTH,RR):GROFF:
        END
    END CASE
    CC+=GR.EMBED
    CRT MENU.COLOURS:
    INCLUDE EB.OS.INCLUDES PC.OFF.CURSOR
    CC-=GR.EMBED
    RV.COL=CC+2-RV.EMBED
    CC+=2
    IF COL.HDS#'' THEN
        RR+=1
        DEPTH-=1
        COL.HD=''
        FOR I=1 TO NBR.ATTRS
            JUST=COL.JUSTS<1,1,I>
            IF JUST#'L#0' THEN COL.HD:=COL.HDS<1,1,I> JUST
        NEXT I
        CRT @(CC,RR):BG:COL.HD HASH:FG:
    END
    RR+=1
    SUB.LENGTH=''
    SUB.CODES=FG$SRCH.CH:VM:FG$SEL.CH:VM:FG$TOP.CH:VM:FG$NXT.CH:VM:FG$PRV.CH:VM:FG$TAB.CH
    IF UNASSIGNED(FG$ID.CHOICES) THEN MAT CRT.PARAMS=''
    IF FG$ID.CHOICES#VALUES<1> THEN SUB.CODES<1,-1>=FG$TAG.CH
    SUB.CODES<2>='1X'
    FIRST.VAL=0
    FOR A=1 TO NBR.ATTRS UNTIL FIRST.VAL
        IF JUSTS<1,1,A>#'' THEN FIRST.VAL=A
    NEXT A
    RR-=1
    FOOTER.PREFIX=@(CC+WIDTH-11-2*GR.EMBED,RR+DEPTH+1):GROFF
    FOOTER.SUFFIX=GRON:@(CC+WIDTH,RR+DEPTH+1):GROFF
RESTART:  !
    R=1
    IF INIT.VALUE='' THEN C=1 ELSE
        LOCATE INIT.VALUE IN VALUES<K.ATTR,vm_start> SETTING C ELSE C=1
    END
    NBR.PAGES=INT((NBR.VALS-1)/DEPTH)+1
    LAST.PGE=INT((NBR.VALS-1)/DEPTH)*DEPTH+1
    IF C>DEPTH THEN
        PGE=INT((C-1)/DEPTH)+1
        I=(PGE-1)*DEPTH+1
        GOSUB SCROLL.PAGE
        R=C-I+1
    END ELSE
        PGE=1
        I=1
        GOSUB SCROLL.PAGE
        R+=C-I
    END
    REPLY=''
    I=C
    LOOP
        MV=I; GOSUB GET.LINE
        CRT @(RV.COL,RR+R):BG:RVON:LINE HASH:RVOFF:FG:
        CALL EB_INPUT_ZERO(REPLY,CC,RR+R,FG$INPUT.CODES,SUB.CODES,SUB.LENGTH)
        IF REPLY=SPC THEN FG$ACT.CODE=FG$TAG.CODE
        IF NOT(FG$ACT.CODE) AND REPLY='' THEN
            IF INDEX(HILINE,1,1) ELSE
                IF K.ATTR='L' THEN VALUE=I ELSE VALUE=VALUES<K.ATTR,I>
            END
            FG$ACT.CODE=FG$END.CODE
        END
    UNTIL FG$ACT.CODE=FG$ABT.CODE OR FG$ACT.CODE=FG$END.CODE OR FG$ACT.CODE=FG$JMP.CODE DO
        BEGIN CASE
        CASE FG$ACT.CODE=FG$SEARCH.CODE
            LOOP
                GOSUB REFINE
            WHILE NBR.VALS=0 DO
                CALL EB_MERRMSG('',FG$ERROR.MSGS<125>,'',ANS,'Y':VM:'N')
                IF ANS='Y' THEN
                    IDCNT=1
                    EOF=FALSE
                    IF FILE#'' THEN
                        GOSUB RESUME.SEARCH
                        NBR.VALS=EOICNT
                    END ELSE
                        VALUES=ORIG.VALUES
                    END
                END ELSE GO EXIT.2
            REPEAT
            GOSUB SCROLL.PAGE
        CASE FG$ACT.CODE=FG$SEL.CODE
            IF HILINE<1,1> THEN
                HILINE=''
            END ELSE
                HILINE=1:STR(VM:1,NBR.VALS-1)
            END
            GOSUB SCROLL.PAGE
        CASE FG$ACT.CODE=FG$TAG.CODE
            IF HILINE<1,I> THEN
                HILINE<1,I>=''
                CRT @(RV.COL,RR+R):RVOFF:LINE HASH:
            END ELSE
                HILINE<1,I>=TRUE
            END
            FG$ACT.CODE=FG$SKP.CODE
        CASE 1
            IF HILINE<1,I> THEN
                CRT @(RV.COL,RR+R):FG:RVON:LINE HASH:
            END ELSE
                CRT @(RV.COL,RR+R):RVOFF:
                IF DIMMED<1,I> THEN CRT BG:
                CRT LINE HASH:FG:
            END
        END CASE
        BEGIN CASE
        CASE FG$ACT.CODE=FG$SEARCH.CODE
        CASE FG$ACT.CODE=FG$SEL.CODE
        CASE FG$ACT.CODE=FG$SKP.CODE OR FG$ACT.CODE=FG$SKP.CODE
            IF I#NBR.VALS THEN
                R+=1; I+=1
                IF R>DEPTH THEN PGE+=1; GOSUB SCROLL.PAGE
            END ELSE
                I=1
                IF LAST.PGE#1 THEN PGE=1; GOSUB SCROLL.PAGE ELSE R=1
            END
        CASE FG$ACT.CODE=FG$BCK.CODE
            IF I>1 THEN
                R-=1; I-=1
                IF R<1 THEN
                    I-=(DEPTH-1)
                    PGE-=1; GOSUB SCROLL.PAGE
                    I+=DEPTH-1
                    R=DEPTH
                END
            END ELSE
                IF LAST.PGE=1 THEN
                    I=NBR.VALS; R=DEPTH
                END ELSE
                    I=LAST.PGE; PGE=NBR.PAGES; GOSUB SCROLL.PAGE
                    I=NBR.VALS; R=NBR.VALS-LAST.PGE+1
                END
            END
        CASE FG$ACT.CODE=FG$TOP.CODE
            IF J>1 THEN
                I=1; PGE=1; GOSUB SCROLL.PAGE
            END ELSE CRT BELL:
        CASE FG$ACT.CODE=FG$NXTS.CODE
            J=I+DEPTH-R+1
            IF J<=NBR.VALS THEN
                I=J
                PGE+=1; GOSUB SCROLL.PAGE
            END ELSE CRT BELL:
        CASE FG$ACT.CODE=FG$PRVS.CODE
            J=I-DEPTH-R+1
            IF J>=1 THEN
                I=J
                PGE-=1; GOSUB SCROLL.PAGE
            END ELSE CRT BELL:
        CASE FG$ACT.CODE=FG$TAB.CODE
            C=I
            LOOP
                C+=1
            UNTIL NOT(DIMMED<1,C>) OR C>NBR.VALS DO REPEAT
            IF C<=NBR.VALS THEN
                IF R+C-I>DEPTH THEN
                    PGE=INT((C-1)/DEPTH)+1
                    I=(PGE-1)*DEPTH+1
                    GOSUB SCROLL.PAGE
                    R=C-I+1
                END ELSE R+=C-I
                I=C
            END
        CASE 1
            REPLY=OCONV(REPLY,'MCU')
            J=I
CHAR.SEARCH:        !
            C=J
            LOOP
                C+=1
                CHR=OCONV(DISP.VALUES<FIRST.VAL,C>[1,1],'MCU')
            UNTIL CHR=REPLY OR C>NBR.VALS OR C=J DO REPEAT
            IF C>NBR.VALS THEN
                IF C>J AND J#0 THEN J=0; GO CHAR.SEARCH ELSE C=1
            END
            MV=INT((C-1)/DEPTH)+1
            IF MV#PGE THEN
                PGE=MV
                I=(PGE-1)*DEPTH+1
                GOSUB SCROLL.PAGE
                R=C-I+1
            END ELSE R+=C-I
            I=C
        END CASE
    REPEAT
!
    CRT @(CC,RR+R):RVOFF:NFLSH:
FINISH:   !
    IF VALUE='' AND NOT(EOF) THEN
        CALL EB_MERRMSG('','Continue with search (Y/N)','',ANS,'Y':@VM:'N')
        IF ANS='Y' THEN
            WARN.FLAG=TRUE
            GOSUB RESUME.SEARCH
            GO RESTART
        END
    END
    FG$CURR.COLOURS=SAVE.COLOURS<1>; FG$PREV.COLOURS=SAVE.COLOURS<2>
    IF SCREEN.SAVE<1,1>#'' THEN
        IF FG$KEEP.CHOICES ELSE
            IF FG$TERM.TYPE[2,1]='V' THEN CRT BOX.CLEAR: ELSE
                CALL EB_SCREEN.REST(FG$DISK.DRIVE:FG$CRT.PAGE)
            END
        END
        CRT FG$CURR.COLOURS:
    END ELSE
        CRT FG$CURR.COLOURS:
!    IF FG$KEEP.CHOICES ELSE
        CRT BOX.CLEAR:
!    FG$REFRESH=FG$FULL.NOCLEAR
!      CALL EB_REPAINT.WINDOW(0,CC,RR-(COL.HDS#''),WIDTH,DEPTH+(COL.HDS#''))
!    END
    END
EXIT.2:   !
    INCLUDE EB.OS.INCLUDES PC.ON.CURSOR
    K.ATTR=SAVE.ATTRS<1,1,1>
    IF FG$ACT.CODE=FG$END.CODE AND INDEX(HILINE,1,1) THEN
        MV=''; MVNDX=0
        FOR I=1 TO NBR.VALS
            IF HILINE<1,I> THEN
                IF K.ATTR='L' THEN STMP=I ELSE STMP=VALUES<K.ATTR,I>
                MVNDX+=1
                VALUE<1,MVNDX>=STMP
                MV<1,MVNDX>=I
            END
        NEXT I
    END
    IF VALUE#'' THEN
        IF K.ATTR=0 THEN K.ATTR=FIRST.ATTR
        IF MV='' THEN
            CONVS<1,1,1>=-1
            IF K.ATTR='L' OR K.ATTR='N' THEN
                MV=VALUE
            END ELSE
                LOCATE VALUE IN VALUES<K.ATTR,vm_start> SETTING MV ELSE MV=FALSE
            END
            IF MV THEN GOSUB GET.LINE
        END
        IF NBR.FLDS THEN
            NBR.MVS=DCOUNT(MV,VM)
            FOR A=2 TO NBR.FLDS
                IF FLD.NBRS<1,1,A> THEN
                    FOR J=1 TO NBR.MVS
!            IF K.ATTR='L' THEN STMP=A ELSE STMP=DISP.VALUES<A,MV<1,J>>
                        IF K.ATTR='L' THEN STMP=A ELSE STMP=VALUES<ATTRS<1,1,A>,MV<1,J>>
                        VALUE<A,J>=STMP
                    NEXT J
                END
            NEXT A
        END
    END
    IF FG$ACT.CODE THEN FG$LAST.ACT.CODE=FG$ACT.CODE; FG$ACT.CODE=FALSE
    IF WINTEGRATE THEN
        LKUP.ID=R.TABLE<8>
        IF LKUP.ID#'' AND POPUP.LIST.BOX THEN
            SCRIPT = "DB End ":LKUP.ID
            SCRIPT = "DB Delete ":LKUP.ID
            CALL EB_WIN.COMSUB(SCRIPT)
            CALL EB_WIN.HGLASS(1,0)
        END
    END
    RETURN
!
SCROLL.PAGE:        !
    IF NBR.VALS<DEPTH THEN
        ODEPTH=DEPTH
        DEPTH=NBR.VALS
        NBR.PAGES=INT((NBR.VALS-1)/DEPTH)+1
        LAST.PGE=INT((NBR.VALS-1)/DEPTH)*DEPTH+1
    END ELSE ODEPTH=DEPTH
    FOR J=1 TO DEPTH
        MV=J+I-1
        IF MV<=NBR.VALS THEN GOSUB GET.LINE ELSE LINE=''
        IF HILINE<1,MV> THEN
            CRT @(RV.COL,RR+J):RVON:
        END ELSE
            CRT @(RV.COL,RR+J):RVOFF:
        END
        IF DIMMED<1,MV> THEN CRT BG:
        CRT LINE HASH:RVOFF:FG:
    NEXT J
    J+=am_start
    LINE='' HASH
    LOOP UNTIL J>ODEPTH DO
        CRT @(RV.COL,RR+J):RVOFF:LINE:
        J+=1
    REPEAT
    IF NBR.PAGES>1 OR FOOTER#'' THEN
        FOOTER=PGE 'R#2':'/':NBR.PAGES 'L#2'
        IF FG$TERM.TYPE[2,1]#'V' THEN
            CRT FOOTER.PREFIX:FOOTER:FOOTER.SUFFIX:
        END ELSE
            CRT ESC:CHAR(8):'WTITLE /B/R ':FOOTER:CHAR(0):
        END
    END
    R=1
    RETURN
!
GET.LINE: !
    IF MV=NBR.VALS AND LAST.CHOICE#'' THEN
        IF WINTEGRATE OR FG$DIALOG.BOX THEN
            GAP=''
            LINE=''
            FOR A=1 TO NBR.ATTRS
                JUST=JUSTS<1,1,A>
                CONV=CONVS<1,1,A>
                IF K.ATTR='L' THEN STMP=MV ELSE STMP=LAST.CHOICE
                IF JUST#'' OR FG$DIALOG.BOX THEN
                    BEGIN CASE
                    CASE FG$DIALOG.BOX
                        IF JUST='' THEN
                            LINE:=GAP:TRIM(STMP)
                        END ELSE LINE:=GAP:TRIM(STMP JUST)
                        GAP=SVM
                    CASE WINTEGRATE
                        LINE:=GAP:TRIM(STMP JUST)
                        GAP=SVM         ;!'":tab:"'
                    CASE 1
                        LINE:=GAP:STMP JUST
                        GAP=SPC
                    END CASE
                END ELSE
                    LINE=''
                    GAP=''
                END
            NEXT A
            DISP.VALUES<1,MV>=LINE
        END ELSE LINE=LAST.CHOICE
    END ELSE
        JUST=JUSTS<1,1,1>
        CONV=CONVS<1,1,1>
        IF CONV=-1 THEN CONV='' ; OCONV.IT=FALSE ELSE OCONV.IT=TRUE
        IF K.ATTR='L' THEN STMP=MV ELSE STMP=VALUES<K.ATTR,MV>
        Key=STMP
        IF CONV#'' THEN Key=OCONV(Key,CONV)
        DISP.VALUES<1,MV>=Key
        IF JUST#'' OR FG$DIALOG.BOX THEN
            BEGIN CASE
            CASE FG$DIALOG.BOX
                LINE=Key
                GAP=SVM
            CASE WINTEGRATE
                LINE=Key
                GAP='":tab:"'
            CASE 1
                LINE=Key JUST
                GAP=SPC
            END CASE
        END ELSE
            LINE=''
            GAP=''
        END
        FOR A=2 TO NBR.ATTRS
            JUST=JUSTS<1,1,A>
            ATTR=ATTRS<1,1,A>
            OK=(NOT(OCONV.IT) AND ATTR#'L' AND ATTR#'N' OR FG$DIALOG.BOX)
            IF JUST#'' OR OK THEN
                CONV=CONVS<1,1,A>
                STMP=DISP.VALUES<A,MV>
                IF TRANSLATE OR STMP='' THEN
                    IF INDEX(ATTR,CTRL.F,1) THEN
                        KPOS=FIELD(ATTR:CTRL.L,CTRL.L,2)
                        ATTR=ATTR[1,COL1()-1]
                        FILE=FIELD(ATTR,CTRL.F,1)
                        IF FILE THEN
                            ATTR=ATTR[COL2()+1,99]
!                            IF KPOS THEN KPOS=DISP.VALUES<KPOS+KOFFSET,MV> ELSE KPOS=Key
                            IF KPOS THEN KPOS=VALUES<KPOS+KOFFSET,MV> ELSE KPOS=Key
                            READV STMP FROM OPENED.FILES(FILE),KPOS,ATTR ELSE STMP=''
                        END ELSE STMP=''
!
! VALUES should contain columns of data therfore the A loop count should
! be used NOT the ATTR counter
                    END ELSE
                        IF USE.ATTR THEN STMP=VALUES<ATTR,MV> ELSE STMP=VALUES<A,MV>
                    END
                    OK=TRUE
                END
                IF CONV#'' AND OK THEN
                    IF OCONV.IT THEN STMP=OCONV(STMP<1,1,1>,CONV) ELSE STMP=ICONV(STMP<1,1,1>,CONV)
                END
                BEGIN CASE
                CASE FG$DIALOG.BOX
                    LINE:=GAP:STMP
                    GAP=SVM
                CASE WINTEGRATE
                    LINE:=GAP:STMP
                    GAP='":tab:"'
                CASE 1
                    LINE:=GAP:STMP JUST
                    GAP=SPC
                END CASE
            END ELSE GAP=''
!      IF FG$STERM OR OK THEN DISP.VALUES<A,MV>=STMP
            IF MOD(FG$STERM,3) THEN DISP.VALUES<A,MV>=STMP
        NEXT A
        IF GUI THEN
            DISP.VALUES<1,MV>=LINE
        END ELSE
            IF WINTEGRATE THEN DISP.VALUES<1,MV>=DQ:LINE:DQ
        END
    END
    RETURN
!
OPEN.FILE:!
!
    IF NOT(NUM(FILE)) THEN
        LOCATE FILE IN OPEN.FILE.LIST<am_start> SETTING POS ELSE
            CALL EB_OPEN_FILE(FILE,POS)
            IF NOT(POS) THEN
                VALUE=''
                RETURN TO EXIT.2
            END
        END
        IF POS THEN F.FILE=OPENED.FILES(POS)
    END ELSE POS=FILE
    RETURN
REFINE:   !
    ORIG.VALUES=VALUES
    CALL EB_REFINE(VALUES,HILINE,DIMMED,NBR.ATTRS,ATTRS,DISP.VALUES,NBR.ATTRS)
    NBR.VALS=DCOUNT(VALUES<1>,VM)
    NBR.PAGES=INT((NBR.VALS-1)/DEPTH)+1
    LAST.PGE=INT((NBR.VALS-1)/DEPTH)*DEPTH+1
    PGE=1; I=1
    RETURN
!
BUILD.CHOICES:      !
!
    SELECT F.FILE
    EOF=FALSE
    VALUES=''
RESUME.SEARCH:      !
    K.ATTR=1
    MV=1
    LOOP
        IF MULT.IDS THEN
            IF IDCNT>EOICNT THEN
                EOF=TRUE
            END ELSE
                ID=B$LIST<1,IDCNT>
                IDCNT+=1
            END
        END ELSE
            READNEXT ID ELSE EOF=TRUE
        END
        INCLUDE EB.OS.INCLUDES SYSTEM.14
    UNTIL EOF OR SYSTEM.14 DO
        READ DESC FROM F.FILE,ID ELSE DESC=''
        OK=TRUE
        IF OPERAND#'' THEN
            IF OPER.ATTR THEN ATTR=DESC<OPER.ATTR,1,1> ELSE ATTR=ID
            BEGIN CASE
            CASE OPERAND='='
                IF ATTR[1,OPER.LEN]=OPER.TEST ELSE OK=FALSE
            CASE OPERAND='#'
                IF ATTR[1,OPER.LEN]#OPER.TEST ELSE OK=FALSE
            CASE OPERAND='>'
                IF ATTR[1,OPER.LEN]>OPER.TEST ELSE OK=FALSE
            CASE OPERAND='<'
                IF ATTR[1,OPER.LEN]<OPER.TEST ELSE OK=FALSE
            END CASE
        END
        IF OK THEN
            IF DESC#'' ELSE DESC=SPC
            ATTR=SAVE.ATTRS<1,1,FIRST.VAL>
            IF INDEX(ATTR,CTRL.F,1) THEN ATTR=FIELD(FIELD(ATTR,CTRL.F,2):CTRL.L,CTRL.L,1)
            IF UNSORTED THEN I=MV ELSE
                IF ATTR#0 THEN
                    LOCATE DESC<ATTR,1,1> IN VALUES<FIRST.VAL,vm_start> BY SORT.BY SETTING I ELSE NULL
                END ELSE
                    LOCATE ID IN VALUES<FIRST.VAL,vm_start> BY SORT.BY SETTING I ELSE NULL
                END
            END
            FOR A=1 TO NBR.ATTRS
                ATTR=SAVE.ATTRS<1,1,A>
                IF INDEX(ATTR,CTRL.F,1) THEN ATTR=FIELD(FIELD(ATTR,CTRL.F,2):CTRL.L,CTRL.L,1)
                IF ATTR#0 THEN
                    INS DESC<ATTR,1,1> BEFORE VALUES<A,I>
                END ELSE
                    INS ID BEFORE VALUES<A,I>
                END
            NEXT A
            MV+=1
            IF WARN.FLAG THEN IF I>DEPTH THEN
                WARN.FLAG=FALSE
                IF FG$STERM OR FG$DIALOG.BOX ELSE CALL EB_ERRMSG(FG$ERROR.MSGS<65>, 0)
            END
        END
    REPEAT
    ECHO OFF
    LOOP
        INCLUDE EB.OS.INCLUDES SYSTEM.14
    WHILE SYSTEM.14 DO
        INCLUDE EB.OS.INCLUDES INPUT.ZERO
    REPEAT
    ECHO ON
    RETURN
END