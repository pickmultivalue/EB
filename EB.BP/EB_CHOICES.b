    SUBROUTINE EB_CHOICES(C.COL,C.ROW,WIDTH,DEPTH,C.FILE,C.ID,VALUE,FLD.NBRS,MV,ATTRS,JUSTS,HEADER)
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    GO MAIN$
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
    EQU MAX TO 999999
!
    MAIN$:!
!
    DEFC INT JBASEEmulateGETINT(INT, INT)
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    vm_start=IF_COMPILED_PRIME
    WINTEGRATE=0
    GUI=0
    BLD.SCRN=((C.COL:C.ROW:WIDTH:DEPTH)='0000')
    EOF=TRUE
    IF FG_TERM.TYPE[2,1]='V' THEN FG_KEEP.CHOICES=FALSE
    POPUP.LIST.BOX=NOT(C.COL<2>); C.COL=C.COL<1>
!
    SAVE.COLOURS=FG_CURR.COLOURS:AM:FG_PREV.COLOURS
    HILINE=''       ;! used for tagged lines
    FOOTER=''
    TRANSLATE=FALSE
    RV.EMBED=(1-EMBED.ATTR<1,1>)
    GR.EMBED=(1-EMBED.ATTR<1,2>)
    IF UNASSIGNED(FG_SCR.CO.ORDS) THEN FG_SCR.CO.ORDS=''
    CC=C.COL+FG_SCR.CO.ORDS<1,1>*(FG_TERM.TYPE[2,1]='V')
    RR=C.ROW+FG_SCR.CO.ORDS<1,2>*(FG_TERM.TYPE[2,1]='V')
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
    IF MV NE '' THEN
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
        IF ID NE '' THEN ATTRS=1 ELSE ATTRS=0
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
        IF JUST NE '' AND NOT(FIRST.VAL) THEN FIRST.VAL=A
        IF COL.HDS NE '' THEN
            IF JUST NE '' THEN
                JUST.LEN=LEN(1 JUST)
                IF JUST[1,1] EQ 'R' THEN JUST='R#':JUST.LEN ELSE JUST='L#':JUST.LEN
                IF A LT NBR.ATTRS THEN JUST:=SPC
            END ELSE JUST='L#0'
            COL.JUSTS<1,1,A>=JUST
        END
        ATTR=ATTRS<1,1,A>
        IF ATTR[1,1] EQ 'S' THEN
            SRT = ATTR[1,3]
            ATTR = ATTR[4, MAX]
        END ELSE SRT = ''
        IF FIRST.ATTR EQ '' AND ATTR NE 'L' AND ATTR NE 'N' THEN     ;* ace
            FIRST.ATTR=A
            IF ATTR EQ 0 AND ID NE '' AND ID EQ ID<1,1> AND B$LIST EQ '' THEN
                SAVE.ATTRS=ATTRS
                VALUES=ID
                DISP.VALUES=VALUES
                VALUE=ID
                GO EXIT.2
            END
        END
        IF NOT(NUM(ATTR)) AND ATTR NE 'L' AND ATTR NE 'N' THEN    ;* ace
            fn=FIELD(ATTR,CTRL.F,1)
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
    IF RR LT 1 THEN RR=8
    IF WIDTH EQ '' THEN
        IF JUSTS NE '' THEN
            WIDTH=2
            GAP=0
            FOR A=1 TO NBR.ATTRS
                JUST=JUSTS<1,1,A>
                IF JUST NE '' THEN
                    WIDTH+=(GAP+LEN(1 JUST))
                    GAP=1
                END ELSE GAP=0
            NEXT A
        END ELSE WIDTH=40
    END
    IF CC+WIDTH GT (PWIDTH-2) THEN
        CC=(PWIDTH-2)-WIDTH
        IF CC LT 1 THEN CC=1
        RR+=1
    END
    IF DEPTH EQ '' THEN
        DEPTH=(PDEPTH-4)-RR
        IF DEPTH LT 10 THEN DEPTH=10
    END
    IF DEPTH LT 4 THEN DEPTH=4
    HASH='L#':WIDTH-2
    WIDTH+=4
    IF JUSTS EQ '' THEN JUSTS=HASH
    VALUE=''
    SORT.BY=JUSTS<1,1,FIRST.VAL>[1,1]
    IF INDEX('LR',SORT.BY,1) ELSE SORT.BY='L'
!  SORT.BY='A':SORT.BY
    IF JUSTS<2>  NE  '' THEN SORT.BY=JUSTS<2> ELSE SORT.BY='A':SORT.BY
    WARN.FLAG=TRUE
    fn=C.FILE
    IF fn NE fn<1,1> THEN LAST.CHOICE=DELETE(fn,1,1,0); fn=fn<1,1> ELSE LAST.CHOICE=''
    ORIG.ATTRS=ATTRS
    IF fn EQ '' THEN
        VALUES=ID
        EOF=TRUE
        IF INDEX(ATTRS,CTRL.L,1) THEN CALL EB_CHOICE.BLD(VALUES,ATTRS:AM:OFFSET)
        IF ATTRS<1,1,1> EQ 'N' THEN        ;* ace
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
        CASE fn='EB.WORK'; F.FILE=FG_WORK.FILE
        CASE fn='EB.HELP.INDEX'; F.FILE=FG_EB.HELP        ;!.INDEX
        CASE fn MATCHES "1N0N"
            IF FG_SCR.PAGING<1,fn> EQ FG_WINDOW THEN fn=WIN.PANE(fn) ELSE fn=DAT(fn)
            GOSUB OPEN.FILE
        CASE fn MATCHES "1N0N"; F.FILE=FILES(fn)
        CASE 1
            GOSUB OPEN.FILE
        END CASE
        IF ID NE '' AND ATTRS<1,1,1> NE 0 AND NOT(MULT.IDS) THEN
            READ VALUES FROM F.FILE,ID ELSE VALUES=''
            SORT.BY = ''
            FOR A=1 TO NBR.ATTRS
                ATTR = ATTRS<1,1,A>
                IF ATTR[1,1] EQ 'S' THEN
                    SORT.BY = ATTR[2,2]
                    ATTR = ATTR[4,MAX]
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
                IF INDEX(ATTRS<1,1,A>,CTRL.F,1) OR ATTRS<1,1,A> EQ 'L' ELSE ATTRS<1,1,A>=A
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
    IF LAST.CHOICE NE '' THEN VALUES<K.ATTR,-1>=LAST.CHOICE
    IF EXCEPTIONS NE '' THEN CALL EB_EXCEPTIONS(VALUES,EXCEPTIONS,ATTRS<1,1,1>)
    IF INDEX(ATTRS,CTRL.F,1) THEN
        DISP.VALUES=VALUES<1>
    END ELSE DISP.VALUES=VALUES<K.ATTR>
    NBR.VALS=1
    K.ATTR=ATTRS<1,1,1>
    VALUE=''
    IF DISP.VALUES EQ '' THEN GO EXIT.2
    USE.ATTR=(ATTRS=ORIG.ATTRS)
    IF K.ATTR NE 'L' THEN
        IF VALUES<K.ATTR> EQ VALUES<K.ATTR,1> THEN
            VALUE=VALUES<K.ATTR>
        END
    END ELSE
        IF DISP.VALUES<1> EQ DISP.VALUES<1,1> THEN
            VALUE=1
        END
    END
    IF VALUE NE '' THEN
        IF BLD.SCRN THEN
            DISP.ATTRS=''
        END ELSE
            GOSUB GET.LINE
            GO EXIT.2
        END
    END
!
    IF WINTEGRATE THEN
        CALL EB_ERRMSG(SPC, 0)
        NBR.VALS=DCOUNT(DISP.VALUES<1>,VM)
        PGE=1
        ST=1
        FI=NBR.VALS; INCR=NBR.VALS; NBR.PGES=1
        TRANSLATE=(SAVE.ATTRS#DISP.ATTRS AND NOT(MULT.IDS) OR CONVS#'')
        IF TRANSLATE ELSE DISP.ATTRS=ATTRS
STERM.RESUME:       !
        VALUE=INIT.VALUE
        PAGE.ITEMS=''
        LOOP
            IF PAGE.ITEMS<PGE> EQ '' THEN
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
            IF FG_DIALOG.BOX AND FG_FLD MATCHES "1N0N" THEN
                CC=FG_SCR.COL<1,FG_FLD>
                IF FG_SCR.PAGING<1,FG_FLD> THEN
                    FIRST.PAGE=FG_SCR.ST.PAGE<1,FG_WINDOW>
                    FIRST.ROW=FG_SCR.ROW<1,FIRST.PAGE>
                    SKIP=FG_SCR.NBR.LINES<1,FG_WINDOW>
                    THIS.ROWS=FG_SCR.NBR.ROWS<1,FG_WINDOW>
                    RR=FIRST.ROW+MOD(FG_PANE-1,THIS.ROWS)*SKIP
                END ELSE
                    RR=FG_SCR.ROW<1,FG_FLD>
                END
                IF CC EQ C.COL AND RR EQ C.ROW AND FG_ACT.CODE EQ FG_OPT.CODE THEN       ;! normal lookup ?
                    STD.LKUP=TRUE
                END ELSE STD.LKUP=FALSE
            END ELSE STD.LKUP=TRUE
            R.TABLE=HEADER:AM:COL.HDS:AM:JUSTS:AM:DEPTH:AM:CC:VM:RR:VM:STD.LKUP
            IF VALIDATE ELSE
            END
            R.TABLE<7>=(ST>INCR):VM:(EOICNT#NBR.VALS)
            DLG.NAME=FG_DBX.NAME:'_':FG_SCREEN.PAGE
            IF FG_DBX.NAME NE '' THEN
                R.TABLE<8>=DLG.NAME
                IF NOT(POPUP.LIST.BOX) THEN
                    R.TABLE<9>='L':FG_FLD
                END ;!ELSE R.TABLE<10>=DROPDOWN*FG_SCR.NBR.PAGES
            END
            MV=''; VALUE=INIT.VALUE
            CALL EB_WIN.CHOICES(R.TABLE,DISP.VALUES,VALUE,1,ATTRS,0,FG_W.COL,FG_W.ROW)
        WHILE VALUE=NEXT.SEL OR VALUE=PREV.SEL DO
            DIRECTION=1-2*(VALUE=PREV.SEL)
            ST+=(INCR*DIRECTION)
        REPEAT
        IF FG_DBX.NAME NE '' AND NOT(GUI) THEN
            IF VALUE[1,1] EQ STX THEN
                FG_NEXT.EVENT=VALUE
                CALL EB_WIN.COMSUB('DB ShowControl ':R.TABLE<8>:',':R.TABLE<9>:',0')
                VALUE=''
            END
        END
        IF VALUE EQ '' AND ((NBR.VALS>50 AND NOT(ST EQ 1 AND FI EQ NBR.VALS)) OR NOT(EOF)) THEN
            CALL EB_MERRMSG('',FG_ERROR.MSGS<124>,'',ANS,'Y':VM:'N')
            IF ANS EQ 'Y' THEN
                IF EOICNT EQ NBR.VALS AND ST EQ 1 ELSE
                    IDCNT=1; EOICNT=NBR.VALS
                    ATTRS=SAVE.ATTRS
                    DISP.VALUES=''
                    VALUES=''
                    EOF=FALSE
                    GOSUB RESUME.SEARCH
                END
                ORIG.COUNT=NBR.VALS
                GOSUB REFINE
                IF NBR.VALS NE ORIG.COUNT THEN
                    IF NBR.VALS EQ 0 THEN
                        CALL EB_MERRMSG('',FG_ERROR.MSGS<125>,'',ANS,'Y':VM:'N')
                        IF ANS EQ 'Y' THEN
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
            END
        END
        GO EXIT.2
    END
!
    IF K.ATTR EQ 'L' THEN
        NBR.VALS=DCOUNT(VALUES<ATTRS<1,1,FIRST.ATTR>>,VM)
    END ELSE
        NBR.VALS=DCOUNT(VALUES<K.ATTR>,VM)
    END
    IF NBR.VALS LT DEPTH THEN DEPTH=NBR.VALS+(COL.HDS NE '')
!
    WIDTH+=GR.EMBED
    CC-=GR.EMBED
!
    FG.COLOUR=FG_COLOURS<1,1,1>; BG.COLOUR=FG_COLOURS<1,1,2>
    BOX.COLOUR=FG_COLOURS<1,1,3>
    IF UNASSIGNED(WHITE) THEN MAT COLOURS=''
    IF WHITE<1,1> NE '' THEN CALL EB_CH_COLOUR(FG.COLOUR,BG.COLOUR)
    MENU.COLOURS=FG_CURR.COLOURS
    IF WHITE<1,1> NE '' THEN CALL EB_CH_COLOUR(BOX.COLOUR,BG.COLOUR)
    BOX.COLOURS=FG_CURR.COLOURS
    BEGIN CASE
    CASE FG_TERM.TYPE[2,1]='V'
        BOX.CLEAR=ESC:CHAR(8):'WCLOSE':CHAR(0)
        BOX.DRAW=ESC:CHAR(8):'WOPEN /S ':CC+2:',':RR:',':WIDTH-5:',':DEPTH+0:CHAR(0):ESC:CHAR(8):'WFRAME 1':CHAR(0):CLS
        CRT BOX.DRAW:
        IF HEADER NE '' THEN CRT ESC:CHAR(8):'WTITLE /C ':HEADER:CHAR(0):
        CC=-2; RR=-1
    CASE 1
        IF SCREEN.SAVE<1,1> NE '' THEN CALL EB_SCREEN.SAVE(FG_DISK.DRIVE:FG_CRT.PAGE)
!
        CALL EB_BOX(CC,RR,WIDTH,DEPTH+1,1,BOX.CLEAR,BOX.DRAW)
        IF HEADER NE '' THEN
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
    IF COL.HDS NE '' THEN
        RR+=1
        DEPTH-=1
        COL.HD=''
        FOR I=1 TO NBR.ATTRS
            JUST=COL.JUSTS<1,1,I>
            IF JUST NE 'L#0' THEN COL.HD:=COL.HDS<1,1,I> JUST
        NEXT I
        CRT @(CC,RR):BG:COL.HD HASH:FG:
    END
    RR+=1
    SUB.LENGTH=''
    SUB.CODES=FG_SRCH.CH:VM:FG_SEL.CH:VM:FG_TOP.CH:VM:FG_NXT.CH:VM:FG_PRV.CH:VM:FG_TAB.CH
    IF UNASSIGNED(FG_ID.CHOICES) THEN MAT CRT.PARAMS=''
    IF FG_ID.CHOICES NE VALUES<1> THEN SUB.CODES<1,-1>=FG_TAG.CH
    SUB.CODES<2>='1X'
    FIRST.VAL=0
    FOR A=1 TO NBR.ATTRS UNTIL FIRST.VAL
        IF JUSTS<1,1,A> NE '' THEN FIRST.VAL=A
    NEXT A
    RR-=1
    FOOTER.PREFIX=@(CC+WIDTH-11-2*GR.EMBED,RR+DEPTH+1):GROFF
    FOOTER.SUFFIX=GRON:@(CC+WIDTH,RR+DEPTH+1):GROFF
RESTART:  !
    R=1
    IF INIT.VALUE EQ '' THEN C=1 ELSE
        LOCATE INIT.VALUE IN VALUES<K.ATTR,vm_start> SETTING C ELSE C=1
    END
    NBR.PAGES=INT((NBR.VALS-1)/DEPTH)+1
    LAST.PGE=INT((NBR.VALS-1)/DEPTH)*DEPTH+1
    IF C GT DEPTH THEN
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
        CALL EB_INPUT_ZERO(REPLY,CC,RR+R,FG_INPUT.CODES,SUB.CODES,SUB.LENGTH)
        IF REPLY EQ SPC THEN FG_ACT.CODE=FG_TAG.CODE
        IF NOT(FG_ACT.CODE) AND REPLY EQ '' THEN
            IF INDEX(HILINE,1,1) ELSE
                IF K.ATTR EQ 'L' THEN VALUE=I ELSE VALUE=VALUES<K.ATTR,I>
            END
            FG_ACT.CODE=FG_END.CODE
        END
    UNTIL FG_ACT.CODE=FG_ABT.CODE OR FG_ACT.CODE=FG_END.CODE OR FG_ACT.CODE=FG_JMP.CODE DO
        BEGIN CASE
        CASE FG_ACT.CODE=FG_SEARCH.CODE
            LOOP
                GOSUB REFINE
            WHILE NBR.VALS=0 DO
                CALL EB_MERRMSG('',FG_ERROR.MSGS<125>,'',ANS,'Y':VM:'N')
                IF ANS EQ 'Y' THEN
                    IDCNT=1
                    EOF=FALSE
                    IF fn NE '' THEN
                        GOSUB RESUME.SEARCH
                        NBR.VALS=EOICNT
                    END ELSE
                        VALUES=ORIG.VALUES
                    END
                END ELSE GO EXIT.2
            REPEAT
            GOSUB SCROLL.PAGE
        CASE FG_ACT.CODE=FG_SEL.CODE
            IF HILINE<1,1> THEN
                HILINE=''
            END ELSE
                HILINE=1:STR(VM:1,NBR.VALS-1)
            END
            GOSUB SCROLL.PAGE
        CASE FG_ACT.CODE=FG_TAG.CODE
            IF HILINE<1,I> THEN
                HILINE<1,I>=''
                CRT @(RV.COL,RR+R):RVOFF:LINE HASH:
            END ELSE
                HILINE<1,I>=TRUE
            END
            FG_ACT.CODE=FG_SKP.CODE
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
        CASE FG_ACT.CODE=FG_SEARCH.CODE
        CASE FG_ACT.CODE=FG_SEL.CODE
        CASE FG_ACT.CODE=FG_SKP.CODE OR FG_ACT.CODE=FG_SKP.CODE
            IF I NE NBR.VALS THEN
                R+=1; I+=1
                IF R GT DEPTH THEN PGE+=1; GOSUB SCROLL.PAGE
            END ELSE
                I=1
                IF LAST.PGE NE 1 THEN PGE=1; GOSUB SCROLL.PAGE ELSE R=1
            END
        CASE FG_ACT.CODE=FG_BCK.CODE
            IF I GT 1 THEN
                R-=1; I-=1
                IF R LT 1 THEN
                    I-=(DEPTH-1)
                    PGE-=1; GOSUB SCROLL.PAGE
                    I+=DEPTH-1
                    R=DEPTH
                END
            END ELSE
                IF LAST.PGE EQ 1 THEN
                    I=NBR.VALS; R=DEPTH
                END ELSE
                    I=LAST.PGE; PGE=NBR.PAGES; GOSUB SCROLL.PAGE
                    I=NBR.VALS; R=NBR.VALS-LAST.PGE+1
                END
            END
        CASE FG_ACT.CODE=FG_TOP.CODE
            IF J GT 1 THEN
                I=1; PGE=1; GOSUB SCROLL.PAGE
            END ELSE CRT BELL:
        CASE FG_ACT.CODE=FG_NXTS.CODE
            J=I+DEPTH-R+1
            IF J LE NBR.VALS THEN
                I=J
                PGE+=1; GOSUB SCROLL.PAGE
            END ELSE CRT BELL:
        CASE FG_ACT.CODE=FG_PRVS.CODE
            J=I-DEPTH-R+1
            IF J GE 1 THEN
                I=J
                PGE-=1; GOSUB SCROLL.PAGE
            END ELSE CRT BELL:
        CASE FG_ACT.CODE=FG_TAB.CODE
            C=I
            LOOP
                C+=1
            UNTIL NOT(DIMMED<1,C>) OR C>NBR.VALS DO REPEAT
            IF C LE NBR.VALS THEN
                IF R+C-I GT DEPTH THEN
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
            IF C GT NBR.VALS THEN
                IF C GT J AND J NE 0 THEN J=0; GO CHAR.SEARCH ELSE C=1
            END
            MV=INT((C-1)/DEPTH)+1
            IF MV NE PGE THEN
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
    IF VALUE EQ '' AND NOT(EOF) THEN
        CALL EB_MERRMSG('','Continue with search (Y/N)','',ANS,'Y':@VM:'N')
        IF ANS EQ 'Y' THEN
            WARN.FLAG=TRUE
            GOSUB RESUME.SEARCH
            GO RESTART
        END
    END
    FG_CURR.COLOURS=SAVE.COLOURS<1>; FG_PREV.COLOURS=SAVE.COLOURS<2>
    IF SCREEN.SAVE<1,1> NE '' THEN
        IF FG_KEEP.CHOICES ELSE
            IF FG_TERM.TYPE[2,1] EQ 'V' THEN CRT BOX.CLEAR: ELSE
                CALL EB_SCREEN.REST(FG_DISK.DRIVE:FG_CRT.PAGE)
            END
        END
        CRT FG_CURR.COLOURS:
    END ELSE
        CRT FG_CURR.COLOURS:
!    IF FG_KEEP.CHOICES ELSE
        CRT BOX.CLEAR:
!    FG_REFRESH=FG_FULL.NOCLEAR
!      CALL EB_REPAINT.WINDOW(0,CC,RR-(COL.HDS#''),WIDTH,DEPTH+(COL.HDS#''))
!    END
    END
EXIT.2:   !
    INCLUDE EB.OS.INCLUDES PC.ON.CURSOR
    K.ATTR=SAVE.ATTRS<1,1,1>
    IF FG_ACT.CODE EQ FG_END.CODE AND INDEX(HILINE,1,1) THEN
        MV=''; MVNDX=0
        FOR I=1 TO NBR.VALS
            IF HILINE<1,I> THEN
                IF K.ATTR EQ 'L' THEN STMP=I ELSE STMP=VALUES<K.ATTR,I>
                MVNDX+=1
                VALUE<1,MVNDX>=STMP
                MV<1,MVNDX>=I
            END
        NEXT I
    END
    IF VALUE NE '' THEN
        IF K.ATTR EQ 0 THEN K.ATTR=FIRST.ATTR
        IF MV EQ '' THEN
            CONVS<1,1,1>=-1
            IF K.ATTR EQ 'L' OR K.ATTR EQ 'N' THEN
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
                        IF K.ATTR EQ 'L' THEN STMP=A ELSE STMP=VALUES<ATTRS<1,1,A>,MV<1,J>>
                        VALUE<A,J>=STMP
                    NEXT J
                END
            NEXT A
        END
    END
    IF FG_ACT.CODE THEN FG_LAST.ACT.CODE=FG_ACT.CODE; FG_ACT.CODE=FALSE
    IF WINTEGRATE THEN
        LKUP.ID=R.TABLE<8>
        IF LKUP.ID NE '' AND POPUP.LIST.BOX THEN
            SCRIPT = "DB End ":LKUP.ID
            SCRIPT = "DB Delete ":LKUP.ID
            CALL EB_WIN.COMSUB(SCRIPT)
            CALL EB_WIN.HGLASS(1,0)
        END
    END
    RETURN
!
SCROLL.PAGE:        !
    IF NBR.VALS LT DEPTH THEN
        ODEPTH=DEPTH
        DEPTH=NBR.VALS
        NBR.PAGES=INT((NBR.VALS-1)/DEPTH)+1
        LAST.PGE=INT((NBR.VALS-1)/DEPTH)*DEPTH+1
    END ELSE ODEPTH=DEPTH
    FOR J=1 TO DEPTH
        MV=J+I-1
        MV=J+I-1
        IF MV LE NBR.VALS THEN GOSUB GET.LINE ELSE LINE=''
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
    IF NBR.PAGES GT 1 OR FOOTER NE '' THEN
        FOOTER=PGE 'R#2':'/':NBR.PAGES 'L#2'
        IF FG_TERM.TYPE[2,1] NE 'V' THEN
            CRT FOOTER.PREFIX:FOOTER:FOOTER.SUFFIX:
        END ELSE
            CRT ESC:CHAR(8):'WTITLE /B/R ':FOOTER:CHAR(0):
        END
    END
    R=1
    RETURN
!
GET.LINE: !
    IF MV EQ NBR.VALS AND LAST.CHOICE NE '' THEN
        IF WINTEGRATE OR FG_DIALOG.BOX THEN
            GAP=''
            LINE=''
            FOR A=1 TO NBR.ATTRS
                JUST=JUSTS<1,1,A>
                CONV=CONVS<1,1,A>
                IF K.ATTR EQ 'L' THEN STMP=MV ELSE STMP=LAST.CHOICE
                IF JUST NE '' OR FG_DIALOG.BOX THEN
                    BEGIN CASE
                    CASE FG_DIALOG.BOX
                        IF JUST EQ '' THEN
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
        IF CONV EQ -1 THEN CONV='' ; OCONV.IT=FALSE ELSE OCONV.IT=TRUE
        IF K.ATTR EQ 'L' THEN STMP=MV ELSE STMP=VALUES<K.ATTR,MV>
        Key=STMP
        IF CONV NE '' THEN Key=OCONV(Key,CONV)
        DISP.VALUES<1,MV>=Key
        IF JUST NE '' OR FG_DIALOG.BOX THEN
            BEGIN CASE
            CASE FG_DIALOG.BOX
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
            OK=(NOT(OCONV.IT) AND ATTR#'L' AND ATTR#'N' OR FG_DIALOG.BOX)
            IF JUST NE '' OR OK THEN
                CONV=CONVS<1,1,A>
                STMP=DISP.VALUES<A,MV>
                IF TRANSLATE OR STMP EQ '' THEN
                    IF INDEX(ATTR,CTRL.F,1) THEN
                        KPOS=FIELD(ATTR:CTRL.L,CTRL.L,2)
                        ATTR=ATTR[1,COL1()-1]
                        fn=FIELD(ATTR,CTRL.F,1)
                        IF fn THEN
                            ATTR=ATTR[COL2()+1,99]
                            IF KPOS THEN KPOS=VALUES<KPOS+KOFFSET,MV> ELSE KPOS=Key
                            READV STMP FROM OPENED.FILES(fn),KPOS,ATTR ELSE STMP=''
                        END ELSE STMP=''
!
! VALUES should contain columns of data therfore the A loop count should
! be used NOT the ATTR counter
                    END ELSE
                        IF USE.ATTR THEN STMP=VALUES<ATTR,MV> ELSE STMP=VALUES<A,MV>
                    END
                    OK=TRUE
                END
                IF CONV NE '' AND OK THEN
                    IF OCONV.IT THEN STMP=OCONV(STMP<1,1,1>,CONV) ELSE STMP=ICONV(STMP<1,1,1>,CONV)
                END
                BEGIN CASE
                CASE FG_DIALOG.BOX
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
    IF NOT(fn MATCHES "1N0N") THEN
        LOCATE fn IN OPEN.FILE.LIST<am_start> SETTING POS ELSE
            CALL EB_OPEN_FILE(fn,POS)
            IF NOT(POS) THEN
                VALUE=''
                RETURN TO EXIT.2
            END
        END
        IF POS THEN F.FILE=OPENED.FILES(POS)
    END ELSE POS=fn
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
            IF IDCNT GT EOICNT THEN
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
        IF OPERAND NE '' THEN
            IF OPER.ATTR THEN ATTR=DESC<OPER.ATTR,1,1> ELSE ATTR=ID
            BEGIN CASE
            CASE OPERAND='='
                IF ATTR[1,OPER.LEN] EQ OPER.TEST ELSE OK=FALSE
            CASE OPERAND='#'
                IF ATTR[1,OPER.LEN] NE OPER.TEST ELSE OK=FALSE
            CASE OPERAND='>'
                IF ATTR[1,OPER.LEN] GT OPER.TEST ELSE OK=FALSE
            CASE OPERAND='<'
                IF ATTR[1,OPER.LEN] LT OPER.TEST ELSE OK=FALSE
            END CASE
        END
        IF OK THEN
            IF DESC NE '' ELSE DESC=SPC
            ATTR=SAVE.ATTRS<1,1,FIRST.VAL>
            IF INDEX(ATTR,CTRL.F,1) THEN ATTR=FIELD(FIELD(ATTR,CTRL.F,2):CTRL.L,CTRL.L,1)
            IF UNSORTED THEN I=MV ELSE
                IF ATTR NE 0 THEN
                    LOCATE DESC<ATTR,1,1> IN VALUES<FIRST.VAL,vm_start> BY SORT.BY SETTING I ELSE NULL
                END ELSE
                    LOCATE ID IN VALUES<FIRST.VAL,vm_start> BY SORT.BY SETTING I ELSE NULL
                END
            END
            FOR A=1 TO NBR.ATTRS
                ATTR=SAVE.ATTRS<1,1,A>
                IF INDEX(ATTR,CTRL.F,1) THEN ATTR=FIELD(FIELD(ATTR,CTRL.F,2):CTRL.L,CTRL.L,1)
                IF ATTR NE 0 THEN
                    INS DESC<ATTR,1,1> BEFORE VALUES<A,I>
                END ELSE
                    INS ID BEFORE VALUES<A,I>
                END
            NEXT A
            MV+=1
            IF WARN.FLAG THEN IF I GT DEPTH THEN
                WARN.FLAG=FALSE
                IF FG_DIALOG.BOX ELSE CALL EB_ERRMSG(FG_ERROR.MSGS<65>, 0)
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
