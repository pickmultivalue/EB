! Side by side (or above/below) comparison tool
!
    $option jabba
    DEFFUN EBJSHOW()
    DEFFUN EBGETHOME()
    INCLUDE EB.EQUS EB.COMMON
    INCLUDE JBC.h
    DEFC INT JBASEEmulateGETINT(INT, INT)
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    vm_start=IF_COMPILED_PRIME
    EQU Bslsh TO '\', Fslsh TO '/'
    EQU MAX TO 9999
    INCLUDE EB.OS.INCLUDES TERM.SETTINGS
    path = EBGETHOME()
    jelf = new object('jelf_helper')
    jelf_opt = jelf->$hasmethod('getobject')
    OPEN path:'JET.PASTE' TO F.JET.PASTE ELSE STOP 'JET.PASTE'
    OPEN path:'SAVEDLISTS' TO F.PF ELSE
        OPEN path:'POINTER-FILE' TO F.PF ELSE
            STOP 201,'POINTER-FILE'
        END
    END
    CALL JBASEParseCommandLine1(args, TCL.OPTS, FG_SENTENCE)
    FG_SENTENCE=' ':CHANGE(FG_SENTENCE, @AM, ' ')
    IF TRIM(FG_SENTENCE) EQ '?' THEN
        CRT
        CRT 'Syntax: COMPARE_ITEM <left-dir/full-item-path><right-dir/full-item-path>'
        CRT
        CRT 'Can be run from an active select'
        CRT
        CRT 'Examples:'
        CRT
        CRT 'COMPARE_ITEM /old_tree/BP/INV_UPDATE.b /new_tree/BP/INV_UPDATE.b'
        CRT
        STOP
    END

    TCL.OPTS=OCONV(TCL.OPTS, 'MCU')
    PATCH.MODE=INDEX(TCL.OPTS,'P',1)
    BCKUP.MODE=INDEX(TCL.OPTS,'B',1)
    T.OPTION=INDEX(TCL.OPTS,'T',1)
    EQU SVM TO CHAR(252)
    EQU VM TO CHAR(253)
    EQU AM TO CHAR(254)
    EQU INTEG TO CHAR(230)
    EQU BELL TO CHAR(7)
    EQU OTHERWISE TO 1
    EQU TRUE TO 1, FALSE TO 0
    EQU SPC TO ' ', TAB TO CHAR(9)
    OPEN 'UPG.WORKFILE' TO F.UPG.WORKFILE THEN
        UPG=TRUE
        USEMODE=''; PASSWD=''
    END ELSE UPG=FALSE
    ORIG.DEPTH=SYSTEM(3)
    ORIG.WIDTH=SYSTEM(2)
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.OS.INCLUDES WHO
    CALL EB_UT_INIT
    EQU ESC TO CHAR(27)
    STLN=1
    CNTA=0
    CNTB=0
    ASENT=FALSE; BSENT=FALSE
    AISENT=FALSE; BISENT=FALSE
    DELAREQ=FALSE; DELBREQ=FALSE
    DELSAVEA=FALSE; DELSAVEB=FALSE
    VERT.FLAG=FALSE
    WIDE.FLAG=TRUE
    DEEP.FLAG=TRUE
    WRAP.FLAG=FALSE
    READN=FALSE
    DEFAULT.OFFSET=99
    NORMAL.LEN=SYSTEM(2)
    NORMAL.WIDTH=SYSTEM(3)
    WIDE.LEN=NORMAL.LEN*2
    COL.WIDTH = (WIDE.LEN-8)/2
    COL.POS = COL.WIDTH-1
    NORMAL.DEPTH=23
    WIDE.DEPTH=PDEPTH
    TERM=SYSTEM(7)
    TOGGLE='' ;!FWD.PAGE;!ESC:'J'
    TOGGLE2=BACK.PAGE   ;!ESC:'K'
    IF TOGGLE='' THEN
        TOGGLE=CLS
        TOGGLE2=''
    END
    WIDE.MODE=COL.132
    NORM.MODE=COL.80
    BEGIN CASE
        CASE TERM EQ 'B'
!      WIDE.MODE=ESC:'[=;132Z'
            DEEP.MODE=ESC:'[=33;Z'
!      NORM.MODE=ESC:'[=;80Z'
            SHALLOW.MODE=ESC:'[=25;Z'
        CASE TERM[1,1] EQ 'W'
            DEEP.MODE=ESC:"e":"*"
!      WIDE.MODE=ESC:'`;'
!      NORM.MODE=ESC:'`:'
            SHALLOW.MODE=ESC:'e&'
        CASE TERM EQ 'Q' OR TERM EQ 'U'     ;! these were copied from W and are probably wrong
            DEEP.MODE=ESC:"e":"*"
!      WIDE.MODE=ESC:'`;'
!      NORM.MODE=ESC:'`:'
            SHALLOW.MODE=ESC:'e&'
        CASE 1
            DEEP.MODE=''
            SHALLOW.MODE=''
    END CASE
    CLEOL=@(-4)
    EL=@(0,PDEPTH-1):CLEOL
    HIOFF=BG:RVOFF
    HION= FG:RVON
    REV.OFF=RVON
    REV.ON=RVOFF
    CL=@(0,NORMAL.DEPTH):CLEOL:HION:REV.ON
    CRT @(-1)
    CRT @(0,0):'COMPARE.ITEMS':TIMEDATE() 'R#63':
    PROMPT ''
    FA=''; IDA=''
    FVB=''; IDB=''
    CHANGEDA=''; CHANGEDB=''
    ORIG.CHANGEDA=''; ORIG.CHANGEDB=''
    LAST.EXEC=''
    SEL=SYSTEM(11)
    IF NOT(SEL) THEN
        ITNM=FIELD(FG_SENTENCE,' ',2)
        IF ITNM#'' THEN
            AISENT=TRUE
            IF INDEX(ITNM, Bslsh, 1) THEN
                g_slsh=Bslsh
                b_slsh=Fslsh
            END ELSE
                g_slsh=Fslsh
                b_slsh=Bslsh
            END
            CONVERT b_slsh TO g_slsh IN ITNM
            IF INDEX(ITNM,g_slsh,1) THEN
                FLNM=ITNM
                ITNM=FIELD(FLNM,g_slsh,DCOUNT(FLNM,g_slsh))
                FLNM=FLNM[1,COL1()-1]
            END ELSE
                INCLUDE EB.OS.INCLUDES GET.FLNM
            END
            IDA=ITNM
            IF FLNM#'' THEN FA=FLNM; ASENT=TRUE
            ITNM=FIELD(FG_SENTENCE,' ',3)
            IF ITNM#'' THEN
                IF INDEX(ITNM, Bslsh, 1) THEN
                    g_slsh=Bslsh
                    b_slsh=Fslsh
                END ELSE
                    g_slsh=Fslsh
                    b_slsh=Bslsh
                END
                BISENT=TRUE
                CONVERT b_slsh TO g_slsh IN ITNM
                IF INDEX(ITNM,g_slsh,1) THEN
                    FLNM=ITNM
                    ITNM=FIELD(FLNM,g_slsh,DCOUNT(FLNM,g_slsh))
                    FLNM=FLNM[1,COL1()-1]
                END ELSE
                    INCLUDE EB.OS.INCLUDES GET.FLNM
                END
                IDB=ITNM
                IF FLNM#'' THEN FVB=FLNM; BSENT=TRUE
            END
        END
    END ELSE
        FA=FIELD(FG_SENTENCE,' ',2)
        IF FA EQ 'DICT' THEN FA='DICT ':FIELD(FG_SENTENCE,' ',3)
        ASENT=(FA#'')
        id_list = ''
        EOF=0
        LOOP
            READNEXT ID ELSE EOF=1
        UNTIL EOF DO
            LOCATE ID IN id_list<am_start> BY 'AL' SETTING IPOS ELSE
                INS ID BEFORE id_list<IPOS>
            END
        REPEAT
        NbrRecs = DCOUNT(id_list, @AM)
        k = 0
    END
100 ! Enter First File Name
    LOOP
        IF ASENT THEN ASENT=FALSE ELSE
            CRT @(0,2):'Enter file A: ':@(-4):
            INPUT FA
        END
    WHILE FA EQ '?' DO
        FH = 'A'
        GOSUB SHOW_FILE_HELP
    REPEAT
    IF FA EQ 'EX' THEN GO 99999
    CRT @(-3):
    IF FIELD(FA,' ',1) EQ 'DICT' THEN
        DICT='DICT'
        FA=FIELD(FA,' ',2)
    END ELSE
        DICT=''
    END
    OPEN DICT,FA TO FILEA ELSE
        CRT EL:'CANNOT OPEN ':FA:
        GO 100
    END
    UPGBACKUP=(FIELD(FA,'.',2)='UPGBACKUP')
    CRT EL:
    IF SEL THEN
        IF FIELD(FA,' ',1) EQ 'DICT' THEN POS=4 ELSE POS=3
        FVB=FIELD(FG_SENTENCE,' ',POS)
        IF FVB EQ 'DICT' THEN FVB='DICT ':FIELD(FG_SENTENCE,' ',POS+1)
        BSENT=(FVB#'' OR UPGBACKUP)
    END
    IF NOT(UPGBACKUP) THEN GOSUB OPEN.FILEB
    AOBJ = FIELD(FA, ',', 2) EQ 'OBJECT'
    BOBJ = FIELD(FB, ',', 2) EQ 'OBJECT'
    IF AOBJ THEN
        IF NOT(jelf_opt) THEN CRT 'Missing jelf_helper';STOP
        rc = IOCTL(FILEA, JBC_COMMAND_GETFILENAME, AOBJ)
        AOBJ := DIR_DELIM_CH
    END
    IF BOBJ THEN
        IF NOT(jelf_opt) THEN CRT 'Missing jelf_helper';STOP
        rc = IOCTL(FILEB, JBC_COMMAND_GETFILENAME, AOBJ)
        BOBJ := DIR_DELIM_CH
    END

110 ! Enter First Id
    IF SEL THEN
        IF k < NbrRecs THEN
            k++
            IDA = id_list<k>
        END ELSE
            SEL=FALSE; READN=FALSE
            GOSUB 1300
            GOTO 110
        END
    END ELSE
        IF AISENT THEN AISENT=FALSE ELSE
            LOOP
                CRT @(0,4):'ENTER ID A: ':@(-4):
                INPUT IDA
            WHILE IDA EQ '?' DO
                IDH = 'A'
                GOSUB SHOW_ID_HELP
            REPEAT
            CRT @(-3):
        END
    END
    IF IDA EQ 'EXK'[1,LEN(IDA)] THEN GO 99999
    IF IDA EQ '^' THEN GO 120
    IF AOBJ THEN
        RECA = jelf->getobject(AOBJ:IDA:'.so')->embed_source
    END ELSE
        READ RECA FROM FILEA,IDA ELSE
            CRT EL:IDA:' NOT IN ':FA:
            GO 110
        END
    END
    CRT EL:
!
130 ! Enter Second Id
    IF SEL THEN
        IF UPGBACKUP THEN
            FVB=FIELD(IDA,'\',2)
            GOSUB OPEN.FILEB
            IDB=FIELD(IDA,'\',3)
        END ELSE
            BEGIN CASE
                CASE INDEX(IDA,'@',1) AND PATCH.MODE
                    IDB=FIELD(IDA,'@',2)
                CASE INDEX(IDA,'*',1) AND BCKUP.MODE
                    IDB=FIELD(IDA,'*',DCOUNT(IDA,'*'))
                CASE 1
                    IDB=IDA
            END CASE
        END
    END ELSE
131     !
        IF BISENT THEN BISENT=FALSE ELSE
            LOOP
                CRT @(42,4):'ENTER ID B: ':@(-4):
                INPUT IDB
            WHILE IDB EQ '?' DO
                IDH = 'B'
                GOSUB SHOW_ID_HELP
            REPEAT
            CRT @(-3):
        END
    END
    IF IDB EQ '' THEN IDB=IDA
    IF IDB EQ '^' THEN GO 110
    IF IDB EQ 'EX' THEN GO 99999
    IF BOBJ THEN
        RECB = jelf->getobject(BOBJ:IDB:'.so')->embed_source
    END ELSE
        READ RECB FROM FILEB,IDB ELSE
            CRT EL:IDB:' NOT IN ':FVB:
            STOP
            GO 131
        END
    END
    IF RECA EQ RECB THEN GOTO 110
    IF READN THEN GOTO 200
    CRT EL:
    CRT @(25,6):'--- OPTIONS ---':
150 ! Enter Display Mode
    CRT @(10,8):'HORIZONTAL OR VERTICAL DISPLAY (H/V): ':
    INPUT OPT
    IF OPT EQ 'EX' THEN GO 99999
    IF OPT EQ '^' THEN GO 130
    IF OPT EQ 'H' THEN VERT.FLAG=FALSE ELSE VERT.FLAG=TRUE
160 ! Enter Display Type
    CRT @(10,10):'WIDE OR NORMAL SCREEN (W/N): ':
    INPUT OPT
    IF OPT EQ 'EX' THEN GO 99999
    IF OPT EQ '^' THEN GO 150
    IF INDEX('W',OPT,1) THEN WIDE.FLAG=TRUE ELSE WIDE.FLAG=FALSE
170 ! Enter Display Level
    CRT @(10,12):'DEEP OR NORMAL SCREEN DEPTH (D/N): ':
    INPUT OPT
    IF OPT EQ 'EX' THEN GO 99999
    IF OPT EQ '^' THEN GO 160
    IF INDEX('D',OPT,1) THEN DEEP.FLAG=TRUE ELSE DEEP.FLAG=FALSE
!!!!!!!!!!!!!!!!!!!!!!!!
200 ! Mainline
    IF SEL THEN READN=TRUE
!
! Save Items In-Case You Do Somethine Stupid With Copy/Merge Commands
!
    IF AOBJ THEN
        RECA = jelf->getobject(AOBJ:IDA:'.so')->embed_source
    END ELSE
    READ RECA FROM FILEA,IDA THEN
        IF INDEX(IDA,'@',1) THEN
            TMP=''
            FOR I=1 TO TMP
                TMP<I>=RECA<1>; DEL RECA<1>
            NEXT I
        END ELSE TMP=''
        IF TMP#'' THEN
            INS TMP BEFORE RECA<1>
            DATA 'A10'
        END
    END ELSE RECA=''
    END
    IF BOBJ THEN
        RECB = jelf->getobject(BOBJ:IDB:'.so')->embed_source
    END ELSE
        READ RECB FROM FILEB,IDB ELSE RECB=''
    END
    NDA='%':IDA:'%'
    SAVA='%':IDA:'.sav%'
    BCKA='%':IDA:'.bck%'
    NDB='%':IDB:'%'
    SAVB='%':IDB:'.sav%'
    BCKB='%':IDB:'.bck%'
    integrate = @FALSE
    IF FA:IDA EQ FVB:IDB THEN ;! integrate?
        IF INDEX(RECA, @AM:'>>>>', 1) AND INDEX(RECA, @AM:'====', 1) AND INDEX(RECA, '<<<<', 1) THEN
            integrate = @TRUE
            occ = 1
            LOOP
                pos = INDEX(RECA, '<<<<<<<', occ)
            WHILE pos DO
                IF pos GT 1 AND RECA[pos-1,1] NE @AM THEN
                    occ++
                END ELSE
                    amc = DCOUNT(RECA[1, pos+1], @AM)
                    DEL RECA<amc>
                    DIFFA = ''
                    AMA = 1
                    LOOP
                        LINEA = RECA<amc>
                        DEL RECA<amc>
                    UNTIL LINEA[1,7] EQ '=======' DO
                        DIFFA<AMA> = LINEA
                        AMA++
                    REPEAT
                    DIFFB = ''
                    AMB = 1
                    LOOP
                        LINEB = RECA<amc>
                    UNTIL LINEB[1,7] = '>>>>>>>' DO
                        DEL RECA<amc>
                        DIFFB<AMB> = LINEB
                        AMB++
                    REPEAT
                    AMC = MAXIMUM(AMA:@AM:AMB)-1
                    LINEA = ''
                    FOR A = 1 TO AMC
                        LINEA<A> = INTEG:DIFFA<A>:INTEG:DIFFB<A>
                    NEXT A
                    RECA<amc> = LINEA:INTEG:AMA:INTEG:AMB
                END
            REPEAT
            RECB = RECA
        END
    END
    ORIGA=RECA
    ORIGB=RECB
!
    IF UPGBACKUP THEN
        TMP=FIELD(IDA,'\',1)
        TMP=FIELD(TMP,'*',1) 'D2/':'@':FIELD(TMP,'*',2) 'MTS'
    END ELSE TMP=IDA
    DIS.IDA=FA:' - ':TMP
    DIS.IDB=FVB:' - ':IDB
    PREV.LOC=''
    STARTA=1
    STARTB=1
    GOSUB 1300 ;! set screen type
    GOSUB 600  ;! Format Display Mode
    GOSUB 900  ;! display both items
210 ! Enter Command Option
    IF DEEP.FLAG THEN
        CL=@(0,WIDE.DEPTH):CLEOL:HION:REV.ON
        POS1=@(COL.POS,WIDE.DEPTH)
        POS2=@(0,WIDE.DEPTH)
    END ELSE
        CL=@(0,NORMAL.DEPTH):CLEOL:HION:REV.ON
        POS1=@(COL.POS,NORMAL.DEPTH)
        POS2=@(0,NORMAL.DEPTH)
    END
    CRT CL:POS1:'ENTER "?" FOR HELP':POS2:FG:REV.ON:'COMMAND: ':
    INPUT CMD:
    CMDU=OCONV(FIELD(CMD,' ',1),'MCU')
    CMD=CMDU:CMD[COL2(),999]
    CRT RVOFF:
    LASTA=DCOUNT(RECA,AM)
    LASTB=DCOUNT(RECB,AM)
    BEGIN CASE
        CASE CMD EQ 'RFR'
            GO 200
        CASE CMD EQ 'W'        ;! wide screen
            WIDE.FLAG=TRUE
            GOSUB 1300
            GOSUB 600 ;! Format Display Mode
            GOSUB 900 ;! display both items
        CASE CMD EQ 'N'        ;! Normal Screen
            WIDE.FLAG=FALSE
            DEEP.FLAG=FALSE
            GOSUB 1300
            GOSUB 600 ;! format display mode
            GOSUB 900 ;! Display Both Items
        CASE CMD EQ 'D'        ;! deep screen
            DEEP.FLAG=TRUE
            GOSUB 1300
            GOSUB 600 ;! Format Display Mode
            GOSUB 900 ;! display both items
        CASE CMD[1,1] EQ 'G'   ;! Go To Line Number
            NBR=FIELD(CMD,' ',2)
            IF NBR # '' AND NUM(NBR) THEN
                STARTA=NBR
                STARTB=NBR
                GOSUB 900 ;! display items
            END
        CASE CMD[1,1] EQ 'R' AND OCONV(CMD,'MCN') ;! replace
            msg = 'Syntax error'
            SUBSTRS = FIELD(CMD, '/', 2, 99)
            CMD = FIELD(CMD, '/', 1)
            FR.RANGE = 3
            SIDES = 1
            SIDE = 'A'
            BEGIN CASE
                CASE CMD[2,1] EQ 'A'; DELREC = RECA
                CASE CMD[2,1] EQ 'B'; DELREC = RECB; SIDE = 'B'
                CASE 1
                    DELREC = RECA
                    SIDES = 2
                    FR.RANGE = 2
            END CASE

            FR.RANGE = TRIM(CMD[FR.RANGE,-1])
            IF FR.RANGE MATCHES "1N0N" OR FR.RANGE MATCHES "1N0N','1N0N" OR FR.RANGE MATCHES "1N0N'-'1N0N" THEN
                CONVERT '/' TO @AM IN SUBSTRS
                SUBSTRS = CHANGE(SUBSTRS, '\':@AM, '/')
                oldstr = SUBSTRS<1>
                newstr = SUBSTRS<2>
                IF INDEX(FR.RANGE, '-', 1) THEN
                    FR.ST=FIELD(FR.RANGE,'-',1); FR.FI=FIELD(FR.RANGE,'-',2)
                    IF NOT(FR.FI) THEN FR.FI = FR.ST
                END ELSE
                    FR.ST=FIELD(FR.RANGE,',',1); FR.FI=FIELD(FR.RANGE,',',2)
                    IF NOT(FR.FI) THEN FR.FI = 1
                    FR.FI = FR.ST + FR.FI - 1
                END
                GOSUB CHECKRANGE
                IF RANGE_OK THEN
                    ROFFSET = AMB - AMA
                    FOR S = 1 TO SIDES
                        msg = ''
                        FOR I = FR.ST TO FR.FI
                            DELREC<I> = CHANGE(DELREC<I>, oldstr, newstr)
                        NEXT I
                        IF CMD[2,1] NE 'B' THEN
                            RECA = DELREC
                        END ELSE
                            RECB = DELREC
                        END
                        IF SIDES EQ 2 THEN
                            DELREC = RECB
                            CMD = 'RB'
                            FR.ST += ROFFSET
                            FR.FI += ROFFSET
                        END
                    NEXT S
                    GOSUB 900
                END
            END
            CRT @(0,CMD.ROW):CLEOL:msg:
        CASE CMD[1,2] EQ 'RA' OR CMD[1,2] EQ 'R ' OR CMD EQ 'R'    ;! Readjust Item A
            OFFSET=FIELD(CMD,' ',2)
            IF NUM(OFFSET) THEN
                IF OFFSET EQ '' THEN OFFSET=DEFAULT.OFFSET
                DEFAULT.OFFSET=OFFSET
                GOSUB 1100          ;! Readjust Item A
                GOSUB 900 ;! Display Both Items
            END
        CASE CMD[1,2] EQ 'RB'  ;! Readjust B
            OFFSET=FIELD(CMD,' ',2)
            IF NUM(OFFSET) THEN
                IF OFFSET EQ '' THEN OFFSET=DEFAULT.OFFSET
                DEFAULT.OFFSET=OFFSET
                GOSUB 1200          ;! Readjust B
                GOSUB 900 ;! Display Both Items
            END
        CASE CMD EQ 'EX' OR CMD EQ 'FI' OR CMD EQ 'FS'
FILE.ITEM:!
            GOSUB UPDATE
            IF CMD#'' THEN
                IF SEL THEN GO 110
                WIDE.FLAG=FALSE
                DEEP.FLAG=FALSE
                GOSUB 1300
                CALLSTACK = SYSTEM(16)
                IF CALLSTACK THEN GOTO 99999
                CRT @(-1)
                CRT @(0,0):'COMPARE.ITEMS':TIMEDATE() 'R#63':
                CRT @(0,2):'Enter file A: ':FA
                CRT @(42,2):'Enter file B: ':FVB:
                GO 110
            END
        CASE CMD EQ 'EXK' OR CMD EQ 'FIK'
            WIDE.FLAG=FALSE
            DEEP.FLAG=FALSE
            GOSUB UPDATE
            IF CMD#'' THEN
                GOSUB 1300
                GOTO 99999
            END
        CASE CMD[1,1] EQ 'F'   ;! Find Next Difference
            GOSUB 1000          ;! Find Next Difference
            IF CHKA#CHKB THEN
                GOSUB 900 ;! Display Both Items
            END ELSE
                CRT @(0,CMD.ROW):CLEOL:'No more differences':
            END
        CASE CMD[1,2] EQ 'CW' OR CMD[1,2] EQ 'CF'     ;! Copy Whole item
            IF CMD[3,1] EQ 'B' THEN
                RECA=RECB
            END ELSE RECB=RECA
            IF CMD[1,2] EQ 'CF' THEN
                CMD = 'FS'
                GO FILE.ITEM
            END
            GOSUB 600
        CASE CMD[1,1] EQ 'I' ;! insert blank lines
            msg = 'Syntax error'
            FR.RANGE = TRIM(CMD[3,-1])
            IF FR.RANGE MATCHES "1N0N" OR FR.RANGE MATCHES "1N0N','1N0N" THEN
                SIDE = 'A'
                SIDES = 1
                BEGIN CASE
                    CASE CMD[2,1] EQ 'A'; DELREC = RECA
                    CASE CMD[2,1] EQ 'B'; DELREC = RECB; SIDE = 'B'
                    CASE 1; DELREC = ''
                END CASE
                IF LEN(DELREC) THEN
                    FR.ST=FIELD(FR.RANGE,'-',1); FR.FI=FIELD(FR.RANGE,'-',2)
                    IF NOT(FR.FI) THEN FR.FI = 1
                    FR.FI = FR.ST + FR.FI - 1
                    GOSUB CHECKRANGE
                    IF RANGE_OK THEN
                        msg = ''
                        FOR I = FR.ST TO FR.FI
                            INS '' BEFORE DELREC<FR.ST>
                        NEXT I
                        IF CMD[2,1] EQ 'A' THEN
                            RECA = DELREC
                        END ELSE
                            RECB = DELREC
                        END
                        GOSUB 900
                    END
                END
            END
            CRT @(0,CMD.ROW):CLEOL:msg:
        CASE CMD[1,2] EQ 'DE' ;! delete lines
            msg = 'Syntax error'
            FR.RANGE = 4
            SIDES = 1
            SIDE = 'A'
            BEGIN CASE
                CASE CMD[3,1] EQ 'A'; DELREC = RECA
                CASE CMD[3,1] EQ 'B'; DELREC = RECB; SIDE = 'B'
                CASE 1
                    DELREC = RECA
                    SIDES = 2
                    FR.RANGE = 3
            END CASE
            FR.RANGE = TRIM(CMD[FR.RANGE,-1])
            IF FR.RANGE MATCHES "1N0N" OR FR.RANGE MATCHES "1N0N','1N0N" OR FR.RANGE MATCHES "1N0N'-'1N0N" THEN
                IF INDEX(FR.RANGE, '-', 1) THEN
                    FR.ST=FIELD(FR.RANGE,'-',1); FR.FI=FIELD(FR.RANGE,'-',2)
                    IF NOT(FR.FI) THEN FR.FI = FR.ST
                END ELSE
                    FR.ST=FIELD(FR.RANGE,',',1); FR.FI=FIELD(FR.RANGE,',',2)
                    IF NOT(FR.FI) THEN FR.FI = 1
                    FR.FI = FR.ST + FR.FI - 1
                END
                IF FR.FI GE FR.ST THEN
                    GOSUB CHECKRANGE
                    IF RANGE_OK THEN
                        msg = ''
                        ROFFSET = AMB - AMA
                        FOR S = 1 TO SIDES
                            FOR I = FR.ST TO FR.FI
                                DEL DELREC<FR.ST>
                            NEXT I
                            IF CMD[3,1] NE 'B' THEN
                                RECA = DELREC
                            END ELSE
                                RECB = DELREC
                            END
                            IF SIDES EQ 2 THEN
                                DELREC = RECB
                                CMD = 'DEB'
                                FR.ST += ROFFSET
                                FR.FI += ROFFSET
                            END
                        NEXT S
                        GOSUB 900
                    END
                END
            END
            CRT @(0,CMD.ROW):CLEOL:msg:
        CASE CMD[1,1] EQ 'C'   ;! Copy Text
            IF OCONV(CMD,'MCN') EQ '' THEN
                GOSUB 990
            END ELSE
                OK=TRUE
                IF CMD MATCHES "'C'1N0X" THEN CMD = 'CA ':CMD[2,-1]
                IF FIELD(CMD,' ',1) EQ 'C' THEN CMD = 'CA':CMD[COL2(),-1]
                IF CMD MATCHES "2A1N0X" THEN CMD = CMD[1,2]:' ':CMD[3,-1]
                SIDES = 2
                SIDE = 'A'
                BEGIN CASE
                    CASE CMD MATCHES "2A' '1N0N' '1N0N"
                    CASE CMD MATCHES "2A' '1N0N"
                    CASE CMD MATCHES "2A' '1N0N'-'1N0N"
                    CASE CMD MATCHES "2A' '1N0N'-'1N0N' '1N0N"
                    CASE CMD MATCHES "2A' '1N0N' '1N0N'-'1N0N"
                    CASE CMD MATCHES "2A' '1N0N'-'1N0N' '1N0N'-'1N0N"
                    CASE 1
                        CRT @(0,CMD.ROW):'Incomplete Copy command':CLEOL:
                        OK=FALSE
                END CASE
                IF OK THEN
                    FR.RANGE=FIELD(CMD,' ',2); TO.RANGE=FIELD(CMD,' ',3)
                    FR.ST=FIELD(FR.RANGE,'-',1); FR.FI=FIELD(FR.RANGE,'-',2)
                    IF FR.FI EQ '' THEN FR.FI=FR.ST
                    IF LEN(TO.RANGE) THEN
                        TO.ST=FIELD(TO.RANGE,'-',1); TO.FI=FIELD(TO.RANGE,'-',2)
                        IF TO.FI EQ '' THEN TO.FI=TO.ST
                    END ELSE
                        ROFFSET = AMB - AMA
                        IF CMD[2,1] EQ 'B' THEN ROFFSET = 0-ROFFSET
                        TO.ST = FR.ST + ROFFSET
                        TO.FI = FR.FI + ROFFSET
                    END
                    IF CMD[2,1] EQ 'B' THEN SIDE = 'B'
                    GOSUB CHECKRANGE
                    IF RANGE_OK THEN
                        IF CMD[2,1] EQ 'B' THEN
                            FOR I=TO.ST TO TO.FI
                                DEL RECA<TO.ST>
                            NEXT I
                            IF TO.ST=1 THEN MERGE.CODE=''; I=1 ELSE MERGE.CODE=RECA<TO.ST-1>; I=2
                            FOR AMB=FR.ST TO FR.FI
                                MERGE.CODE<I>=RECB<AMB>; I+=1
                            NEXT AMB
                            IF TO.ST=1 THEN INS MERGE.CODE BEFORE RECA<1> ELSE RECA<TO.ST-1>=MERGE.CODE
                            AMA=TO.ST
                            STLN=AMA-STARTA+1
                        END ELSE
                            FOR I=TO.ST TO TO.FI
                                DEL RECB<TO.ST>
                            NEXT I
                            IF TO.ST=1 THEN MERGE.CODE=''; I=1 ELSE MERGE.CODE=RECB<TO.ST-1>; I=2
                            FOR AMA=FR.ST TO FR.FI
                                MERGE.CODE<I>=RECA<AMA>; I+=1
                            NEXT AMA
                            IF TO.ST=1 THEN INS MERGE.CODE BEFORE RECB<1> ELSE RECB<TO.ST-1>=MERGE.CODE
                            AMB=TO.ST
                            STLN=AMB-STARTB+1
                        END
                        GOSUB 900
                    END
                END
            END
        CASE CMD[1,1] EQ 'M'   ;! Merge Text
            IF OCONV(CMD,'MCN') EQ '' THEN
                GOSUB 995
            END ELSE
                FR.RANGE=FIELD(CMD,' ',1)
                SIDES = 2
                SIDE = 'A'
                BEGIN CASE
                    CASE FR.RANGE EQ 'MA'
                    CASE FR.RANGE EQ 'MB'
                    CASE FR.RANGE[1,2] EQ 'MA'; CMD=CMD[1,2]:' ':CMD[3,999]
                    CASE FR.RANGE[1,2] EQ 'MB'; CMD=CMD[1,2]:' ':CMD[3,999]
                    CASE 1
                        CMD = 'MA ':TRIM(CMD[2,999])
                END CASE
                IF CMD MATCHES "2A' '1N0N" THEN
                    FR.RANGE=FIELD(CMD,' ',2)
                    CMD=CMD[1,COL1()]:FR.RANGE
                    CMD=CMD:' ':FIELD(FR.RANGE:'-','-',1)
                END
                IF CMD MATCHES "2A' '1N0N' '1N0N" THEN
                    FR.RANGE=FIELD(CMD,' ',2)
                    CMD=CMD[1,COL1()]:FR.RANGE:'-':FR.RANGE:CMD[COL2(),999]
                END
                IF CMD MATCHES "2A' '1N0N'-'1N0N' '1N0N" THEN
                    FR.RANGE=FIELD(CMD,' ',2); TO.RANGE=FIELD(CMD,' ',3)
                    FR.ST=FIELD(FR.RANGE,'-',1); FR.FI=FIELD(FR.RANGE,'-',2)
                    TO.ST=TO.RANGE; TO.FI=TO.RANGE
                    IF CMD[2,1] EQ 'B' THEN SIDE = 'B'
                    GOSUB CHECKRANGE
                    IF RANGE_OK THEN
                        IF CMD[2,1] EQ 'B' THEN
                            MERGE.CODE=RECA<TO.RANGE-1>
                            I=1
                            FOR AMB=FR.ST TO FR.FI
                                I++
                                MERGE.CODE<I>=RECB<AMB>
                            NEXT AMB
                            RECA<TO.RANGE-1>=MERGE.CODE
                        END ELSE
                            MERGE.CODE=RECB<TO.RANGE-1>
                            I=1
                            FOR AMA=FR.ST TO FR.FI
                                I++
                                MERGE.CODE<I>=RECA<AMA>
                            NEXT AMA
                            RECB<TO.RANGE-1>=MERGE.CODE
                        END
                        GOSUB 900
                    END
                END
            END
        CASE CMD[1,1] EQ 'S'   ;! Next Locate
            IF PREV.LOC # '' THEN
                searchText=PREV.LOC
                GO 215
            END
        CASE CMD EQ 'UNDO'
            READ RECA FROM FILEA,SAVA ELSE NULL
            READ RECB FROM FILEB,SAVB ELSE NULL
            GOSUB 900
        CASE CMD EQ 'EA'
!            DATA "?"
            GOSUB WRITEA
            DATA 'ED ':FA
            EXECUTE 'SELECT ':FA:' "':NDA:'"'
            READ RECA FROM FILEA,NDA ELSE NULL
            GOSUB 600
            GOSUB 900
        CASE CMD EQ 'EB'
!            DATA "?"
            GOSUB WRITEB
            DATA 'ED ':FVB
            EXECUTE 'SELECT ':FVB:' "':NDB:'"'
            READ RECB FROM FILEB,NDB ELSE NULL
            GOSUB 600
            GOSUB 900
        CASE CMD EQ 'EBA'
            GOSUB WRITEA
            DATA 'EB ':FA
            EXECUTE 'SELECT ':FA:' "':NDA:'"'
            READ RECA FROM FILEA,NDA ELSE NULL
            GOSUB 600
            GOSUB 900
        CASE CMD EQ 'EBB'
            GOSUB WRITEB
            DATA 'EB ':FVB
            EXECUTE 'SELECT ':FVB:' "':NDB:'"'
            READ RECB FROM FILEB,NDB ELSE NULL
            GOSUB 600
            GOSUB 900
        CASE CMD EQ 'I'
            GOSUB WRITEA
            DATA 'jEDIfmt ':FA
            EXECUTE 'SELECT ':FA:' "':NDA:'"'
            READ RECA FROM FILEA,NDA ELSE NULL
            GOSUB WRITEB
            DATA 'jEDIfmt ':FVB
            EXECUTE 'SELECT ':FVB:' "':NDB:'"'
            READ   RECB FROM FILEB,NDB ELSE NULL
            GOSUB 600
            GOSUB 900
        CASE CMD EQ 'SVN' OR CMD EQ 'GIT'
            OP=FIELD(CMD,' ',2)
            NewCmd=(IF CMD EQ 'SVN' THEN 'svn' ELSE 'git'):' ':OP
            OP=2
            LOOP
                arg=FIELD(CMD,' ',OP)
            UNTIL arg EQ '' DO
                BEGIN CASE
                    CASE OCONV(arg,'MCU') EQ 'A'
                        arg=FA:'/':IDA
                    CASE OCONV(arg,'MCU') EQ 'B'
                        arg=FVB:'/':IDB
                END CASE
                NewCmd:=SPC:arg
                OP++
            REPEAT
            EXECUTE NewCmd
        CASE CMD EQ 'JA'
            GOSUB WRITEA
            DATA 'JED ':FA
            EXECUTE 'SELECT ':FA:' "':NDA:'"'
            READ RECA FROM FILEA,NDA ELSE NULL
            GOSUB 600
            GOSUB 900
        CASE CMD EQ 'JB'
            GOSUB WRITEB
            DATA 'JED ':FVB
            EXECUTE 'SELECT ':FVB:' "':NDB:'"'
            READ RECB FROM FILEB,NDB ELSE NULL
            GOSUB 600
            GOSUB 900
        CASE CMD[1,3] EQ 'LOC'
            side = TRIM(CMD[4,1])
            IF LEN(side) EQ 0 THEN side = 'AB'
            POS=INDEX(CMD,' ',1)
            searchText=CMD[POS+1,999]
215         CRT CL:'Searching for "':searchText:'", please wait ':CLEOL:
! Find String In Item A
            IF INDEX(side, 'A', 1) THEN
                J=STARTA
220             !
                J+=1
                IF J>LASTA THEN
                    CRT EL:'"':searchText:'" NOT FOUND ':
                    GO 210
                END
                LINE=RECA<J>
                IF NOT(INDEX(LINE,searchText,1)) THEN GO 220
                STARTA=J
            END
! Find String In Item B
            IF INDEX(side, 'B', 1) THEN
                I=STARTB
221             !
                I+=1
                IF I>LASTB THEN
                    CRT CL:'"':searchText:'" NOT FOUND ':
                    INPUT RET,1:
                    GO 210
                END
                LINE=RECB<I>
                IF NOT(INDEX(LINE,searchText,1)) THEN GO 221
                STARTB=I
            END
            PREV.LOC=searchText
            GOSUB 900 ;! Display Both Items
        CASE CMD[1,1] EQ '='
            CMD=TRIM(FIELD(CMD,'=',2))
            IF CMD MATCHES "1N0N' '1N0N" THEN
                STARTA=FIELD(CMD, ' ', 1)
                STARTB=FIELD(CMD, ' ', 2)
                GOSUB 900 ;! Display Item A
            END
        CASE CMD[1,2] EQ 'A EQ ' OR CMD[1,3] EQ 'A ='
            CMD=TRIM(FIELD(CMD,'=',2))
            IF CMD#'' AND NUM(CMD) THEN
                STARTA=CMD
                GOSUB 900 ;! Display Item A
            END
        CASE CMD[1,2] EQ 'B=' OR CMD[1,3] EQ 'B ='
            CMD=TRIM(FIELD(CMD,'=',2))
            IF CMD#'' AND NUM(CMD) THEN
                STARTB=CMD
                GOSUB 900 ;! Display Item B
            END
        CASE CMD[1,1] EQ 'A'
            CMD=TRIM(CMD[2,99])
            IF CMD#'' AND NUM(CMD) THEN
                STARTA+=CMD
            END ELSE
                STARTA+=MAX.LINES
            END
            GOSUB 900 ;! Display Item A
        CASE CMD[1,1] EQ 'B'
            CMD=TRIM(CMD[2,99])
            IF CMD#'' AND NUM(CMD) THEN
                STARTB+=CMD
            END ELSE
                STARTB+=MAX.LINES
            END
            GOSUB 900 ;! Display Item B
        CASE CMD EQ '?'
            GOSUB 700 ;! Display Help Screen
        CASE CMD[1,2] EQ 'PR'
            GOSUB 800 ;! Print Items
        CASE CMD[1,1] EQ 'V'
            VERT.FLAG=TRUE
            GOSUB 600 ;! Format Display Mode
            GOSUB 900 ;! Display Both Items
        CASE CMD[1,1] EQ 'H'
            VERT.FLAG=FALSE
            GOSUB 600 ;! Format Display Mode
            GOSUB 900 ;! Display Both Items
        CASE CMD[1,3] EQ 'XEQ'
            CMD=TRIM(CMD[4,999])
            IF CMD EQ '' THEN CMD=LAST.EXEC
            IF CMD # '' THEN
                EXECUTE CMD
                LAST.EXEC=CMD
                CRT 'Press [RETURN] to continue ':
                INPUT RET,1:
                GOSUB 600 ;! Format Display Mode
                GOSUB 900 ;! Display Both Items
            END
        CASE CMD EQ 'TOP'
            STARTA=1
            STARTB=1
            GOSUB 900 ;! Display Both Items
        CASE NUM(CMD) AND CMD # ''
            STARTA+=CMD
            STARTB+=CMD
            GOSUB 900 ;! Display Both Items
        CASE CMD[1,1] EQ 'Z'   ;! zoom into multi-valued attribute
            IF UPG THEN
                AMA=OCONV(CMD,'MCN')
                AMB=STARTB+(AMA-STARTA)
                LINEA = RECA<AMA>
                CONVERT VM:SVM TO AM:VM IN LINEA
                UPGA='%COMPA%':IDA:'%':AMA:'%':FG_TLINE
                UPGB='%COMPB%':IDB:'%':AMB:'%':FG_TLINE
                WRITE LINEA ON F.UPG.WORKFILE,UPGA
                LINEB = RECB<AMB>
                CONVERT VM:SVM TO AM:VM IN LINEB
                WRITE LINEB ON F.UPG.WORKFILE,UPGB
                DATA 'UPG.WORKFILE',''
                DATA UPGA, UPGB
                DATA '','',''
                EXECUTE 'COMPARE_ITEM'
                READ LINEA FROM F.UPG.WORKFILE,UPGA ELSE NULL
                READ LINEB FROM F.UPG.WORKFILE,UPGB ELSE NULL
                CONVERT VM:AM TO SVM:VM IN LINEA
                RECA<AMA>=LINEA
                CONVERT VM:AM TO SVM:VM IN LINEB
                RECB<AMB>=LINEB
                DELETE F.UPG.WORKFILE,UPGA
                DELETE F.UPG.WORKFILE,UPGB
                GOSUB 900
            END
        CASE OTHERWISE
            STARTA+=MAX.LINES
            STARTB+=MAX.LINES
            GOSUB 900 ;! Display Both Items
    END CASE
    GO 210
!
400 ! Display Item A
    IF STARTA < 1 THEN STARTA=1
    IF STARTB < 1 THEN STARTB=1
    ENDA=STARTA+MAX.LINES
    ENDB=STARTB+MAX.LINES
    IF ENDA > 999 THEN PADA=4 ELSE PADA=3
    IF ENDB > 999 THEN PADB=4 ELSE PADB=3
    PDAJ='R%':PADA:' '
    PDBJ='R%':PADB:' '
    COL=START.COLA
    CRT HIOFF:
    FOR J=1 TO MAX.LINES
        AMA=STARTA+J-1
        NBRA=AMA PDAJ
        AMB=STARTB+J-1
        NBRB=AMB PDBJ
        GOSUB GETLINES
        IF TMPA # TMPB THEN
            PAD=REV.OFF
        END ELSE
            PAD=REV.ON
        END
        LINEA = CHANGE(TMPA, TAB, '\t')
        LINEB = CHANGE(TMPB, TAB, '\t')
        LINEA=OCONV(LINEA,'MCP')
        LINEB=OCONV(LINEB,'MCP')
        IF LINEA EQ '' THEN
            LINEA=BLANK.LINE
        END ELSE
            LINEA=NBRA:LINEA:BLANK.LINE
        END
        IF LINEB EQ '' THEN
            LINEB=BLANK.LINE
        END ELSE
            LINEB=NBRB:LINEB:BLANK.LINE
        END
        CMTA = CMTA[1, LINE.LEN-2 - LEN(LINEA)]
        CMTB = CMTB[1, LINE.LEN-2 - LEN(LINEB)]
        L = MAXIMUM(LEN(LINEA):@AM:LEN(LINEB))
        FOR C = 1 TO L
            IF LINEA[C,1] NE LINEB[C,1] THEN
                C--
                BREAK
            END
        NEXT L
        LINEA=LINEA[1,C]:PAD:TRIM(LINEA[C+1,-1], ' ', 'L')[1,LINE.LEN-2]
        LINEB=LINEB[1,C]:PAD:TRIM(LINEB[C+1,-2], ' ', 'L')[1,LINE.LEN-2]
        IF CMTA # CMTB THEN
            PAD=HION
        END ELSE
            PAD=HIOFF
        END
        IF LEN(CMTA) THEN
            LINEA:=PAD:CMTA
        END ELSE LINEA=TRIM(LINEA, ' ', 'T')
        IF LEN(CMTB) THEN
            LINEB:=PAD:CMTB
        END ELSE LINEB=TRIM(LINEB, ' ', 'T')
        CRT @(COL,START.ROWA+J):LINEA:RVOFF:CLEOL:
        CRT @(COL,START.ROWB+J):LINEB:RVOFF:CLEOL:
    NEXT J
499 !
    RETURN
600 ! Format Display Mode
    IF VERT.FLAG THEN
        IF WIDE.FLAG THEN
            LINE.LEN=WIDE.LEN/2
            DISPLAY.LEN=WIDE.LEN-1
        END ELSE
            LINE.LEN=40
            DISPLAY.LEN=79
        END
        IF DEEP.FLAG THEN MAX.LINES=WIDE.DEPTH-4 ELSE MAX.LINES=NORMAL.DEPTH-4
        CRT @(-1)
        CRT @(0,0):@(4,0):DIS.IDA:@(LINE.LEN+4,0):DIS.IDB:
        CRT @(0,1):STR('-',DISPLAY.LEN):
        CRT @(0,MAX.LINES+3):STR('-',DISPLAY.LEN):
        START.COLA=0
        START.COLB=LINE.LEN
        START.ROWA=1
        START.ROWB=1
    END ELSE
        MAX.LINES=(WIDE.DEPTH-4)/2 "0"
        IF WIDE.FLAG THEN
            LINE.LEN = WIDE.LEN
        END ELSE
            LINE.LEN=79
        END
        CRT @(-1)
        LINE=STR('-',20):' ':DIS.IDA:' ':STR('-',LINE.LEN-20)
        CRT @(0,0):REV.ON:LINE[1,LINE.LEN-2]:
        LINE=STR('-',20):' ':DIS.IDB:' ':STR('-',LINE.LEN-20)
        START.COLA=0
        START.COLB=0
        START.ROWA=0
        START.ROWB=MAX.LINES+1
        CRT @(0,START.ROWB):REV.ON:LINE[1,LINE.LEN-2]:
        CRT @(0,(MAX.LINES*2)+2):STR('-',LINE.LEN):
    END
    BLANK.LINE=STR(' ',LINE.LEN)
    RETURN
700 ! Display Help Screen
    CRT TOGGLE:@(0,0):CLEOP:'COMPARE.ITEMS':TIMEDATE() 'R#65':
    CRT @(15,1):'H E L P   S C R E E N':
    CRT @(10,2):'"number" - scroll both items by "number" of lines':
    CRT @(10,3):'"A number" - scroll A by "number" lines':
    CRT @(10,4):'"B number" - scroll B by "number" lines':
    CRT @(10,5):'"A=number" - set first line of A to "number"':
    CRT @(10,6):'"B=number" - set first line of B to "number"':
    CRT @(10,7):'"B=number" - scroll B by "number" lines':
    CRT @(10,8):'"V"ertical display':
    CRT @(10,9):'"H"orizontal display':
    CRT @(10,10):'"LOC"ate string':
    CRT @(10,11):'"S"earch again':
    CRT @(10,12):'"F"ind next difference; "R{[<A>,B}}" re-adjust items':
    CRT @(10,13):'"TOP" of items':
    CRT @(10,14):'"EA" edit item A; "EB" edit item B':
    CRT @(10,15):'"PR"int comparsion':
    CRT @(10,16):'"XEQ"ECUTE A TCL STATEMENT':
    CRT @(10,17):'"C{[<A>,B]}"opy different lines':
    CRT @(10,18):'"CW{[<A>,B]}"opy record':
    CRT @(10,19):'"M{[<A>,B]}"erge different lines':
    CRT @(10,20):'"C{{[<A>,B]} a-b n-m}" replace lines n-m with a-b':
    CRT @(10,21):'"M{{[<A>,B]} a-b n}" merge lines a-b before n':
    CRT @(10,22):'"DE{{[<A>,B]} a{-b}}" delete line a or lines a-b':
    CRT @(10,23):'"I{{[<A>,B]} a{,b}}" insert a blank line at a or b lines at a':
    CRT @(10,24):'"R{{[<A>,B]} a{,b}}" insert a blank line at a or b lines at a':
    CRT @(10,25):'"EX{K}" exit without save; "FI{K}" file changes':
    CRT @(10,26):'"UNDO restore prior to last changes':
    CRT @(0,CMD.ROW):'Press [RETURN] to continue ':CLEOL:
    INPUT RET,1:
    IF TOGGLE2#'' THEN
        CRT TOGGLE2:
    END ELSE
        GOSUB 600
        GOSUB 900
    END
    RETURN
800 ! Print Items Print Comparison
    LINE.NO=99
    MAX.PAGE=55
    HD='COMPARE.ITEMS':TIMEDATE() 'R#119'
    HA=FA:' - ':IDA
    HB=FVB:' - ':IDB
    PRINTER ON
    HL=SPACE(6):(HA:SPACE(COL.WIDTH))[1,COL.WIDTH]:' | ':HB[1,COL.WIDTH]
    J=0
810 !
    J+=1
    LINE.NO+=1
    IF LINE.NO>MAX.PAGE THEN
        PRINT TOF
        PRINT HD
        PRINT HL
        PRINT STR('-',WIDE.LEN)
        LINE.NO=4
    END
    LINEA=RECA<J>
    LINEB=RECB<J>
    IF LINEA EQ '' AND LINEB EQ '' THEN GO 820
    NBR=STR('0',4-LEN(J)):J:'| '
    LINE=NBR:(LINEA:SPACE(COL.WIDTH))[1,COL.WIDTH]:' | ':LINEB[1,COL.WIDTH]
    PRINT LINE
    GO 810
820 !
    PRINTER OFF
    PRINTER CLOSE
    GOSUB 600 ;! Format Display Mode
    GOSUB 900 ;! Display Both Items
    RETURN
!!!!!!!
900 ! Display A Page
! Do restore backup
    READ BCKA FROM FILEA,NDA ELSE BCKA = RECA
    IF RECA NE BCKA THEN
        DELSAVEA=TRUE
        WRITE BCKA ON FILEA,SAVA
        GOSUB WRITEA
    END

    READ BCKB FROM FILEB,NDB ELSE BCKB = RECB
    IF RECB NE BCKB THEN
        DELSAVEB=TRUE
        WRITE BCKB ON FILEB,SAVB
        GOSUB WRITEB
    END
    IF NOT(VERT.FLAG) THEN
        GOSUB 400 ;! Display Item A
        RETURN
    END
! Make Sure Have Not Scrolled Past Top Of Records
    IF STARTA < 1 THEN STARTA=1
    IF STARTB < 1 THEN STARTB=1
! Check Whether To Pad Line Number To 3 Or 4 Places
    ENDA=STARTA+MAX.LINES
    ENDB=STARTB+MAX.LINES
    IF ENDA > 999 THEN PADA=4 ELSE PADA=3
    IF ENDB > 999 THEN PADB=4 ELSE PADB=3
    PDAJ='R%':PADA:' '
    PDBJ='R%':PADB:' '
! Set Attribute Counter To Starting Attribute
    AMA=STARTA-1
    AMB=STARTB-1
! Set Display Columns And Rows
    COLA=START.COLA
    ROWA=START.ROWA
    COLB=START.COLB
    ROWB=START.ROWB
    ENDLINE=MAX.LINES+1
    FOR J=1 TO ENDLINE
        AMA+=1
        AMB+=1
        ROWA+=1
        ROWB+=1
        IF J<STLN ELSE
            NBRA=AMA PDAJ
            NBRB=AMB PDBJ
            GOSUB GETLINES
            IF TMPA # TMPB THEN
                PAD=REV.OFF
            END ELSE
                PAD=REV.ON
            END
            LINEA = CHANGE(TMPA, TAB, '    ')
            LINEB = CHANGE(TMPB, TAB, '    ')
            LINEA=TRIM(OCONV(LINEA,'MCP'), ' ', 'L')
            LINEB=TRIM(OCONV(LINEB,'MCP'), ' ', 'L')
            CMTA = CMTA[1, LINE.LEN - LEN(LINEA)]
            CMTB = CMTB[1, LINE.LEN - LEN(LINEB)]
            L = MAXIMUM(LEN(LINEA):@AM:LEN(LINEB))
            FOR C = 1 TO L
                IF LINEA[C,1] NE LINEB[C,1] THEN
                    C--
                    BREAK
                END
            NEXT L
            LINEA=LINEA[1,C]:PAD:LINEA[C+1,LINE.LEN-2]
            LINEB=LINEB[1,C]:PAD:LINEB[C+1,LINE.LEN-2]
            IF CMTA # CMTB THEN
                PAD=HION
            END ELSE
                PAD=HIOFF
            END
            L = MAXIMUM(LEN(CMTA):@AM:LEN(CMTB))
            FOR C = 1 TO L
                IF CMTA[C,1] NE CMTB[C,1] THEN
                    C--
                    BREAK
                END
            NEXT L
            IF LEN(CMTA) THEN LINEA:=CMTA[1,C]:PAD:CMTA[C+1,-1]
            IF LEN(CMTB) THEN LINEB:=CMTB[1,C]:PAD:CMTB[C+1,-1]
            CRT @(COLA,ROWA):CLEOL:NBRA:LINEA:RVOFF:
            CRT @(COLB,ROWB):CLEOL:NBRB:LINEB:RVOFF:
        END
    NEXT J
    STLN=1
    RETURN
990 ! Copy Different Text
! Set Attribute Counter To Starting Attribute
    AMA=STARTA -1
    AMB=STARTB -1
! Set Display Columns And Rows
    IF integrate THEN
        ENDLINE = MAXIMUM(DCOUNT(RECA,@AM):@AM:DCOUNT(RECB,@AM))
    END ELSE ENDLINE=MAX.LINES+1
    UPDA=FALSE
    UPDB=FALSE
    LASTA=''; LASTB=''
    FOR J=1 TO ENDLINE
        AMA+=1
        AMB+=1
        LINEA=RECA<AMA>
        LINEB=RECB<AMB>
        GOSUB GETINTEGRATE
        IF integrate OR LINEA#'' OR LINEB#'' THEN
            TMPA=LINEA
            TMPB=LINEB
            IF (T.OPTION) THEN
                CONVERT TAB TO SPC IN TMPA
                CONVERT TAB TO SPC IN TMPB
                TMPA=TRIM(TMPA)
                TMPB=TRIM(TMPB)
            END
            IF TMPA # TMPB THEN
                IF CMD[2,1] EQ 'B' THEN
                    UPDA=TRUE
                    LINEA=LINEB
                END ELSE
                    UPDB=TRUE
                    LINEB=LINEA
                END
                RECA<AMA>=LINEA
                RECB<AMB>=LINEB
                IF VERT.FLAG THEN
                    CRT @(START.COLA,START.ROWA+J):CLEOL:AMA PDAJ:LINEA[1,LINE.LEN-6]:
                    CRT @(START.COLB,START.ROWA+J):AMB PDBJ:LINEB[1,LINE.LEN-6]:
                END ELSE
                    CRT @(START.COLA,START.ROWA+J):CLEOL:AMA PDAJ:LINEA[1,LINE.LEN-2]:
                    CRT @(START.COLB,START.ROWB+J):CLEOL:AMB PDBJ:LINEB[1,LINE.LEN-2]:
                END
            END
            IF LASTA NE LASTB THEN
                IF CMD[2,1] EQ 'B' THEN
                    LINEA = LASTB
                    LASTB = LASTA
                    LASTA = LINEA
                END
                LOOP WHILE LASTB GT LASTA DO
                    DEL RECA<AMA>
                    DEL RECB<AMA>
                    AMA--
                    LASTB--
                REPEAT
                GOSUB 600
                GOSUB 900
                BREAK
            END
        END
    NEXT J
    RETURN
!
995 ! Merge Different Text
! Set Attribute Counter To Starting Attribute
    AMA=STARTA
    AMB=STARTB
! Set Display Columns And Rows
    ENDLINE=MAX.LINES+1
    MERGE.LINE=''
    MERGE.CODE=''
    FOR J=1 TO ENDLINE
        LINEA=RECA<AMA>
        LINEB=RECB<AMB>
        TMPA=LINEA
        TMPB=LINEB
        IF (T.OPTION) THEN
            CONVERT TAB TO SPC IN TMPA
            CONVERT TAB TO SPC IN TMPB
            TMPA=TRIM(TMPA)
            TMPB=TRIM(TMPB)
        END
        IF TMPA # TMPB THEN
            IF CMD[2,1] EQ 'B' THEN
                MERGE.CODE := @AM:LINEB
                IF MERGE.LINE EQ '' THEN MERGE.LINE=AMA
                AMB+=1
            END ELSE
                MERGE.CODE := @AM:LINEA
                IF MERGE.LINE EQ '' THEN MERGE.LINE=AMB
                AMA+=1
            END
        END ELSE
            IF MERGE.LINE#'' THEN
                J=ENDLINE
            END ELSE
                AMA+=1
                AMB+=1
            END
        END
    NEXT J
    IF MERGE.CODE#'' THEN
        MERGE.CODE = MERGE.CODE[2,-1]
        IF CMD[2,1] EQ 'B' THEN
            MERGE.CODE<-1>=RECA<AMA>
            RECA<MERGE.LINE>= MERGE.CODE
            AMA=MERGE.LINE
            STLN=AMA-STARTA+1
        END ELSE
            MERGE.CODE<-1>=RECB<AMB>
            RECB<MERGE.LINE>=MERGE.CODE
            AMB=MERGE.LINE
            STLN=AMB-STARTB+1
        END
        GOSUB 900
    END
    RETURN
!
1000 ! Find Next Differnce
! Make Sure Have Not Scrolled Past Top Of Records
    IF STARTA < 1 THEN STARTA=1
    IF STARTB < 1 THEN STARTB=1
    AMA=STARTA+MAX.LINES+1
    AMB=STARTB+MAX.LINES+1
    IF integrate THEN
        occ = 1
        LOOP
            pos = INDEX(RECA, @AM:INTEG, occ)
            IF pos THEN
                amc = DCOUNT(RECA[1, pos], @AM)
            END ELSE amc = @FALSE
        UNTIL pos EQ @FALSE OR amc GE AMA DO occ++ REPEAT
        IF amc THEN
            amc += 5
            AMA = amc
            AMB = amc
            CHKA = 'A'
            CHKB = 'B'
        END ELSE RETURN
    END ELSE
        CRT CL:'Now searching next difference ':
        INCLUDE EB.OS.INCLUDES FIND.NEXT.DIFF
    END
1099 !
    IF CHKA#CHKB THEN
        STARTA=AMA - 5
        STARTB=AMB - 5
    END
    RETURN
!
1100 ! Adjust Item A
! Make Sure Have Not Scrolled Past Top Of Records
    IF STARTA < 1 THEN STARTA=1
    IF STARTB < 1 THEN STARTB=1
    AMA=STARTA - 1
1105 ! Get Item A
    AMA+=1
    AMB=STARTB
    CRT CL:'Now adjusting items (':AMA:'-':AMB:') ':
    LASTA=DCOUNT(RECA,AM)
    IF AMA>LASTA THEN
        CRT CL:'CANNOT ADJUST ITEMS, PRESS [RETURN] TO CONTINUE ':
        INPUT RET:
        GO 1199
    END
    LINEA=TRIM(RECA<AMA>)
    EMB=AMB+OFFSET
    LASTB=DCOUNT(RECB,AM)
    FOR J=AMB TO EMB
        CRT @(19,NORMAL.DEPTH):'(':AMA:'-':J:')':CLEOL:
        IF J>LASTB THEN GO 1105
        LINEB=TRIM(RECB<J>)
        TMPA=LINEA
        TMPB=LINEB
        IF (T.OPTION) THEN
            CONVERT TAB TO SPC IN TMPA
            CONVERT TAB TO SPC IN TMPB
            TMPA=TRIM(TMPA)
            TMPB=TRIM(TMPB)
        END
        IF TMPA=TMPB THEN GO 1199
    NEXT J
    GO 1105
1199 !
    STARTA=AMA
    STARTB=J
    RETURN
1200 ! Adjust Item B
! Make Sure Have Not Scrolled Past Top Of Records
    IF STARTA < 1 THEN STARTA=1
    IF STARTB < 1 THEN STARTB=1
    AMB=STARTB - 1
1205 ! Get Item B
    AMB+=1
    AMA=STARTA
    CRT CL:'Now adjusting items (':AMB:'-':AMA:') ':
    LASTA=DCOUNT(RECB,AM)
    IF AMA>LASTB THEN
        CRT CL:'CANNOT ADJUST ITEMS, PRESS [RETURN] TO CONTINUE ':
        INPUT RET:
        GO 1299
    END
    LINEB=TRIM(RECB<AMB>)
    EMA=AMA+OFFSET
    LASTA=DCOUNT(RECA,AM)
    FOR J=AMA TO EMA
        CRT @(19,NORMAL.DEPTH):'(':AMB:'-':J:')':CLEOL:
        IF J>LASTA THEN GO 1205
        LINEA=TRIM(RECA<J>)
        TMPA=LINEA
        TMPB=LINEB
        IF (T.OPTION) THEN
            CONVERT TAB TO SPC IN TMPA
            CONVERT TAB TO SPC IN TMPB
            TMPA=TRIM(TMPA)
            TMPB=TRIM(TMPB)
        END
        IF TMPB=TMPA THEN GO 1299
    NEXT J
    GO 1205
1299 !
    STARTB=AMB
    STARTA=J
    RETURN
1300 ! Set Screen Type
    IF DEEP.FLAG THEN
        CRT DEEP.MODE:
!        EXECUTE "TERM ,45"
        CMD.ROW=WIDE.DEPTH
    END ELSE
        CRT SHALLOW.MODE:
!        EXECUTE "TERM ,":NORMAL.DEPTH
        CMD.ROW=NORMAL.DEPTH
    END
    IF WIDE.FLAG THEN
        CRT WIDE.MODE:
        EXECUTE 'TERM ':WIDE.LEN
    END ELSE
        CRT NORM.MODE:
        EXECUTE 'TERM ':NORMAL.WIDTH
    END
    RETURN
UPDATE: !
    GOSUB DELETEA
    GOSUB DELETEB
    CRT FG:RVOFF
    IF (ORIGB#RECB OR ORIGA#RECA) THEN
        IF CMD[1,2] EQ 'EX' THEN
            LOOP
                CRT BELL:@(0,CMD.ROW):CLEOL:'Changes have been made...continue (Y/N) ? ':
                INPUT ANS,1:
            UNTIL ANS EQ 'Y' OR ANS EQ 'N' DO REPEAT
        END ELSE ANS='Y'
!
        IF ANS#'Y' THEN
            CMD=''
        END ELSE
            IF CMD[1,2] EQ 'FI' OR CMD[1,2] EQ 'FS' THEN
                IF ORIGA#RECA AND CMD[3,1] NE 'B' THEN
                    WRITE RECA ON FILEA,IDA
                    IF CMD[2,1] EQ 'I' THEN
                        IF 0*NOT(INDEX(FA,'PATCH',1)) THEN
                            LOOP
                                CRT BELL:@(0,CMD.ROW):CLEOL:'Make patch for ':FA:' ':IDA:' (Y/N) ? ':
                                INPUT ANS,1:
                            UNTIL ANS EQ 'Y' OR ANS EQ 'N' DO REPEAT
                            IF ANS EQ 'Y' THEN
                                EXECUTE 'EB ':FA:' ':IDA
                            END ELSE
                                LOCATE IDA IN CHANGEDA<am_start> BY 'AL' SETTING POS ELSE
                                    INS IDA BEFORE CHANGEDA<POS>
                                END
                            END
                        END
                    END
                END
                IF ORIGB#RECB AND CMD[3,1] NE 'A' THEN
                    WRITE RECB ON FILEB,IDB
                    IF CMD[2,1] EQ 'I' THEN
                        IF 0*NOT(INDEX(FVB,'PATCH',1)) THEN
                            LOOP
                                CRT BELL:@(0,CMD.ROW):CLEOL:'Make patch for ':FVB:' ':IDB:' (Y/N) ? ':
                                INPUT ANS,1:
                            UNTIL ANS EQ 'Y' OR ANS EQ 'N' DO REPEAT
                            IF ANS EQ 'Y' THEN
                                EXECUTE 'EB ':FVB:' ':IDB
                            END ELSE
                                LOCATE IDB IN CHANGEDB<am_start> BY 'AL' SETTING POS ELSE
                                    INS IDB BEFORE CHANGEDB<POS>
                                END
                            END
                        END
                    END
                END
                IF CMD[2,1] EQ 'S' THEN CMD = '' ;! don't exit
            END
        END
    END
    RETURN
!!
99999 !!!
!    EXECUTE "TERM ":ORIG.TERM
    EXECUTE 'TERM ':ORIG.WIDTH:',':ORIG.DEPTH
    CRT NORM.MODE:
    INCLUDE EB.OS.INCLUDES CLEARSELECT
!!
FINISH: !
    GOSUB UPDATE.CHANGE
    IF DELSAVEA THEN
        DELETE FILEA,SAVA
        DELSAVEA=FALSE
    END
    IF DELSAVEB THEN
        DELETE FILEB,SAVB
        DELSAVEB=FALSE
    END
    STOP
WRITEA: !
    WRITE RECA ON FILEA,NDA
    DELAREQ=TRUE
    RETURN
WRITEB: !
    WRITE RECB ON FILEB,NDB
    DELBREQ=TRUE
    RETURN
DELETEA: !
    IF DELAREQ THEN
        DELETE FILEA,NDA
        DELAREQ=FALSE
        RETURN
    END
DELETEB: !
    IF DELBREQ THEN
        DELETE FILEB,NDB
        DELBREQ=FALSE
        RETURN
    END
    RETURN
OPEN.FILEB: !
120 ! Enter Second File Name
    IF BSENT OR UPGBACKUP THEN BSENT=FALSE ELSE
        LOOP
            CRT @(42,2):'Enter file B: ':@(-4):
            INPUT FVB
        WHILE FVB EQ '?' DO
            FH = 'B'
            GOSUB SHOW_FILE_HELP
        REPEAT
        CRT @(-3):
    END
    IF FVB EQ '' THEN FVB=FA
    IF FVB EQ '^' THEN RETURN TO 100
    IF FVB EQ 'EX' THEN RETURN TO 99999
    IF SEL AND FVB EQ FA THEN
        CRT 'Select Active, file B must be different'
        GOTO 120
    END
    IF FIELD(FVB,' ',1) EQ 'DICT' THEN
        DICT='DICT'
        FVB=FIELD(FVB,' ',2)
    END ELSE
        DICT=''
    END
    CRT @(42,2):'Enter file B: ':FVB:
    OPEN DICT,FVB TO FILEB ELSE
        CRT EL:'CANNOT OPEN ':FVB:
        GO 120
    END
    CRT EL:
    IF CHANGEDA#'' THEN GOSUB UPDATE.CHANGE
    READ CHANGEDA FROM F.PF,FA ELSE CHANGEDA=''
    READ CHANGEDB FROM F.PF,FVB ELSE CHANGEDB=''
    ORIG.CHANGEDA=CHANGEDA
    ORIG.CHANGEDB=CHANGEDB
    PATCHFILE=INDEX(FVB:FA,'PATCH',1)
    RETURN
UPDATE.CHANGE: !
    IF NOT(AOBJ) AND CHANGEDA#ORIG.CHANGEDA THEN WRITE CHANGEDA ON F.PF,FA; CRT FA:' written to POINTER-FILE'
    IF NOT(BOBJ) AND CHANGEDB#ORIG.CHANGEDB THEN WRITE CHANGEDB ON F.PF,FVB; CRT FVB:' written to POINTER-FILE'
    RETURN
GETLINES: !
    LINEA=RECA<AMA>
    LINEB=RECB<AMB>
    GOSUB GETINTEGRATE
    LINEA=TRIM(LINEA,VM,'T')
    LINEA=TRIM(LINEA,SVM,'T')
    LINEB=TRIM(LINEB,VM,'T')
    LINEB=TRIM(LINEB,SVM,'T')
    TMPA=LINEA
    TMPB=LINEB
    CTMP = TMPA; GOSUB SPLITCOMMENT; TMPA = CTMP<1>; CMTA = CTMP<2>
    CTMP = TMPB; GOSUB SPLITCOMMENT; TMPB = CTMP<1>; CMTB = CTMP<2>
    IF (T.OPTION) THEN
        CONVERT TAB TO SPC IN TMPA
        CONVERT TAB TO SPC IN TMPB
        TMPA=TRIM(TMPA)
        TMPB=TRIM(TMPB)
    END
    IF PATCHFILE THEN
        IF INDEX('!*',LINEA[1,1],1) THEN TMPA=TRIM(TMPA[2,999])
        IF INDEX('!*',LINEB[1,1],1) THEN TMPB=TRIM(TMPB[2,999])
        TMPA = SWAP(TMPA, ';*', ';!')
        TMPB = SWAP(TMPB, ';*', ';!')
        TMPA = SWAP(TMPA, ' ;!', ';!')
        TMPB = SWAP(TMPB, ' ;!', ';!')
    END
    RETURN
GETINTEGRATE: !
    IF integrate AND INDEX(LINEA, INTEG, 1) THEN
        LASTA = FIELD(LINEA, INTEG, 4)
        LASTB = FIELD(LINEA, INTEG, 5)
        LINEA = FIELD(LINEA, INTEG, 2)
        LINEB = FIELD(LINEB, INTEG, 3)
    END
    RETURN
SPLITCOMMENT: !
    POSC = INDEX(CTMP, '//', 1)
    IF NOT(POSC) THEN
        POSC = INDEX(CTMP, '/*', 1)
    END
    IF POSC THEN
        CTMP = CTMP[1, POSC-1]:@AM:CTMP[POSC,-1]
    END
    RETURN
CHECKRANGE: !
!
! Don't allow range commands to include lines we can't see
!
    RANGE_OK = FALSE
    IF SIDE EQ 'A' THEN
        FRST = STARTA
        TOST = STARTB
    END ELSE
        FRST = STARTB
        TOST = STARTA
    END
    FRFI = FRST+MAX.LINES
    TOFI = TOST+MAX.LINES
    BEGIN CASE
        CASE FR.ST LT FRST
        CASE FR.ST GT FRFI
        CASE SIDES EQ 1
            RANGE_OK = TRUE
        CASE TO.ST LT TOST
        CASE TO.ST GT TOFI
        CASE 1
            RANGE_OK = TRUE
    END CASE
    IF NOT(RANGE_OK) THEN
        CRT @(0,CMD.ROW):CLEOL:'Range (':FR.ST:'-':FR.FI:
        IF SIDES EQ 2 THEN
            CRT ' / ':TO.ST:'-':TO.FI:
        END
        CRT ") not in current view. Enter 'Y' to override ":BELL:
        INPUT RANGE_OK,1:_
        RANGE_OK = UPCASE(RANGE_OK EQ 'Y')
    END
    RETURN
SHOW_FILE_HELP:
    CRT
    CRT 'Enter EX to exit, [DICT ] file name (or full path)':(IF FH EQ 'A' THEN "" ELSE " or ^ to go back"):
    RETURN
SHOW_ID_HELP:
    CRT
    CRT 'Enter an item name from ':(IF IDH EQ 'A' THEN FILEA ELSE FILEB):', "^" to go back or "EX{K}" to exit':
    RETURN
