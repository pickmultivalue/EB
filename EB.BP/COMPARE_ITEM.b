* @(#) COMPARE_ITEM.b Ported to jBASE 07:23:52  18 FEB 2010
!
! Compiled On 19 Jun 1989 At 13:25
! Copyright C 1989 By Amalgamated Wireless (Australasia) Limited Ne Sigma Data.
! All Rights Reserved.
!
!
    INCLUDE JBC.h
    DEFC INT JBASEEmulateGETINT(INT, INT)
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    vm_start=IF_COMPILED_PRIME
    EQU Bslsh TO '\', Fslsh TO '/'
    EQU MAX TO 9999
    INCLUDE EB.OS.INCLUDES TERM.SETTINGS
    OPEN 'MD' TO F.MD ELSE STOP 'MD'
    OPEN 'SAVEDLISTS' TO F.PF ELSE
        OPEN 'POINTER-FILE' TO F.PF ELSE
            STOP 201,'POINTER-FILE'
        END
    END
    TCL.OPTS=OCONV(FIELD(SENTENCE():'(','(',2),'MCU')
    FG$SENTENCE=SENTENCE()[1,COL1()-1]
    PATCH.MODE=INDEX(TCL.OPTS,'P',1)
    BCKUP.MODE=INDEX(TCL.OPTS,'B',1)
    T.OPTION=INDEX(TCL.OPTS,'T',1)
    EQU SVM TO CHAR(252)
    EQU VM TO CHAR(253)
    EQU AM TO CHAR(254)
    EQU BELL TO CHAR(7)
    EQU OTHERWISE TO 1
    EQU TRUE TO 1, FALSE TO 0
    EQU SPC TO ' ', TAB TO CHAR(9)
    OPEN 'UPG.WORKFILE' TO F.UPG.WORKFILE THEN
        UPG=TRUE
        USEMODE=''; PASSWD=''
    END ELSE UPG=FALSE
    ORIG.DEPTH=PDEPTH
    ORIG.WIDTH=PWIDTH
    DIM SCREEN.PARAMS(100)
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.OS.INCLUDES WHO
    CALL EB_GETTCC(FG$TLINE, MAT SCREEN.PARAMS, TERM)
    EQU ESC TO CHAR(27)
    TOF=CHAR(12)
    STLN=1
    CNTA=0
    CNTB=0
    ASENT=FALSE; BSENT=FALSE
    AISENT=FALSE; BISENT=FALSE
    DELAREQ=FALSE; DELBREQ=FALSE
    VERT.FLAG=FALSE
    WIDE.FLAG=TRUE
!# Start Of Bug Fix By Dwa On 19/05/88.
    DEEP.FLAG=TRUE
!#  end  of bug fix by dwa on 19/05/88.
    WRAP.FLAG=FALSE
    READN=FALSE
    DEFAULT.OFFSET=99
    NORMAL.LEN=79
    WIDE.LEN=132
!    WIDE.LEN=PWIDTH
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
        CASE TERM='B'
!      WIDE.MODE=ESC:'[=;132Z'
            DEEP.MODE=ESC:'[=33;Z'
!      NORM.MODE=ESC:'[=;80Z'
            SHALLOW.MODE=ESC:'[=25;Z'
        CASE TERM[1,1]='W'
            DEEP.MODE=ESC:"e":"*"
!      WIDE.MODE=ESC:'`;'
!      NORM.MODE=ESC:'`:'
            SHALLOW.MODE=ESC:'e&'
        CASE TERM='Q' OR TERM='U'     ;! these were copied from W and are probably wrong
            DEEP.MODE=ESC:"e":"*"
!      WIDE.MODE=ESC:'`;'
!      NORM.MODE=ESC:'`:'
            SHALLOW.MODE=ESC:'e&'
        CASE 1
            DEEP.MODE=''
            SHALLOW.MODE=''
    END CASE
!  CLEOL=@(-4)
    EL=@(0,PDEPTH-1):CLEOL
    HIOFF=BG:RVON
    HION= FG:RVOFF
    REV.OFF=RVON
    REV.ON=RVOFF
    CL=@(0,NORMAL.DEPTH):CLEOL:HION:REV.ON
    CRT @(-1)
    CRT @(0,0):'COMPARE.ITEMS':TIMEDATE() 'R#63':
    PROMPT ''
    FA=''; IDA=''
    FB=''; IDB=''
    CHANGEDA=''; CHANGEDB=''
    ORIG.CHANGEDA=''; ORIG.CHANGEDB=''
    LAST.EXEC=''
    SEL=SYSTEM(11)
    IF NOT(SEL) THEN
        ITNM=FIELD(FG$SENTENCE,' ',2)
        IF ITNM#'' THEN
            AISENT=TRUE
            IF INDEX(ITNM, Bslsh, 1) THEN
                gslsh=Bslsh
                bslsh=Fslsh
            END ELSE
                gslsh=Fslsh
                bslsh=Bslsh
            END
            CONVERT bslsh TO gslsh IN ITNM
            IF INDEX(ITNM,gslsh,1) THEN
                FLNM=ITNM
                ITNM=FIELD(FLNM,gslsh,DCOUNT(FLNM,gslsh))
                FLNM=FLNM[1,COL1()-1]
            END ELSE
                INCLUDE EB.OS.INCLUDES GET.FLNM
            END
            IDA=ITNM
            IF FLNM#'' THEN FA=FLNM; ASENT=TRUE
            ITNM=FIELD(FG$SENTENCE,' ',3)
            IF ITNM#'' THEN
                IF INDEX(ITNM, Bslsh, 1) THEN
                    gslsh=Bslsh
                    bslsh=Fslsh
                END ELSE
                    gslsh=Fslsh
                    bslsh=Bslsh
                END
                BISENT=TRUE
                CONVERT bslsh TO gslsh IN ITNM
                IF INDEX(ITNM,gslsh,1) THEN
                    FLNM=ITNM
                    ITNM=FIELD(FLNM,gslsh,DCOUNT(FLNM,gslsh))
                    FLNM=FLNM[1,COL1()-1]
                END ELSE
                    INCLUDE EB.OS.INCLUDES GET.FLNM
                END
                IDB=ITNM
                IF FLNM#'' THEN FB=FLNM; BSENT=TRUE
            END
        END
    END ELSE
        FA=FIELD(FG$SENTENCE,' ',2)
        IF FA='DICT' THEN FA='DICT ':FIELD(FG$SENTENCE,' ',3)
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
    IF ASENT THEN ASENT=FALSE ELSE
        CRT @(0,2):'ENTER FILE A: ':
        INPUT FA
    END
    IF FA='EX' THEN GO 99999
    IF FIELD(FA,' ',1)='DICT' THEN
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
        IF FIELD(FA,' ',1)='DICT' THEN POS=4 ELSE POS=3
        FB=FIELD(FG$SENTENCE,' ',POS)
        IF FB='DICT' THEN FB='DICT ':FIELD(FG$SENTENCE,' ',POS+1)
        BSENT=(FB#'' OR UPGBACKUP)
    END
    IF NOT(UPGBACKUP) THEN GOSUB OPEN.FILEB
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
            CRT @(0,4):'ENTER ID A: ':
            INPUT IDA
        END
    END
    IF IDA='EXK'[1,LEN(IDA)] THEN GO 99999
    IF IDA='^' THEN GO 120
    READV RECA FROM FILEA,IDA,1 ELSE
        CRT EL:IDA:' NOT IN ':FA:
        GO 110
    END
    CRT EL:
!
130 ! Enter Second Id
    IF SEL THEN
        IF UPGBACKUP THEN
            FB=FIELD(IDA,'\',2)
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
            CRT @(42,4):'ENTER ID B: ':
            INPUT IDB
        END
    END
    IF IDB='' THEN IDB=IDA
    IF IDB='^' THEN GO 110
    IF IDB='EX' THEN GO 99999
    READV RECB FROM FILEB,IDB,1 ELSE
        CRT EL:IDB:' NOT IN ':FB:
        GO 131
    END
    IF READN THEN GOTO 200
    CRT EL:
    CRT @(25,6):'--- OPTIONS ---':
150 ! Enter Display Mode
    CRT @(10,8):'HORIZONTAL OR VERTICAL DISPLAY (H/V): ':
    INPUT OPT
    IF OPT='EX' THEN GO 99999
    IF OPT='^' THEN GO 130
    IF OPT='H' THEN VERT.FLAG=FALSE ELSE VERT.FLAG=TRUE
160 ! Enter Display Type
    CRT @(10,10):'WIDE OR NORMAL SCREEN (W/N): ':
    INPUT OPT
    IF OPT='EX' THEN GO 99999
    IF OPT='^' THEN GO 150
    IF INDEX('W',OPT,1) THEN WIDE.FLAG=TRUE ELSE WIDE.FLAG=FALSE
170 ! Enter Display Level
    CRT @(10,12):'DEEP OR NORMAL SCREEN DEPTH (D/N): ':
    INPUT OPT
    IF OPT='EX' THEN GO 99999
    IF OPT='^' THEN GO 160
    IF INDEX('D',OPT,1) THEN DEEP.FLAG=TRUE ELSE DEEP.FLAG=FALSE
!!!!!!!!!!!!!!!!!!!!!!!!
200 ! Mainline
    IF SEL THEN READN=TRUE
!
! Save Items In-Case You Do Somethine Stupid With Copy/Merge Commands
!
    READ RECA FROM FILEA,IDA THEN
        IF INDEX(IDA,'@',1) THEN
            TMP=''
            FOR I=1 TO TMP
                TMP<I>=RECA<1>; DEL RECA<1>
            NEXT I
        END ELSE TMP=''
        GOSUB DECRYPTA
        IF TMP#'' THEN
            INS TMP BEFORE RECA<1>
            DATA 'A10'
        END
        ORIGA=RECA
    END ELSE RECA=''; ORIGA=''
    READ RECB FROM FILEB,IDB THEN
        GOSUB DECRYPTB
        ORIGB=RECB
    END ELSE RECB=''; ORIGB=''
    NDA='%':IDA:'%'
    NDB='%':IDB:'%'
!
    IF UPGBACKUP THEN
        TMP=FIELD(IDA,'\',1)
        TMP=FIELD(TMP,'*',1) 'D2/':'@':FIELD(TMP,'*',2) 'MTS'
    END ELSE TMP=IDA
    DIS.IDA=FA:' - ':TMP
    DIS.IDB=FB:' - ':IDB
    PREV.LOC=''
    STARTA=1
    STARTB=1
    GOSUB 1300          ;! set screen type
    GOSUB 600 ;! Format Display Mode
    GOSUB 900 ;! display both items
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
    cmd=OCONV(FIELD(CMD,' ',1),'MCU')
    CMD=cmd:CMD[COL2(),999]
    CRT RVOFF:
    BEGIN CASE
        CASE CMD='W'        ;! wide screen
            WIDE.FLAG=TRUE
            GOSUB 1300
            GOSUB 600 ;! Format Display Mode
            GOSUB 900 ;! display both items
        CASE CMD='N'        ;! Normal Screen
            WIDE.FLAG=FALSE
            DEEP.FLAG=FALSE
            GOSUB 1300
            GOSUB 600 ;! format display mode
            GOSUB 900 ;! Display Both Items
        CASE CMD='D'        ;! deep screen
            DEEP.FLAG=TRUE
            GOSUB 1300
            GOSUB 600 ;! Format Display Mode
            GOSUB 900 ;! display both items
        CASE CMD[1,1]='G'   ;! Go To Line Number
            NBR=FIELD(CMD,' ',2)
            IF NBR # '' AND NUM(NBR) THEN
                STARTA=NBR
                STARTB=NBR
                GOSUB 900 ;! display items
            END
        CASE CMD[1,2]='RA' OR CMD[1,2]='R ' OR CMD='R'    ;! Readjust Item A
            OFFSET=FIELD(CMD,' ',2)
            IF NUM(OFFSET) THEN
                IF OFFSET='' THEN OFFSET=DEFAULT.OFFSET
                DEFAULT.OFFSET=OFFSET
                GOSUB 1100          ;! Readjust Item A
                GOSUB 900 ;! Display Both Items
            END
        CASE CMD[1,2]='RB'  ;! Readjust B
            OFFSET=FIELD(CMD,' ',2)
            IF NUM(OFFSET) THEN
                IF OFFSET='' THEN OFFSET=DEFAULT.OFFSET
                DEFAULT.OFFSET=OFFSET
                GOSUB 1200          ;! Readjust B
                GOSUB 900 ;! Display Both Items
            END
        CASE CMD='EX' OR CMD='FI' OR CMD='FS'
FILE.ITEM:!
            GOSUB UPDATE
            IF CMD#'' THEN
                IF SEL THEN GO 110
                WIDE.FLAG=FALSE
                DEEP.FLAG=FALSE
                GOSUB 1300
                CRT @(-1)
                CRT @(0,0):'COMPARE.ITEMS':TIMEDATE() 'R#63':
                CRT @(0,2):'ENTER FILE A: ':FA
                CRT @(42,2):'ENTER FILE B: ':FB:
                GO 110
            END
        CASE CMD='EXK' OR CMD='FIK'
            WIDE.FLAG=FALSE
            DEEP.FLAG=FALSE
            GOSUB UPDATE
            IF CMD#'' THEN
                GOSUB 1300
                GOTO 99999
            END
        CASE CMD[1,1]='F'   ;! Find Next Difference
            GOSUB 1000          ;! Find Next Difference
            IF CHKA#CHKB THEN
                GOSUB 900 ;! Display Both Items
            END ELSE
                CRT @(0,CMD.ROW):CLEOL:'No more differences':
            END
        CASE CMD[1,2]='CW' OR CMD[1,2]='CF'     ;! Copy Whole item
            IF CMD[3,1]='B' THEN
                RECA=RECB
            END ELSE RECB=RECA
            IF CMD[1,2]='CF' THEN
                CMD='FS'
                GO FILE.ITEM
            END
            GOSUB 600
        CASE CMD[1,1]='C'   ;! Copy Text
            IF OCONV(CMD,'MCN')='' THEN
                GOSUB 990
            END ELSE
                OK=TRUE
                IF FIELD(CMD,' ',1)='C' THEN CMD='CA':CMD[COL2(),99]
                BEGIN CASE
                    CASE CMD MATCHES "2A' '1N0N' '1N0N"
                    CASE CMD MATCHES "2A' '1N0N'-'1N0N' '1N0N"
                    CASE CMD MATCHES "2A' '1N0N' '1N0N'-'1N0N"
                    CASE CMD MATCHES "2A' '1N0N'-'1N0N' '1N0N'-'1N0N"
                    CASE 1
                        CRT @(0,NORMAL.DEPTH):'Incomplete Copy command':CLEOL:
                        OK=FALSE
                END CASE
                IF OK THEN
                    FR.RANGE=FIELD(CMD,' ',2); TO.RANGE=FIELD(CMD,' ',3)
                    FR.ST=FIELD(FR.RANGE,'-',1); FR.FI=FIELD(FR.RANGE,'-',2)
                    TO.ST=FIELD(TO.RANGE,'-',1); TO.FI=FIELD(TO.RANGE,'-',2)
                    IF FR.FI='' THEN FR.FI=FR.ST
                    IF TO.FI='' THEN TO.FI=TO.ST
                    IF CMD[2,1]='B' THEN
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
        CASE CMD[1,1]='M'   ;! Merge Text
            IF OCONV(CMD,'MCN')='' THEN
                GOSUB 995
            END ELSE
                FR.RANGE=FIELD(CMD,' ',1)
                BEGIN CASE
                    CASE FR.RANGE='MA'
                    CASE FR.RANGE='MB'
                    CASE FR.RANGE[1,2]='MA'; CMD=CMD[1,2]:' ':CMD[3,999]
                    CASE FR.RANGE[1,2]='MB'; CMD=CMD[1,2]:' ':CMD[3,999]
                    CASE 1
                        CMD='MA ':TRIM(CMD[2,999])
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
                    IF CMD[2,1]='B' THEN
                        MERGE.CODE=RECA<TO.RANGE-1>
                        FOR AMB=FR.ST TO FR.FI
                            MERGE.CODE<-1>=RECB<AMB>
                        NEXT AMB
                        RECA<TO.RANGE-1>=MERGE.CODE
                    END ELSE
                        MERGE.CODE=RECB<TO.RANGE-1>
                        FOR AMA=FR.ST TO FR.FI
                            MERGE.CODE<-1>=RECA<AMA>
                        NEXT AMA
                        RECB<TO.RANGE-1>=MERGE.CODE
                    END
                END
                GOSUB 900
            END
        CASE CMD[1,1]='S'   ;! Next Locate
            IF PREV.LOC # '' THEN
                TEXT=PREV.LOC
                GO 215
            END
        CASE CMD='EA'
            DATA "?"
            GOSUB WRITEA
            DATA 'ED ':FA
            EXECUTE 'SELECT ':FA:' "':NDA:'"'
            READ RECA FROM FILEA,NDA ELSE NULL
            GOSUB 600
            GOSUB 900
        CASE CMD='EB'
            DATA "?"
            GOSUB WRITEB
            DATA 'ED ':FB
            EXECUTE 'SELECT ':FB:' "':NDB:'"'
            READ RECB FROM FILEB,NDB ELSE NULL
            GOSUB 600
            GOSUB 900
        CASE CMD='EBA'
            GOSUB WRITEA
            DATA 'EB ':FA
            EXECUTE 'SELECT ':FA:' "':NDA:'"'
            READ RECA FROM FILEA,NDA ELSE NULL
            GOSUB 600
            GOSUB 900
        CASE CMD='EBB'
            GOSUB WRITEB
            DATA 'EB ':FB
            EXECUTE 'SELECT ':FB:' "':NDB:'"'
            READ RECB FROM FILEB,NDB ELSE NULL
            GOSUB 600
            GOSUB 900
        CASE CMD='I'
            GOSUB WRITEA
            DATA 'jEDIfmt ':FA
            EXECUTE 'SELECT ':FA:' "':NDA:'"'
            READ RECA FROM FILEA,NDA ELSE NULL
            GOSUB WRITEB
            DATA 'jEDIfmt ':FB
            EXECUTE 'SELECT ':FB:' "':NDB:'"'
            READ RECB FROM FILEB,NDB ELSE NULL
            GOSUB 600
            GOSUB 900
        CASE cmd='SVN'
            OP=FIELD(CMD,' ',2)
            NewCmd='svn ':OP
            OP=2
            LOOP
                arg=FIELD(CMD,' ',OP)
            UNTIL arg='' DO
                BEGIN CASE
                    CASE OCONV(arg,'MCU')='A'
                        arg=FA:'/':IDA
                    CASE OCONV(arg,'MCU')='B'
                        arg=FB:'/':IDB
                END CASE
                NewCmd:=SPC:arg
                OP++
            REPEAT
            EXECUTE NewCmd
        CASE CMD='JA'
            GOSUB WRITEA
            DATA 'JED ':FA
            EXECUTE 'SELECT ':FA:' "':NDA:'"'
            READ RECA FROM FILEA,NDA ELSE NULL
            GOSUB 600
            GOSUB 900
        CASE CMD='JB'
            GOSUB WRITEB
            DATA 'JED ':FB
            EXECUTE 'SELECT ':FB:' "':NDB:'"'
            READ RECB FROM FILEB,NDB ELSE NULL
            GOSUB 600
            GOSUB 900
        CASE CMD[1,3]='LOC'
            POS=INDEX(CMD,' ',1)
            TEXT=CMD[POS+1,999]
215         CRT CL:'Searching for "':TEXT:'", please wait ':CLEOL:
! Find String In Item A
            J=STARTA
            LASTA=DCOUNT(RECA,AM)
            LASTB=DCOUNT(RECB,AM)
220         !
            J+=1
            IF J>LASTA THEN
                CRT EL:'"':TEXT:'" NOT FOUND ':
                GO 210
            END
            LINE=RECA<J>
            IF NOT(INDEX(LINE,TEXT,1)) THEN GO 220
! Find String In Item B
            I=STARTB
221         !
            I+=1
            IF I>LASTB THEN
                CRT CL:'"':TEXT:'" NOT FOUND ':
                INPUT RET,1:
                GO 210
            END
            LINE=RECB<I>
            IF NOT(INDEX(LINE,TEXT,1)) THEN GO 221
            STARTA=J
            STARTB=I
            GOSUB 900 ;! Display Both Items
        CASE CMD[1,2]='A=' OR CMD[1,3]='A ='
            CMD=TRIM(FIELD(CMD,'=',2))
            IF CMD#'' AND NUM(CMD) THEN
                STARTA=CMD
                GOSUB 900 ;! Display Item A
            END
        CASE CMD[1,2]='B=' OR CMD[1,3]='B ='
            CMD=TRIM(FIELD(CMD,'=',2))
            IF CMD#'' AND NUM(CMD) THEN
                STARTB=CMD
                GOSUB 900 ;! Display Item B
            END
        CASE CMD[1,1]='A'
            CMD=TRIM(CMD[2,99])
            IF CMD#'' AND NUM(CMD) THEN
                STARTA+=CMD
            END ELSE
                STARTA+=MAX.LINES
            END
            GOSUB 900 ;! Display Item A
        CASE CMD[1,1]='B'
            CMD=TRIM(CMD[2,99])
            IF CMD#'' AND NUM(CMD) THEN
                STARTB+=CMD
            END ELSE
                STARTB+=MAX.LINES
            END
            GOSUB 900 ;! Display Item B
        CASE CMD='?'
            GOSUB 700 ;! Display Help Screen
        CASE CMD[1,2]='PR'
            GOSUB 800 ;! Print Items
        CASE CMD[1,1]='V'
            VERT.FLAG=TRUE
            GOSUB 600 ;! Format Display Mode
            GOSUB 900 ;! Display Both Items
        CASE CMD[1,1]='H'
            VERT.FLAG=FALSE
            GOSUB 600 ;! Format Display Mode
            GOSUB 900 ;! Display Both Items
        CASE CMD[1,3]='XEQ'
            CMD=TRIM(CMD[4,999])
            IF CMD='' THEN CMD=LAST.EXEC
            IF CMD # '' THEN
                EXECUTE CMD
                LAST.EXEC=CMD
                CRT 'Press [RETURN] to continue ':
                INPUT RET,1:
                GOSUB 600 ;! Format Display Mode
                GOSUB 900 ;! Display Both Items
            END
        CASE CMD='TOP'
            STARTA=1
            STARTB=1
            GOSUB 900 ;! Display Both Items
        CASE NUM(CMD) AND CMD # ''
            STARTA+=CMD
            STARTB+=CMD
            GOSUB 900 ;! Display Both Items
        CASE CMD[1,1]='Z'   ;! zoom into multi-valued attribute
            IF UPG THEN
                AMA=OCONV(CMD,'MCN')
                AMB=STARTB+(AMA-STARTA)
                LINEA = RECA<AMA>
                CONVERT VM:SVM TO AM:VM IN LINEA
                UPGA='%COMPA%':IDA:'%':AMA:'%':FG$TLINE
                UPGB='%COMPB%':IDB:'%':AMB:'%':FG$TLINE
                WRITE LINEA ON F.UPG.WORKFILE,UPGA
                LINEB = RECB<AMB>
                CONVERT VM:SVM TO AM:VM IN LINEB
                WRITE LINEB ON F.UPG.WORKFILE,UPGB
                DATA 'UPG.WORKFILE',''
                DATA UPGA, UPGB
                DATA '','',''
                EXECUTE 'COMPARE.ITEM'
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
        LINEA=TRIM(RECA<AMA>)
        LINEB=TRIM(RECB<AMB>)
        TMPA=LINEA
        TMPB=LINEB
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
        IF TMPA # TMPB THEN PAD=HION ELSE PAD=HIOFF
        LINEA=OCONV(LINEA,'MCP')
        LINEB=OCONV(LINEB,'MCP')
        IF LINEA='' THEN
            LINEA=BLANK.LINE
        END ELSE
            LINEA=NBRA:LINEA:BLANK.LINE
        END
        IF LINEB='' THEN
            LINEB=BLANK.LINE
        END ELSE
            LINEB=NBRB:LINEB:BLANK.LINE
        END
        LINEA=PAD:TRIM(LINEA, ' ', 'L')[1,LINE.LEN-2]:HIOFF
        LINEB=PAD:TRIM(LINEB, ' ', 'L')[1,LINE.LEN-2]:HIOFF
        CRT @(COL,START.ROWA+J):LINEA:
        CRT @(COL,START.ROWB+J):LINEB:
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
            LINE.LEN = PWIDTH
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
    CRT @(10,12):'"TOP" of items':
    CRT @(10,13):'"EA" edit item A; "EB" edit item B':
    CRT @(10,14):'"PR"int comparsion':
    CRT @(10,15):'"XEQ"ECUTE A TCL STATEMENT':
    CRT @(10,16):'"C{[<A>,B]}"opy different lines':
    CRT @(10,17):'"CW{[<A>,B]}"opy record':
    CRT @(10,18):'"M{[<A>,B]}"erge different lines':
    CRT @(10,19):'"C{{[<A>,B]} a-b n-m}" replace lines n-m with a-b':
    CRT @(10,20):'"M{{[<A>,B]} a-b n}" merge lines a-b before n':
    CRT @(10,21):'"EX{K}" exit without save; "FI{K}" file changes':
    CRT @(10,22):'"F"ind next difference; "R{[<A>,B}}" re-adjust items':
    CRT @(0,NORMAL.DEPTH):'Press [RETURN] to continue ':CLEOL:
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
    HB=FB:' - ':IDB
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
    IF LINEA='' AND LINEB='' THEN GO 820
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
! Check Display Mode
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
            LINEA=RECA<AMA>
            LINEB=RECB<AMB>
            LINEA=TRIM(LINEA,VM,'T')
            LINEA=TRIM(LINEA,SVM,'T')
            LINEB=TRIM(LINEB,VM,'T')
            LINEB=TRIM(LINEB,SVM,'T')
            TMPA=LINEA
            TMPB=LINEB
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
            IF TMPA # TMPB THEN PAD=HION ELSE PAD=HIOFF
            LINEA=TRIM(OCONV(LINEA,'MCP'), ' ', 'L')
            LINEB=TRIM(OCONV(LINEB,'MCP'), ' ', 'L')
            IF LINEA='' THEN
                LINEA=BLANK.LINE
            END ELSE
                LINEA=NBRA:LINEA:BLANK.LINE
            END
            IF LINEB='' THEN
                LINEB=BLANK.LINE
            END ELSE
                LINEB=NBRB:LINEB:BLANK.LINE
            END
            LINEA=PAD:LINEA[1,LINE.LEN-2]:HIOFF
            LINEB=PAD:LINEB[1,LINE.LEN-2]:HIOFF
            CRT @(COLA,ROWA):LINEA:@(COLB,ROWB):LINEB:
        END
    NEXT J
    STLN=1
    RETURN
990 ! Copy Different Text
! Set Attribute Counter To Starting Attribute
    AMA=STARTA -1
    AMB=STARTB -1
! Set Display Columns And Rows
    ENDLINE=MAX.LINES+1
    UPDA=FALSE
    UPDB=FALSE
    FOR J=1 TO ENDLINE
        AMA+=1
        AMB+=1
        LINEA=RECA<AMA>
        LINEB=RECB<AMB>
        IF LINEA#'' OR LINEB#'' THEN
            TMPA=LINEA
            TMPB=LINEB
            IF (T.OPTION) THEN
                CONVERT TAB TO SPC IN TMPA
                CONVERT TAB TO SPC IN TMPB
                TMPA=TRIM(TMPA)
                TMPB=TRIM(TMPB)
            END
            IF TMPA # TMPB THEN
                IF CMD[2,1]='B' THEN
                    RECA<AMA>=LINEB
                    UPDA=TRUE
                    LINEA=LINEB
                END ELSE
                    RECB<AMB>=LINEA
                    UPDB=TRUE
                    LINEB=LINEA
                END
                IF VERT.FLAG THEN
                    CRT @(START.COLA,START.ROWA+J):CLEOL:AMA PDAJ:LINEA[1,LINE.LEN-6]:
                    CRT @(START.COLB,START.ROWA+J):AMB PDBJ:LINEB[1,LINE.LEN-6]:
                END ELSE
                    CRT @(START.COLA,START.ROWA+J):CLEOL:AMA PDAJ:LINEA[1,LINE.LEN-2]:
                    CRT @(START.COLB,START.ROWB+J):CLEOL:AMB PDBJ:LINEB[1,LINE.LEN-2]:
                END
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
            IF CMD[2,1]='B' THEN
                MERGE.CODE<-1>=LINEB
                IF MERGE.LINE='' THEN MERGE.LINE=AMA
                AMB+=1
            END ELSE
                MERGE.CODE<-1>=LINEA
                IF MERGE.LINE='' THEN MERGE.LINE=AMB
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
        IF CMD[2,1]='B' THEN
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
    CRT CL:'Now searching next difference ':
    INCLUDE EB.OS.INCLUDES FIND.NEXT.DIFF
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
        EXECUTE "TERM ,45"
        CMD.ROW=WIDE.DEPTH
    END ELSE
        CRT SHALLOW.MODE:
        EXECUTE "TERM ,":NORMAL.DEPTH
        CMD.ROW=NORMAL.DEPTH
    END
    IF WIDE.FLAG THEN
        CRT WIDE.MODE:
        EXECUTE 'TERM ':WIDE.LEN
    END ELSE
        CRT NORM.MODE:
        EXECUTE 'TERM 79'
    END
    RETURN
UPDATE: !
    GOSUB DELETEA
    GOSUB DELETEB
    CRT FG:RVOFF
    IF (ORIGB#RECB OR ORIGA#RECA) THEN
        IF CMD[1,2]='EX' THEN
            LOOP
                CRT BELL:@(0,CMD.ROW):CLEOL:'Changes have been made...continue (Y/N) ? ':
                INPUT ANS,1:
            UNTIL ANS='Y' OR ANS='N' DO REPEAT
        END ELSE ANS='Y'
!
        IF ANS#'Y' THEN
            CMD=''
        END ELSE
            IF CMD[1,2]='FI' OR CMD[1,2]='FS' THEN
                IF ORIGA#RECA THEN
                    WRITE RECA ON FILEA,IDA
                    IF CMD[2,1]='I' THEN
                        IF 0*NOT(INDEX(FA,'PATCH',1)) THEN
                            LOOP
                                CRT BELL:@(0,CMD.ROW):CLEOL:'Make patch for ':FA:' ':IDA:' (Y/N) ? ':
                                INPUT ANS,1:
                            UNTIL ANS='Y' OR ANS='N' DO REPEAT
                            IF ANS='Y' THEN
                                EXECUTE 'EB ':FA:' ':IDA
                            END ELSE
                                LOCATE IDA IN CHANGEDA<am_start> BY 'AL' SETTING POS ELSE
                                    INS IDA BEFORE CHANGEDA<POS>
                                END
                            END
                        END
                    END
                END
                IF ORIGB#RECB THEN
                    WRITE RECB ON FILEB,IDB
                    IF CMD[2,1]='I' THEN
                        IF 0*NOT(INDEX(FB,'PATCH',1)) THEN
                            LOOP
                                CRT BELL:@(0,CMD.ROW):CLEOL:'Make patch for ':FB:' ':IDB:' (Y/N) ? ':
                                INPUT ANS,1:
                            UNTIL ANS='Y' OR ANS='N' DO REPEAT
                            IF ANS='Y' THEN
                                EXECUTE 'EB ':FB:' ':IDB
                            END ELSE
                                LOCATE IDB IN CHANGEDB<am_start> BY 'AL' SETTING POS ELSE
                                    INS IDB BEFORE CHANGEDB<POS>
                                END
                            END
                        END
                    END
                END
            END
        END
    END
    RETURN
!!
99999 !!!
!    EXECUTE "TERM ":ORIG.TERM
!    EXECUTE 'TERM ':ORIG.WIDTH:',':ORIG.DEPTH
    INCLUDE EB.OS.INCLUDES CLEARSELECT
!!
FINISH: !
    GOSUB UPDATE.CHANGE
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
DECRYPTLA:!
    UPGITEM=LINEA; UPGFNAME=FA; UPGINAME=IDA
    GOSUB DECRYPT
    LINEA=UPGITEM
    RETURN
DECRYPTLB:!
    UPGITEM=LINEB; UPGFNAME=FB; UPGINAME=IDB
    GOSUB DECRYPT
    LINEB=UPGITEM
    RETURN
DECRYPTA: !
    UPGITEM=RECA; UPGFNAME=FA; UPGINAME=IDA
    GOSUB DECRYPT
    RECA=UPGITEM
    RETURN
DECRYPTB: !
    UPGITEM=RECB; UPGFNAME=FB; UPGINAME=IDB
    GOSUB DECRYPT
    RECB=UPGITEM
    RETURN
DECRYPT: !
    IF UPG THEN
        CALL UPGCHKENCRYPT(UPGITEM,ENCRYPTED,CHKSUM,SIZE,VERSION)
    END ELSE ENCRYPTED=''
    IF ENCRYPTED THEN
        IF NOT(UPG) THEN
            CRT 'Encrypted program (':IDA:')...required UPG.WORKFILE'
            STOP
        END
    END
    IF ENCRYPTED THEN
        IF USEMODE='' THEN CALL UPGMODE (USEMODE)
        IF PASSWD='' THEN CALL UPGPASSWD (PASSWD,F.UPG.WORKFILE)
        CALL UPGCONVERT(UPGFNAME,UPGINAME,UPGITEM,'DQ',USEMODE,F.UPG.WORKFILE,PASSWD,CONVOK)
        IF NOT(CONVOK) THEN STOP
    END
    RETURN
OPEN.FILEB: !
120 ! Enter Second File Name
    IF BSENT OR UPGBACKUP THEN BSENT=FALSE ELSE
        CRT @(42,2):'ENTER FILE B: ':
        INPUT FB
    END
    IF FB='' THEN FB=FA
    IF FB='^' THEN RETURN TO 100
    IF FB='EX' THEN RETURN TO 99999
    IF SEL AND FB=FA THEN
        CRT 'Select Active, FILE B must be different'
        GOTO 120
    END
    IF FIELD(FB,' ',1)='DICT' THEN
        DICT='DICT'
        FB=FIELD(FB,' ',2)
    END ELSE
        DICT=''
    END
    CRT @(42,2):'ENTER FILE B: ':FB:
    OPEN DICT,FB TO FILEB ELSE
        CRT EL:'CANNOT OPEN ':FB:
        GO 120
    END
    CRT EL:
    IF CHANGEDA#'' THEN GOSUB UPDATE.CHANGE
    READ CHANGEDA FROM F.PF,FA ELSE CHANGEDA=''
    READ CHANGEDB FROM F.PF,FB ELSE CHANGEDB=''
    ORIG.CHANGEDA=CHANGEDA
    ORIG.CHANGEDB=CHANGEDB
    PATCHFILE=INDEX(FB:FA,'PATCH',1)
    RETURN
UPDATE.CHANGE: !
    IF CHANGEDA#ORIG.CHANGEDA THEN WRITE CHANGEDA ON F.PF,FA; CRT FA:' written to POINTER-FILE'
    IF CHANGEDB#ORIG.CHANGEDB THEN WRITE CHANGEDB ON F.PF,FB; CRT FB:' written to POINTER-FILE'
    RETURN
