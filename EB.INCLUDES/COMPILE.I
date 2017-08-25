* @(#) COMPILE.I Ported to jBASE 15:13:19  17 APR 2000
*  The following variable names were converted
*   PROG,ENCRYPT,PROGRAM
* 
    EQU AM TO CHAR(254), TRUE TO 1, FALSE TO 0
    INCLUDE EB.OS.INCLUDES GET.TCL.SENTENCE
    BP.FILE=FIELD(FG$SENTENCE,' ',2)
    INCLUDE EB.OS.INCLUDES OS.ERRORS
    OPEN BP.FILE TO F.BP ELSE STOP OPER,BP.FILE
    IF INDEX(FG$SENTENCE,'(',1) THEN
        OPTIONS=' -':FIELD(FG$SENTENCE,'(',2)
        FG$SENTENCE=FG$SENTENCE[1,COL1()-1]
    END ELSE OPTIONS=''
    PROGRAMS=TRIM(OCONV(FG$SENTENCE,'G2 99'))
    IF PROGRAMS='' AND SYSTEM(11) THEN
        EOF=FALSE
        LOOP
            READNEXT Prog THEN
                PROGRAMS:=Prog:' '
            END ELSE EOF=TRUE
        UNTIL EOF DO REPEAT
    END
    INCLUDE EB.OS.INCLUDES WHO
    INCLUDE UPG.DEFS ENCRYPT.MESG
    FG$SENTENCE=TRIM(FG$SENTENCE)
    CALL UPGMODE (USEMODE)
    OPEN 'EB.PARAMS' TO F.PARAMS THEN
        READ EB.VERSION FROM F.PARAMS,'VERSION' ELSE
            CRT 'Cannot read EB.PARAMS item VERSION'; STOP
        END
        IF EB.VERSION=5 THEN
            READ NOCOMPILE FROM F.PARAMS,'NOCOMPILE' ELSE
                CRT 'Cannot read EB.PARAMS item NOCOMPILE'; STOP
            END
        END
    END
    PORT=OCONV(OPTIONS,'MCN')
    OPTIONS=OCONV(OPTIONS,'MCA')
    OPTIONS=OCONV(OPTIONS,'MCU')
    AUTO.CAT=INDEX(OPTIONS,'C',1)
    Encrypt=INDEX(OPTIONS,'E',1)
    CHKSUMIT=INDEX(OPTIONS,'K',1)
    MSG.SUPP=INDEX(OPTIONS,'P',1)
    EDIT.ITEM=INDEX(OPTIONS,'X',1)
    NO.COMP=INDEX(OPTIONS,'N',1)
    AFTER=OPTIONS
    OLD='C'; GOSUB REMOVE.IT
    OLD='E'; GOSUB REMOVE.IT
    OLD='K'; GOSUB REMOVE.IT
    OLD='P'; GOSUB REMOVE.IT
    OLD='B'; GOSUB REMOVE.IT
    OLD='X'; GOSUB REMOVE.IT
    OLD='N'; GOSUB REMOVE.IT
    OPTIONS=AFTER
    USEMODE=''; PASSWD=''
    OPEN 'UPG.WORKFILE' TO F.UPG.WORKFILE THEN
        UPG=TRUE
        IF PORT#'' AND PORT#FG$TLINE THEN
            READ PASSWD FROM F.UPG.WORKFILE,PORT:'*TEMPID' THEN PASSWD=PASSWD<1>
        END
    END ELSE
        CRT 'CANNOT OPEN FILE UPG.WORKFILE'
        UPG=FALSE
        Encrypt=FALSE
    END
    EQU BAD.CHARS TO '$_%'    ;! if PROG starts with any of these skip it
    EQU CRIT.CHARS TO '<>#='  ;! SAC supports SAC filename criteria (options
    IF MSG.SUPP THEN PORT=''
    BEGIN CASE
    CASE PROGRAMS='*'
        SELECT F.BP
        SEL=1
    CASE INDEX(CRIT.CHARS,PROGRAMS[1,1],1)
        EXECUTE 'SELECT ':BP.FILE:' ':PROGRAMS CAPTURING OUTPUT
        SEL=SYSTEM(11)
    CASE 1
        IF PROGRAMS='' AND SYSTEM(11) THEN SEL=1 ELSE SEL=0
    END CASE
    CRT 'TO:':
    PROMPT ''
    INPUT DEST
    DEST=TRIM(DEST)
    IF DEST='' THEN
        CRT 'must specify (file and/or item list'
        STOP
    END
    IF DEST[1,1]='(' THEN
        BP.DEST=FIELD(FIELD(DEST,'(',2),' ',1)
        OPEN BP.DEST TO F.DEST ELSE STOP OPER,BP.DEST
        DEST=OCONV(DEST,'G1 99')
    END ELSE
        IF SEL OR DEST=PROGRAMS THEN
            CRT 'Cannot write to same item'
        END ELSE
            BP.DEST=BP.FILE
            F.DEST=F.BP
        END
    END
    ERRMSG=''
    P=1
    LPROG=''
    LOOP
        LOOP
            IF SEL THEN
                READNEXT Prog ELSE GOTO FINISH
            END ELSE
                Prog=FIELD(PROGRAMS,' ',P)
                IF Prog='' THEN GOTO FINISH
            END
            DPROG=FIELD(DEST,' ',P)
            IF DPROG='' THEN DPROG=Prog
            IF DPROG=Prog AND BP.FILE=BP.DEST THEN
                CRT 'Cannot overwrite ':Prog
                Prog=BAD.CHARS[1,1]:Prog
            END
            P=P+1
        WHILE INDEX(BAD.CHARS,Prog[1,1],1) DO REPEAT
    WHILE 1 DO
        LPROG=Prog
        READ Program FROM F.BP,Prog THEN
            NEW.PROGRAM=Program
            CALL UPGCHKENCRYPT (Program,ENCRYPTED,CHKSUM,SIZE,VERSION)
            IF ENCRYPTED THEN
                IF NOT(UPG) THEN
                    CRT 'Encrypted program...required UPG.WORKFILE'
                    STOP
                END
            END
            IF ENCRYPTED THEN
                IF PASSWD='' THEN CALL UPGPASSWD (PASSWD,F.UPG.WORKFILE)
                CALL UPGCONVERT (BP.FILE,Prog,NEW.PROGRAM,'DQ',USEMODE,F.UPG.WORKFILE,PASSWD,CONVOK)
                IF NOT(CONVOK) THEN STOP
            END
            ERRMSG=''; COMPILE.ITEM=TRUE
            IF EB.VERSION=5 THEN
                COMPILE.ITEM=NOT(INDEX(NEW.PROGRAM,NOCOMPILE,1))
                IF NOT(COMPILE.ITEM) THEN CRT Prog:' must be compiled with screen compiler'
            END
            IF COMPILE.ITEM THEN
                IF CHKSUMIT THEN
                    IF Program<2>=ENCRYPT.MESG<1> THEN
                        FOR AMC=1 TO ENCRYPT.HEADCNT; DEL Program<2>; NEXT AMC
                    END
                    CALL UPGCHKSUM(Program,CHKSUM,SIZE,F.UPG.WORKFILE,USEMODE,'BP',VERSION)
                    MESG=ENCRYPT.MESG
                    MESG<3>=MESG<3>:CHKSUM
                    MESG<4>=MESG<4>:SIZE
                    MESG<5>=MESG<5>:VERSION
                    MESG<6>=MESG<6>:TRIM(TIMEDATE())
                    INS MESG BEFORE Program<2>
                    WRITE Program ON F.BP,Prog
                    NEW.PROGRAM=Program
                END
                IF NO.COMP THEN
                    CONV=FALSE
                END ELSE
                    GOSUB PRE.COMPILE
                END
                IF Encrypt AND NOT(ENCRYPTED) THEN
                    IF USEMODE='' THEN CALL UPGMODE (USEMODE)
                    IF PASSWD='' THEN CALL UPGPASSWD (PASSWD,F.UPG.WORKFILE)
                    CALL UPGCONVERT (BP.DEST,Prog,Program,'E',USEMODE,F.UPG.WORKFILE,PASSWD,CONVOK)
                    IF CONVOK THEN WRITE Program ON F.DEST,DPROG
                END
            END
        END ELSE
            CRT Prog:' not on file'
        END
    REPEAT
FINISH:   !
    IF P>2 OR FIELD(FG$SENTENCE,' ',1)='PRE.COMPILE' ELSE
        ERRMSG=FIELD(ERRMSG,' ',DCOUNT(ERRMSG,' '))
        IF ERRMSG#'' THEN STOP ERRMSG<1,1,1>,LPROG
    END
    STOP
GET.WORD: !
    CNT=1; QFLG=0; DFLG=0; SQFLG=0; RFLG=0; SFLG=0
    WE=W-1
    FOR I=1 TO WE
        LOOP
            CHR=WORD[CNT,1]
            CNT=CNT+1
        UNTIL INDEX(',',CHR,1) AND NOT(SQFLG) AND NOT(QFLG) AND NOT(DFLG) AND NOT(SFLG) OR CHR='' DO
            BEGIN CASE
            CASE CHR='<'; DFLG=DFLG+NOT(QFLG OR SFLG)
            CASE CHR='>'; DFLG=DFLG-NOT(QFLG OR SFLG)
            CASE CHR='['; SQFLG=SQFLG+NOT(QFLG OR SFLG)
            CASE CHR=']'; SQFLG=SQFLG-NOT(QFLG OR SFLG)
            CASE CHR='"' AND NOT(SFLG); QFLG=NOT(QFLG)
            CASE CHR="'" AND NOT(QFLG); SFLG=NOT(SFLG)
            CASE CHR='('; RFLG=RFLG+NOT(QFLG OR SFLG)
            CASE CHR=')'; RFLG=RFLG-NOT(QFLG OR SFLG)
            END CASE
        REPEAT
    NEXT I
    S=CNT
    LOOP
        CHR=WORD[CNT,1]
        CNT=CNT+1
    UNTIL (INDEX(',',CHR,1) OR INDEX(')',CHR,1)) AND NOT(QFLG) AND NOT(RFLG) AND NOT(SQFLG) AND NOT(DFLG) AND NOT(SFLG) DO
        BEGIN CASE
        CASE CHR='<'; DFLG=DFLG+NOT(QFLG OR SFLG)
        CASE CHR='>'; DFLG=DFLG-NOT(QFLG OR SFLG)
        CASE CHR='['; SQFLG=SQFLG+NOT(QFLG OR SFLG)
        CASE CHR=']'; SQFLG=SQFLG-NOT(QFLG OR SFLG)
        CASE CHR='('; RFLG=RFLG+NOT(QFLG OR SFLG)
        CASE CHR=')'; RFLG=RFLG-NOT(QFLG OR SFLG)
        CASE CHR='"' AND NOT(SFLG); QFLG=NOT(QFLG)
        CASE CHR="'" AND NOT(QFLG); SFLG=NOT(SFLG)
        END CASE
    REPEAT
    WORD=WORD[S,CNT-S-1]
    RETURN
REMOVE.IT:!
    L=LEN(OLD)
    S=1
    LOOP
        PP=INDEX(AFTER[S,99999],OLD,1)
    WHILE PP DO
        PP=PP+S-1
        A=PP+L
        AFTER=AFTER[1,PP-1]:AFTER[A,99999]
        S=PP
    REPEAT
    RETURN
!
1000      ! Convert += -= :=
!
    VARIABLE=''
    O=POS-1
    LOOP
        CHR=LINE[O,1]
    WHILE CHR=' ' DO O=O-1 REPEAT
    C=O
    LOOP
        CHR=LINE[C,1]
    UNTIL INDEX(DLMS,CHR,1) OR C=0 DO C=C-1 REPEAT
    VARIABLE=LINE[C+1,O-C]
    LV=LEN(VARIABLE)
    FOR VV=1 TO LV WHILE VARIABLE#''
        CV=VARIABLE[VV,1]
        IF INDEX(DELIMS,CV,1) AND NOT(INDEX('()<>,',CV,1)) THEN VARIABLE=''
    NEXT VV
    IF VARIABLE#'' THEN LINE=LINE[1,C]:VARIABLE:'=':VARIABLE:CHK[1,1]:LINE[POS+2,99]
    RETURN
