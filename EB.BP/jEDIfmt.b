!
! Format Basic Program Listing
    DEFC INT JBASEEmulateGETINT(INT, INT)

    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    mv_start=IF_COMPILED_PRIME
    DIM ITEM(50000), T(100), CT(100), PP(10), SP(10), PS(5), SS(5), PE(5), SE(5)
    DIM OPTION(26), IND(5), DELCHR(5), DL(5)
    EQU TRUE TO 1, FALSE TO 0, BELL TO CHAR(7)
    EQU AM TO CHAR(254), VM TO CHAR(253), SVM TO CHAR(252)
    EQU SPC TO ' ', MAX TO 99999
    EQU B.OPTION TO OPTION(2)
    EQU C.OPTION TO OPTION(3)
    EQU D.OPTION TO OPTION(4)
    EQU F.OPTION TO OPTION(6)
    EQU I.OPTION TO OPTION(9)
    EQU L.OPTION TO OPTION(12)
    EQU M.OPTION TO OPTION(13)
    EQU O.OPTION TO OPTION(15)
    EQU Q.OPTION TO OPTION(17)
    EQU R.OPTION TO OPTION(18)
    EQU S.OPTION TO OPTION(19)
    EQU T.OPTION TO OPTION(20)
    EQU U.OPTION TO OPTION(21)
!
    EQU PLSQL TO OPTION(17)
!
    EQU SQ TO DELCHR(1)
    EQU DQ TO DELCHR(2)
    EQU FS TO DELCHR(3)
    SQ="'"
    DQ='"'
    FS='/'
!
    FG$SENTENCE=SENTENCE(-1)
    BUFF=FG$SENTENCE
    BUFF=TRIM(BUFF)
    OPTS=OCONV(TRIM(FIELD(BUFF,'(',2)),'MCU')
    IF INDEX('Q',OPTS,1) THEN OPTS:='CFO'
    BUFF=TRIM(FIELD(BUFF,'(',1))
    BUFF=CHANGE(BUFF,SPC,AM)
    IF BUFF<1>='RUN' THEN DEL BUFF<1>; DEL BUFF<1>
    PROMPT ''
    FNAME=BUFF<2>
    IF FNAME='' THEN
        CRT 'Filename: ':
        INPUT FNAME
    END
    IF FNAME='?' THEN
        CRT 'jEDIfmt'
        CRT '  * Used to format programs'
        CRT
        CRT 'Format:'
        CRT '  jEDIfmt filename {item list} {(options)}'
        CRT '  * options:'
        CRT "       b - expand lines containing only one (!) to (*)'s"
        CRT '       c - do not indent comment lines'
        CRT '       f - update to file (disables output)'
        CRT '       i - use internal defaults for formatting instead of "*blist" defaults'
        CRT '       l - suppress line # output'
        CRT '       m - double space'
        CRT '       n - no wait at end of page'
        CRT '       o - overlay existing item (used with f-option)'
        CRT '           otherwise, overlay id=id.BLIST'
        CRT '       q - format PL/SQL code'
        CRT '       p - output to spooler'
        CRT '       r - force all indentations to zero'
        CRT '       s - draw verical structure lines'
        CRT '       t - output to crt (defaulted without f-option)'
        CRT '       u - update item with unresolved format structure'
        STOP
    END
    OPEN FNAME TO FILE ELSE STOP 201,FNAME
    MAT IND=0; IND(1)=4; IND(2)=4
    IF FNAME = 'JET.PASTE' THEN
        ID=BUFF<3>
        rFname = FIELD(ID, '%', 2)
        IF rFname = '' THEN rFname = FNAME
    END
    OPEN 'DICT',rFname TO DFILE THEN
        MATREAD IND FROM DFILE,'EB_INDENT' ELSE
            OPEN 'EB.PARAMS' THEN
                MATREAD IND FROM 'EB_INDENT' ELSE NULL
            END
        END
    END ELSE
        OPEN 'EB.PARAMS' THEN
            MATREAD IND FROM 'EB_INDENT' ELSE NULL
        END
    END
    MAT OPTION=0
    IF OPTS='' ELSE
        LOPT=LEN(OPTS)
        FOR I=1 TO LOPT
            X=INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',OPTS[I,1],1)
            IF X THEN OPTION(X)=TRUE
        NEXT I
    END
    MAT PP=1
    MAT SP=1
    MAT PS=0
    MAT SS=1
    EXCEPT.IND=0
    MATCH.IND=''
    BEGIN CASE
        CASE PLSQL
            IND(1)=0;
            COMMENTS='--'; COMMENTLEN=2
            ENDW='END IF'; ENDSW='ELSIF '
            PREFIX='ELSIF':AM:'END':AM:'EXCEPTION':AM:'FOR'
            PREFIX.IND = -1:AM:-1:AM:-1:AM:0
            PREFIX.NEXT = 1:AM:0:AM:0:AM:1
            EXACT='BEGIN':AM:'ELSE':AM:'ELSIF':AM:'END':AM:'END IF':AM:'END LOOP'
            EXACT.IND = 0:AM:-1:AM:-1:AM:-1:AM:-1:AM:-1
            EXACT.NEXT = 1:AM:1:AM:1:AM:0:AM:0:AM:0
            SUFFIX='AS':AM:'ELSE':AM:'THEN'
            CONTFIX='THEN'
            EXCEPTIONS='WHEN':AM:'FORALL'
            MATCH.S='P4':VM:'S3'
            MATCH.S<2>='E4':VM:'E3'
            MATCH.E='E3':VM:'E4'
            MATCH.E<2>='S3':VM:'P4'
!
            PP(4)=0     ;! FOR
!
            SP(1)=0     ;! ELSIF
            SP(2)=0     ;! END
!
            PS(2)=1     ;! ELSE
!
            PE(1)=0; SE(1)=1      ;! BEGIN CASE
            PE(2)=0; SE(2)=0      ;! END
            PE(3)=0; SE(3)=0      ;! END IF
            PE(4)=0; SE(4)=0      ;! END LOOP
        CASE 1
            COMMENTS='!*#'; COMMENTLEN=1
            EXACT = 'BEGIN CASE':AM:'END':AM:'END CASE':AM:'END ELSE':AM:'END THEN':AM:'LOOP':AM:'REPEAT'
            EXACT.IND = 0:AM:-1:AM:-2:AM:-1:AM:-1:AM:0:AM:-1
            EXACT.NEXT = 2:AM:0:AM:0:AM:1:AM:1:AM:1:AM:0
            PREFIX='CASE':AM:'END':AM:'FOR':AM:'LOOP':AM:'NEXT':AM:'UNTIL':AM:'WHILE'
            PREFIX.IND = -1:AM:-1:AM:0:AM:0:AM:-1:AM:-1:AM:-1
            PREFIX.NEXT = 1:AM:0:AM:1:AM:1:AM:0:AM:1:AM:1
            SUFFIX='ELSE':AM:'ERROR':AM:'LOCKED':AM:'THEN'
            MATCH.S='P3':VM:'P4'
            MATCH.S<2>='P5':VM:'E3':VM:'P6':VM:'S3':VM:'E3':VM:'P6':VM:'S3':VM:'E3':VM:'P6':VM:'S3'
            MATCH.E='E3':VM:'P5':VM:'P6':VM:'S3'
            MATCH.E<2>='P4':VM:'P3':VM:'P4':VM:'P4'
            EXCEPTIONS=''
            PP(3)=0     ;! FOR
            PP(4)=0     ;! LOOP
!            PP(6)=0     ;! REPEAT
!
            SP(2)=0     ;! END
            SP(5)=0     ;! NEXT
!
            CONTFIX='ELSE':AM:'LOCKED':AM:'THEN'
!
            SS(4)=-1    ;! REPEAT
!
            PE(1)=0; SE(1)=2      ;! BEGIN CASE
            PE(2)=1; SE(2)=0      ;! END CASE
            PE(3)=1; SE(3)=0      ;! REPEAT
    END CASE
!
    LBL.SUFFIX=':!*'; SC=';'
    AL='AL'; TOW='TO'; REMW='REM '; GOW='GO'
!
    IF SYSTEM(11) THEN LISTACTIVE=TRUE ELSE
        LISTACTIVE=FALSE
        ID=BUFF<3>
        IF ID='' THEN
            CRT 'Program Name: ':
            INPUT ID
            BUFF=TRIM(BUFF:SPC:ID)
            BUFF=CHANGE(BUFF,SPC,AM)
            ID=BUFF<3>
        END
        IF ID='*' THEN LISTACTIVE=TRUE ELSE
            IF ID='' THEN STOP
            NOIDS=DCOUNT(BUFF,AM)-2
            IF NOIDS GT 1 THEN
                IDLIST=SPC
                FOR A=1 TO NOIDS
                    IDLIST:="'":BUFF<A+2>:"'"
                NEXT A
                EXECUTE 'SELECT ':FNAME:IDLIST CAPTURING OPUT
                IF SYSTEM(11) ELSE STOP
                LISTACTIVE=TRUE
            END
        END
    END
    IF LISTACTIVE THEN SELECT FILE TO LIST1
    IF F.OPTION THEN
        T.OPTION=FALSE
        PRINTER OFF
        IF O.OPTION THEN ID.SUFFIX='' ELSE
            CRT 'Overlay existing items (Y,<N>)? ':
            INPUT O.OPTION
            O.OPTION=TRIM(O.OPTION)[1,1]
            IF O.OPTION='Y' THEN ID.SUFFIX='' ELSE ID.SUFFIX='.BLIST'
        END
    END
!
! GET TAB SETTINGS
! IND(1)=LEFT MARGIN INDENT
! IND(2)=CODE INDENT
! IND(3)=COMMENT LEFT MARGIN INDENT
! IND(4)=COMMENT INDENT
!
    IF R.OPTION THEN
        MAT T=0
        MAT CT=0
    END ELSE
        T(1)=IND(1)
        FOR I=2 TO 100
            T(I)=T(I-1)+IND(2)
        NEXT I
        IF C.OPTION THEN MAT CT=0 ELSE
            CT(1)=IND(3)
            CT(2)=IND(3)
            IF IND(4)=0 THEN MAT CT=0 ELSE
                FOR I=3 TO 100
                    CT(I)=CT(I-1)+IND(4)
                NEXT I
            END
        END
    END
!
    IF T.OPTION THEN
        PAD.FMT=SPACE(130)
        FOR I=1 TO 100
            PAD.FMT=PAD.FMT[1,T(I)]:'|':PAD.FMT[T(I)+2,200]
        NEXT I
    END
!
    DEBUG_LINE = ''
    rc = GETENV('jEDIfmt_DEBUG', DEBUG_LINE)
!
    EOL=FALSE
    ICNT=1
    LOOP
        IF LISTACTIVE THEN
            READNEXT ID FROM LIST1 ELSE EOL=TRUE
        END
    UNTIL EOL DO
        READU D.ITEM FROM FILE,ID THEN
            MATPARSE ITEM FROM D.ITEM USING AM SETTING NA; IF NA # DCOUNT(D.ITEM,AM) THEN MATPARSE ITEM FROM D.ITEM USING AM SETTING NA
            IF L.OPTION THEN OMASK='' ELSE
                IF NA > 999 THEN OMASK='R%4 ' ELSE OMASK='R%3 '
            END
            TECNT=0
            OLEN=LEN(0 OMASK)
            D.ITEM=''
!
! HEADING
!
            IF T.OPTION THEN
                HEADING FNAME:' --> ':ID:"'JT'  Page'PL'"
                IF ICNT GT 1 THEN PAGE  ;!ELSE PAGE 1
            END
!
! MAIN LOOP
!
            NEXT.INDEX=1
            DQFLAG=FALSE; SQFLAG=FALSE
            FOR LNO=1 TO NA
                CUR.INDEX=NEXT.INDEX
                LINE=''
                STMT=1
                COMMENT=FALSE
                CMNT=FALSE
                ATTR=ITEM(LNO)
                ONEND=FALSE
                IF LNO=DEBUG_LINE THEN DEBUG
!
                IF TRIM(ATTR)='' THEN ATTR=''
                LBL=''
                LOOP
                    GOSUB SPLITSTMT
                    TCH1=T.STMT[1,COMMENTLEN]
                    FWORD=''; F1=''
                    IF INDEX(COMMENTS,TCH1,1) OR T.STMT[1,4]=REMW THEN
                        IF STMT=1 THEN COMMENT=TRUE
                        CMNT=TRUE
                    END ELSE
                        IF CMNT ELSE
                            IF PLSQL THEN
                                PLSTMT=SWAP(T.STMT,SQ:SQ,'')
                                DQPOS=INDEX(PLSTMT,DQ,1)
                                DQCNT=COUNT(PLSTMT,DQ)
                                SQPOS=INDEX(PLSTMT,SQ,1)
                                SQCNT=COUNT(PLSTMT,SQ)
                                IF DQFLAG THEN    ;! check for DQ
                                    DQFLAG=MOD(DQCNT,2)
                                END ELSE
                                    IF SQFLAG THEN          ;! check for SQ
                                        SQFLAG=MOD(SQCNT,2)
                                    END ELSE
                                        IF DQPOS<SQPOS THEN
                                            DQFLAG=MOD(DQCNT,2)
                                        END ELSE
                                            SQFLAG=MOD(SQCNT,2)
                                            IF SQFLAG AND NOT(INDEX(PLSTMT[1,INDEX(PLSTMT,SQ,SQCNT)], DQ, 1)) THEN DEBUG
                                        END
                                    END
                                END
                            END ELSE SQFLAG = FALSE; DQFLAG = FALSE
                            IF NOT(SQFLAG OR DQFLAG) THEN
                                NUM.FLDS=DCOUNT(T.STMT,SPC)
                                F1=FIELD(T.STMT,SPC,1)
                                IF NUM(F1) OR (INDEX(LBL.SUFFIX,F1[LEN(F1),1],1) AND NOT(INDEX(F1,DQ,1) OR INDEX(F1,SQ,1))) THEN
                                    LBL=F1; F1=FIELD(T.STMT,SPC,2)
                                END
                                FLAST=FIELD(T.STMT,SPC,NUM.FLDS)
                                NFLAST=FIELD(T.STMT,SPC,NUM.FLDS-1)
                                IF NFLAST=TOW THEN
                                    NFLAST=FIELD(T.STMT,SPC,NUM.FLDS-2)[1,2]
                                END ELSE NFLAST=NFLAST[1,2]
!
                                IF 1 THEN         ;!NOT(PLSQL) THEN
                                    LOCATE T.STMT IN EXACT<am_start> BY 'AL' SETTING EPOS ELSE
                                        EPOS = FALSE
                                    END
                                    LOCATE FLAST IN SUFFIX<am_start> BY 'AL' SETTING SPOS ELSE
                                        SPOS = FALSE
                                    END
                                    LOCATE FLAST IN EXACT<am_start> BY 'AL' SETTING FLPOS ELSE
                                        FLPOS = FALSE
                                    END
                                    LOCATE F1 IN PREFIX<am_start> BY 'AL' SETTING FPOS THEN
                                        IF PLSQL AND FIELD(T.STMT, SPC, 2) = 'UPDATE' THEN FPOS=FALSE
                                    END ELSE
                                        FPOS = FALSE
                                    END
                                    IF LEN(NEXT.ATTR) THEN
                                        SAVE.T.STMT = T.STMT
                                        LOOP
                                            ATTR = TRIM(NEXT.ATTR)
                                            F1 = FIELD(ATTR, ' ',1)
                                            FLAST = FIELD(ATTR, ' ', DCOUNT(ATTR, ' '))
                                            GOSUB SPLITSTMT
                                        UNTIL NEXT.ATTR = '' DO REPEAT
                                        LOCATE F1 IN PREFIX<am_start> BY 'AL' SETTING NFPOS ELSE NFPOS = FALSE
                                        LOCATE FLAST IN EXACT<am_start> BY 'AL' SETTING NLPOS ELSE NLPOS = FALSE
                                        T.STMT = SAVE.T.STMT
                                        ATTR = ITEM(LNO)
                                    END ELSE
                                        NFPOS=FALSE
                                        NLPOS=FALSE
                                    END
                                    BEGIN CASE
                                        CASE EPOS
                                            CUR.INDEX += EXACT.IND<EPOS>
                                            NEXT.INDEX = CUR.INDEX + EXACT.NEXT<EPOS>
                                        CASE SPOS AND NOT(PLSQL)
                                            NEXT.INDEX = CUR.INDEX + 1
                                        CASE FPOS
                                            CUR.INDEX += PREFIX.IND<FPOS>
                                            IF NOT(FLPOS OR NFPOS OR NLPOS) THEN
                                                NEXT.INDEX = CUR.INDEX + PREFIX.NEXT<FPOS>
                                            END ELSE NEXT.INDEX = CUR.INDEX
                                        CASE SPOS
                                            NEXT.INDEX = CUR.INDEX + 1
                                    END CASE
                                END ELSE
!
                                    MS.INDEX=''
                                    ME.INDEX=FALSE
                                    LOCATE F1 IN PREFIX<am_start> BY AL SETTING P.INDEX THEN
                                        OFFSET=1
                                        LOCATE T.STMT IN PREFIX<am_start> BY AL SETTING E.INDEX THEN
                                            IF EXCEPT.IND THEN
                                                IF EXCEPT.IND=1 THEN OFFSET=2
                                                EXCEPT.IND-=1
                                            END
                                        END
                                        IF STMT=1 THEN CUR.INDEX-=PP(P.INDEX)*OFFSET
                                        NEXT.INDEX-=(PP(P.INDEX)-SP(P.INDEX))*OFFSET
                                        M.INDEX='P':P.INDEX
                                        LOCATE M.INDEX IN MATCH.E<1,mv_start> BY AL SETTING ME.INDEX ELSE ME.INDEX=FALSE
                                        LOCATE M.INDEX IN MATCH.S<1,mv_start> BY AL SETTING MATCH.POS THEN MS.INDEX=M.INDEX
                                    END ELSE ONEND=FALSE; P.INDEX=FALSE
!
                                    IF NFLAST=GOW ELSE
                                        LOCATE FLAST IN SUFFIX<am_start> BY AL SETTING S.INDEX THEN
                                            IF STMT=1 THEN CUR.INDEX-=PS(S.INDEX)
                                            NEXT.INDEX-=(PS(S.INDEX)-SS(S.INDEX))
                                            ONEND=FALSE
                                            M.INDEX='S':S.INDEX
                                            IF NOT(ME.INDEX) THEN
                                                LOCATE M.INDEX IN MATCH.E<1,mv_start> BY AL SETTING ME.INDEX ELSE ME.INDEX=FALSE
                                            END
                                            IF MS.INDEX='' THEN
                                                LOCATE M.INDEX IN MATCH.S<1,mv_start> BY AL SETTING MATCH.POS THEN MS.INDEX=M.INDEX
                                            END
                                        END
                                    END
                                    IF T.STMT=ENDW THEN
                                        TECNT-=1
                                        ONEND=TRUE
                                    END ELSE ONEND=FALSE
                                    LOCATE T.STMT IN EXACT<am_start> BY AL SETTING E.INDEX THEN
                                        IF STMT=1 THEN CUR.INDEX-=PE(E.INDEX)
                                        NEXT.INDEX-=(PE(E.INDEX)-SE(E.INDEX))
                                        IF (F1:SPC)=ENDSW THEN TECNT+=1
                                        ONEND=FALSE
                                        M.INDEX='E':E.INDEX
                                        IF NOT(ME.INDEX) THEN
                                            LOCATE M.INDEX IN MATCH.E<1,mv_start> BY AL SETTING ME.INDEX ELSE ME.INDEX=FALSE
                                        END
                                        IF MS.INDEX='' THEN
                                            LOCATE M.INDEX IN MATCH.S<1,mv_start> BY AL SETTING MATCH.POS THEN MS.INDEX=M.INDEX
                                        END
                                    END
                                    IF MS.INDEX#'' THEN
                                        IF (F1:SPC)#ENDSW THEN
                                            MS.INDEX<1,-1>=CUR.INDEX
                                            INS MS.INDEX BEFORE MATCH.IND<1>
                                        END
                                    END ELSE
                                        IF ME.INDEX THEN
                                            POS=1
                                            LOOP
                                                MS.INDEX=MATCH.E<2,ME.INDEX,POS>:VM:CUR.INDEX
                                            UNTIL MS.INDEX<1,1>='' OR MS.INDEX=MATCH.IND<1> DO POS+=1 REPEAT
                                            IF MS.INDEX<1,1>#'' THEN
                                                DEL MATCH.IND<1>
                                            END ELSE
                                                CRT 'Indent mismatch detected at ':LNO; RQM
                                                GO Error
                                            END
                                        END
                                    END
                                    IF F1#'' THEN
                                        LOCATE F1 IN EXCEPTIONS<am_start> BY AL SETTING EX.INDEX THEN
                                            EXCEPT.IND+=1
                                        END
                                    END
                                END
                            END
                        END
                    END
                UNTIL NEXT.ATTR='' DO
                    STMT+=1
                    LINE:=ATTR[INDEX(ATTR,T.STMT[1,1],1), MAX]
                    IF INDEX(COMMENTS,NEXT.ATTR[1,COMMENTLEN],1) ELSE LINE:=SPC
                    ATTR=NEXT.ATTR
                REPEAT
                LINE:=ATTR[INDEX(ATTR,T.STMT[1,1],1), MAX]
                IF T.OPTION THEN
                    IF M.OPTION THEN IF LNO GT 1 THEN PRINT
                    IF L.OPTION ELSE PRINT LNO OMASK:
                END
                IF COMMENT THEN
                    IF NOT(CUR.INDEX) THEN CUR.INDEX=1
                END ELSE
                    IF ONEND THEN
                        IF NOT(TECNT) THEN
                            IF NOT(CUR.INDEX) THEN CUR.INDEX=1
                        END
                    END
                END
                IF CUR.INDEX > 0 OR (T.STMT = 'END' AND LNO = NA AND CUR.INDEX = 0) THEN
                    IF CUR.INDEX = 0 THEN
                        PAD = 0
                        LINE = ''
                    END ELSE
                        IF COMMENT THEN PAD=CT(CUR.INDEX) ELSE PAD=T(CUR.INDEX)
                    END
                    IF T.OPTION THEN
                        IF S.OPTION THEN
                            PRINT PAD.FMT[1,PAD]:
                            PAD2=PAD.FMT[1,PAD]
                        END ELSE
                            PRINT SPACE(PAD):
                            PAD2=SPACE(PAD)
                        END
                    END
                END ELSE
                    PAD=1
                    PAD2=SPACE(1)
                    IF SYSTEM(1) ELSE CRT BELL:
                    IF NOT(F.OPTION OR T.OPTION) THEN CRT ID
                    PRINT '? ':LNO 'R%4 ':LINE
                    RQM;RQM
                    CUR.INDEX=1
                    NEXT.INDEX=1
                END
                LINELEN=SYSTEM(2)-OLEN-PAD
                LNLEN=LINELEN+1
                IF T.OPTION THEN
                    IF COMMENT AND B.OPTION THEN
                        IF LINE='!' THEN LINE=STR('*',LINELEN)
                    END
                    LJUST='L#':LINELEN
                    PRINT LINE LJUST
                    LOOP WHILE LEN(LINE) GT LNLEN DO
                        PRINT SPACE(OLEN):PAD2:
                        PRINT LINE[LNLEN,LINELEN]
                        LNLEN+=LINELEN
                    REPEAT
                END
                IF TRIM(LINE)='' THEN LINE=''
                IF F.OPTION THEN
                    IF LINE='' THEN ITEM(LNO)='' ELSE
                        IF LBL='' THEN
                            LINE=SPACE(PAD):LINE
                        END ELSE
                            PAD-=LEN(LBL)
                            IF PAD<1 THEN PAD=1
                            LINE=LBL:SPACE(PAD):TRIM(OCONV(LINE,'G1 999'), ' ', 'L')
                        END
                        LINE=TRIM(LINE,' ',"T")
                        ITEM(LNO)=LINE
                    END
                END
            NEXT LNO
Error:      !
            IF F.OPTION THEN
                IF CUR.INDEX > 1 THEN
                    IF U.OPTION THEN MATWRITE ITEM ON FILE,ID:ID.SUFFIX
                END ELSE MATWRITE ITEM ON FILE,ID:ID.SUFFIX
            END
            IF SYSTEM(1) ELSE
                IF F.OPTION THEN
                    IF T.OPTION THEN CRT '--- done ---': ELSE
                        CRT ICNT 'R#6 ':ID:ID.SUFFIX:
                    END
                END ELSE CRT '--- done ---':
                IF CUR.INDEX > 1 THEN
                    CRT @(-3):BELL:' * unresolved structure * at line ':LNO:' (':CUR.INDEX:')'
                    CRT T.STMT
                    INPUT CONT,1
                END ELSE CRT
            END
        END ELSE
            CRT BELL:SQ:ID:"' not on file!"
            INPUT CONT,1
            RELEASE FILE,ID
        END
        IF LISTACTIVE THEN ICNT=ICNT+1 ELSE STOP
    REPEAT
    STOP
!
SPLITSTMT:
!
    IF INDEX(COMMENTS,ATTR,1) THEN SEMI.COLON=0 ELSE SEMI.COLON=INDEX(ATTR,';',1)
    SEMICTR=2
    IF SEMI.COLON THEN
        FOR DT=1 TO 3 WHILE SEMI.COLON
            NODEL=DCOUNT(ATTR,DELCHR(DT))
            FOR DC=1 TO NODEL STEP 2 WHILE SEMI.COLON
                DL(DT)=INDEX(ATTR,DELCHR(DT),DC)
                IF SEMI.COLON LT DL(DT) ELSE
                    DL(DT)=INDEX(ATTR,DELCHR(DT),DC+1)
                    LOOP WHILE SEMI.COLON AND SEMI.COLON LT DL(DT) DO
                        SEMI.COLON=INDEX(ATTR,SC,SEMICTR)
                        SEMICTR+=1
                    REPEAT
                END
            NEXT DC
        NEXT DT
    END
    IF SEMI.COLON THEN
        T.STMT=TRIM(ATTR[1,SEMI.COLON-1],' ',"B")
        NEXT.ATTR=ATTR[SEMI.COLON+1, MAX]
        ATTR=ATTR[1,SEMI.COLON]
    END ELSE
        T.STMT=TRIM(ATTR,' ',"B")
        NEXT.ATTR=''
    END
    RETURN
