    SUBROUTINE EB_SEARCH
    INCLUDE EB.INCLUDES EB_LEXER
    INCLUDE EB.INCLUDES lex.h
    INCLUDE EB.EQUS EB.COMMON
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS COLOURS
    DEFFUN EB_REGEX()
MAIN$:!
    MAX=LEN(REC)
    LAST.AM=DCOUNT(REC,AM)
    PR="--Press <RETURN>"
    RefreshRequired=FALSE
    DELIMS=' ():;+-*/,&!^#=<>[]@{}':AM:VM:SVM:TAB
!
    MSG='String (F1]/[F2]/up-arrow/down-arrow) '
    ICOL=LEN(MSG); IROW=(PDEPTH-1)
    Indent=ITAB<ITABPOS>
    GOSUB DisplayPrompt
    SAVEROW=ROW
    L=PWIDTH-1-ICOL; SSTR=''; SRCH.STR=''
    SPWIDTH = PWIDTH
!    PWIDTH = 999
    OPTIONS=''
    RPOS=1
    IF FG_ACT.CODE=FG_MULTI.CODE THEN FG_ACT.CODE=FALSE ELSE FG_LAST.ACT.CODE=FG_ACT.CODE
    RSEARCH=(FG_LAST.ACT.CODE=FG_BSEARCH.CODE)
    IF FG_ACT.CODE THEN
        CONVERT AM TO VM IN SSS
        LOOP
            CRT @(ICOL,IROW):
            IF RPOS AND RPOS<21 THEN Z=SSS<1,RPOS>
            INPTYPE='LIT'; GOSUB INPT
            LOCATE FG_ACT.CODE IN CYCLES<am_start> SETTING POS ELSE POS=FALSE
            IF FG_ACT.CODE=FG_HLP.CODE THEN
                CALL EB_HELP('EBSEARCH', TRUE)
                CALL EB_REFRESH
                GOSUB DisplayPrompt
                POS = TRUE
            END
        WHILE POS DO
            BEGIN CASE
                CASE FG_ACT.CODE=FG_OPT.CODE
                    CALL EB_CHOICES(50,8,31,10,'',SSS,Z,1,RPOS,1,'L#30','Previous Searches')
                CASE FG_ACT.CODE=FG_SKP.CODE OR FG_ACT.CODE=FG_MULTI.CODE; RPOS+=1
                CASE 1; RPOS-=1
            END CASE
            IF RPOS<1 THEN RPOS=0
            FG_ACT.CODE=FALSE
        REPEAT
        CONVERT VM TO AM IN SSS
    END ELSE
        IF COUNT(PSSTR,AM) THEN PSSTR = PSSTR<2>:';':PSSTR<1>
        Z=PSSTR
        IF LEN(Z)=0 THEN Z=SSS<RPOS>
    END
    IF FG_ACT.CODE=FG_SEARCH.CODE THEN FG_ACT.CODE=FALSE
    IF FG_ACT.CODE THEN GO 4096
    LOCATE Z IN SSS<am_start> SETTING POS ELSE
        IF POS<20 THEN POS=0 ELSE POS=20
    END
    IF POS THEN DEL SSS<POS>
    INS Z BEFORE SSS<1>
    CRT MSG.AKN:
    STR.POS=0
    SSTR=Z
    IF INDEX(SSTR,'\',1) THEN
        L=LEN(SSTR)
        FOR I=1 TO L
            IF SSTR[I,1]='\' THEN
                CHR.NBR=SSTR[I+1,3]
                IF CHR.NBR MATCHES "3N" THEN
                    SSTR=SSTR[1,I-1]:CHAR(CHR.NBR):SSTR[I+4,MAX]
                    L-=3
                END
            END
        NEXT I
    END
    IF SSTR='///' OR SSTR='\\\' THEN
        PSSTR=SSTR
        IF SSTR='///' THEN LNM=1 ELSE LNM=-1
        DUMMY=RDSP(LROW); GOSUB FORMAT
        SPOS=INDEX(REC,AM,INDROW+ROW)
        curlies = 0
        IF DUMMY='' THEN GO 4096 ELSE
            STRT=INDROW+ROW+LNM
            IF DUMMY[1,3] NE '!#!' THEN
                Y=I-Indent
                IF Y < 2 THEN Y=3
                IF DUMMY='{' OR DUMMY='}' THEN
                    IF LNM=-1 THEN SSTR='{' ELSE SSTR='}'
                    IF SSTR='{' THEN RSTR='}' ELSE RSTR='{'
                    curlies = 1
                END ELSE
                    SSTR=(Y-2):"X' '1X0X"
                END
            END ELSE
                SSTR="'":DUMMY:"'"
                IF LEN(DUMMY)>3 THEN SSTR:='0X'
                Y=LEN(DUMMY)
            END
            INCLUDE EB.OS.INCLUDES REMOVE.LINE
            IF LINE='' THEN
                INCLUDE EB.OS.INCLUDES REMOVE.LINE
            END
            CALL EB_TABS(LINE,PWIDTH,0,0)
            LOOP
                done = (STRT=1 AND LNM<0)
                IF curlies THEN
                    curlies += COUNT(LINE,RSTR) - COUNT(LINE, SSTR)
                    done += (curlies < 1)
                END ELSE
                    done += (NOT(TRIM(LINE)[1,COMMENTLEN]=COMMENT) AND LINE[1,Y] MATCHES SSTR AND TRIM(LINE[1,Y]) NE '')
                END
                done += (LNM>=LAST.AM)
            UNTIL done DO
                STRT+=LNM
                INCLUDE EB.OS.INCLUDES REMOVE.LINE
                CALL EB_TABS(LINE,PWIDTH,0,0)
                IF DUMMY[1,3] NE '!#!' THEN
                    IF INDEX(COMMENT,TRIM(LINE)[1,COMMENTLEN],1) THEN LINE=COMMENT
                END
            REPEAT
            IF LINE NE '' THEN
                IF NOT(INDEX('{}',SSTR,1)) THEN
                    IF DUMMY[1,3] NE '!#!' THEN
                        SSTR=FIELD(LINE[Y,9],' ',1); LINE=OCONV(LINE[Y,20],'G 2')
                    END ELSE SSTR=FIELD(LINE,' ',1)
                    LOCATE SSTR IN DUMMY<am_start> SETTING POS ELSE
                        LOCATE LINE IN DUMMY<am_start> SETTING POS ELSE
                            DUMMY=SWAP(DUMMY,AM,' or ')
                            CRT @(0,(PDEPTH-1)):BELL:CLEOL:'Expected ':DUMMY[1,SPWIDTH-14]:'...':; RQM
                        END
                    END
                END
                IF STRT<INDROW OR STRT>(INDROW+(PDEPTH-1)) THEN
                    IF CHANGED THEN GOSUB 6000
                    INDROW=STRT; SCR.UD=TRUE; OFFSET=0; COL=5; ROW=0
                END ELSE ROW=STRT-INDROW; COL=I+Indent
                SCR.UD=1
                CALL EB_REFRESH
                GO 4099
            END ELSE GO 4096
        END
    END
    IF SSTR=ESC THEN GO 4096
    IF SSTR="" AND PSSTR="" THEN GO 4096
    REPEATSEARCH = (SSTR EQ PSSTR)
    OPTIONS=FIELD(SSTR,';',1)
    IF SSTR[1,1] NE ';' AND (LEN(OPTIONS) EQ 0 OR OPTIONS EQ SSTR) THEN
        OPTIONS=FIELD(SSTR,'/',1)
    END
    IF OPTIONS[-1,1] EQ '\' THEN
        OPTIONS = ''
    END ELSE
        SSTR=SSTR[COL2()+1,MAX]
        IF SSTR='' THEN SSTR=OPTIONS; OPTIONS='' ELSE OPTIONS=OCONV(OPTIONS,'MCU')
    END
    WHOLE.WORDS=INDEX(OPTIONS,'V',1) NE 0
    WHOLE.WORDS:=INDEX(OPTIONS,'S',1) NE 0
    REGEX.SEARCH = INDEX(OPTIONS,'X',1)
    WHOLE.WORDS:=REGEX.SEARCH
    CASE.INSENSITIVE = INDEX(OPTIONS,'I',1)
    WHOLE.WORDS:=CASE.INSENSITIVE
    IF CASE.INSENSITIVE THEN SSTR = UPCASE(SSTR)
    IF INDEX(OPTIONS,'C',1) THEN        ;! convert chars
        L=LEN(SSTR)
        FOR I=1 TO L
            CHR.NBR=SSTR[I,3]
            IF CHR.NBR MATCHES "3N" THEN
                SSTR=SSTR[1,I-1]:CHAR(CHR.NBR):SSTR[I+3,MAX]
                L-=Indent
            END
        NEXT I
    END
    IF OPTIONS NE '' THEN PSSTR=OPTIONS:';':SSTR
    SRCH.STR=SSTR
    IF INDEX(OPTIONS,'A',1) THEN GO 4100          ;! display all occurrences.
    STRT=INDROW+ROW
    SAVE.INDROW=INDROW
RETRY:
    IF SSTR[1,1]='^' THEN
        IF SSTR[2,1]='^' THEN SSTR=SSTR[2,MAX] ELSE
            SSTR=AM:SSTR[2,MAX]
        END
    END
    IF SSTR[LEN(SSTR),1]='$' THEN
        IF SSTR[LEN(SSTR)-1,1]='$' THEN SSTR=SSTR[1,LEN(SSTR)-1] ELSE
            SSTR=SSTR[1,LEN(SSTR)-1]:AM
        END
    END
    IF RSEARCH THEN FG_ACT.CODE=FG_BSEARCH.CODE
    IF STRT>1 THEN
        IF STRT>LAST.AM THEN
            STR.POS = FALSE
        END ELSE
            CALL EB_TRIM(TMP,RDSP(LROW)[1,LCOL],' ','T')
            LINE.POS=INDEX(REC,AM,STRT-1)+LEN(TMP)+2
            IF FG_ACT.CODE=FG_BSEARCH.CODE THEN
                RPOS=1; EPOS=LINE.POS
            END ELSE
                RPOS=LINE.POS; EPOS=MAX
            END
            GOSUB SETMREC
            CALL EB_FIND(STR.POS,WHOLE.WORDS:'')
        END
        IF STR.POS ELSE GO 4095
        STR.LINE=COUNT(MREC[1,STR.POS],AM)
    END ELSE
        IF FG_ACT.CODE=FG_BSEARCH.CODE THEN
            RPOS=1; EPOS=LCOL
        END ELSE
            RPOS=LCOL+1; EPOS=MAX
        END
        GOSUB SETMREC
        CALL EB_FIND(STR.POS,WHOLE.WORDS:'')
        IF NOT(STR.POS) THEN GO 4095
        STR.LINE=COUNT(REC[RPOS,STR.POS],AM)      ;!+(SSTR<1>='')
    END
    FG_ACT.CODE=FALSE
    IF SSTR<1>='' THEN DEL SSTR<1> ELSE DEL SSTR<2>
    OLD.OFFSET=OFFSET
    IF RSEARCH THEN STRT=1
    NLINE=STRT+STR.LINE-INDROW
    NLINE = NLINE>(PDEPTH-2) OR NLINE<1
    IF NLINE OR NOT(REPEATSEARCH) THEN
        SCR.LR=1
        I=INDROW
        IF NLINE THEN
            INDROW=STRT
            IF STR.LINE THEN
                INDROW += STR.LINE-1
                STR.LINE=1
            END
            STRT=INDROW
        END
        IF I NE INDROW THEN
            LCOL=1
            DUMMY=INDROW+(PDEPTH-1)
            FOR J=INDROW TO DUMMY
                RDSP(J-INDROW+1)=REC<J>
            NEXT J
        END
    END
    STR.LINE+=(STRT-INDROW+1)
4095 !
    IF NOT(STR.POS) THEN
        IF RSEARCH THEN
            RPOS=LAST.AM
            WORD='bottom'
        END ELSE
            RPOS=1
            WORD='top'
        END
        IF STRT=RPOS THEN
            CRT MSG.CLR:"String not found!":MSG.AKN: BELL:; RQM
            INDROW=SAVE.INDROW
            DUMMY=INDROW+(PDEPTH-2)
            FOR J=INDROW TO DUMMY
                RDSP(J-INDROW+1)=REC<J>
            NEXT J
            ROW=SAVEROW
        END ELSE
            CRT MSG.CLR:"Wrapping to ":WORD:" of record!":MSG.AKN: BELL:; RQM
            INDROW=1
            DUMMY=INDROW+(PDEPTH-2)
            FOR J=INDROW TO DUMMY
                RDSP(J-INDROW+1)=REC<J>
            NEXT J
            J += am_start
            IF RSEARCH THEN
                STRT=LAST.AM; LCOL=LEN(RDSP(J-1))
                RPOS=LAST.AM-PDEPTH-2
            END ELSE
                STRT=1; LCOL=0          ;!RPOS
            END
            LROW=1; ROW=0
            SCR.LR=1
            GO RETRY
        END
    END ELSE
        IF STR.LINE=LROW THEN
            MREC=RDSP(STR.LINE)[LCOL+1,MAX]
            COL=LCOL
        END ELSE COL=0; MREC=RDSP(STR.LINE); ROW=STR.LINE-1
        CALL EB_FIND(STR.POS,WHOLE.WORDS:'')
        IF NOT(STR.POS) THEN
            LCOL=0
            COL=0
            STR.LINE++
            ROW++
            MREC=RDSP(STR.LINE)
            CALL EB_FIND(STR.POS,WHOLE.WORDS:'')
        END
        LCOL=STR.POS+COL
        SCOL = COL
        CALL EB_TABCOL(RDSP(STR.LINE),COL,LCOL,FALSE)
        IF COL GE (SPWIDTH-3) THEN
            ADJUST = COL-SCOL - 5
        END ELSE
            ADJUST = 0-OFFSET
        END
        OFFSET += ADJUST
        COL -= ADJUST
        LCOL -= ADJUST
        IF LCOL < 1 THEN
            ADJUST = 1 - LCOL
            LCOL += ADJUST
            COL += ADJUST
        END
        IF OFFSET AND COL<SPWIDTH OR OFFSET NE OLD.OFFSET THEN SCR.LR=1
        IF SCR.LR=1 THEN
            CALL EB_REFRESH
            SCR.LR=0
            IF 0 THEN
                DUMMY=INDROW+(PDEPTH-2)
                FOR J=INDROW TO DUMMY
                    Y=RDSP(J-INDROW+1)[1+OFFSET,SPWIDTH-4]
                    IF TAB.MODE THEN CALL EB_TABS(Y,SPWIDTH,0,0)
                    Y=OCONV(Y[1+OFFSET,SPWIDTH-4],'MCP')
                    IF REGEX.SEARCH THEN
                        LINE.POS=EB_REGEX(Y,SSTR, @FALSE)
                    END ELSE
                        LINE.POS=INDEX(Y,SSTR,1)
                    END
                    IF LINE.POS THEN
                        IF WHOLE.WORDS[1,1] THEN
                            OCC=2
                            LOOP
                                CHR1=Y[LINE.POS-1,1]
                                L=LINE.POS+LEN(SSTR)
                                CHR2=Y[L,1]
                                IF INDEX(DELIMS,CHR1,1) AND INDEX(DELIMS,CHR2,1) THEN
                                    Y=Y[1,LINE.POS-1]:BG:SSTR:FG:Y[L,MAX]
                                END
                                LINE.POS=INDEX(Y,SSTR,OCC)
                            WHILE LINE.POS DO
                                OCC++
                            REPEAT
                        END ELSE
                            Y=SWAP(Y,SSTR,BG:SSTR:FG)
                        END
                    END
                    IF J > LAST.AM THEN
                        DIMON = BG
                        DIMOFF = FG
                    END ELSE DIMON = ''; DIMOFF = ''
                    CRT @(0,J-INDROW):CLEOL:DIMON:J"R#4":DIMOFF:" ":;!Y:
                    CRTLN=Y; GOSUB CRT.LN
                NEXT J
            END
        END
        LROW=STR.LINE
    END
4096 !
    CRT MSG.DSP:
    PSSTR=SRCH.STR:AM:OPTIONS
    MREC=""; SSTR=""
4099 !
    PWIDTH = SPWIDTH
    RETURN
!========
4100 ! Display all occurrences of a string.
    FOUND=0
    OCC=1; PGE=1
    HLIM=0
    BEGIN CASE
        CASE INDEX(OPTIONS,'+',1); LLIM=INDROW
        CASE INDEX(OPTIONS,'-',1); LLIM=1; HLIM=INDEX(REC,AM,INDROW+1)-1
        CASE 1; LLIM=1
    END CASE
    IF LLIM=1 THEN STRT=1 ELSE STRT=INDEX(REC,AM,LLIM-1)+1
    IF HLIM THEN MAX=HLIM
4110 !
    EPOS = MAX
    SSSTR = SSTR
    LOOP
        SPOS = STRT
        GOSUB SETMREC
        MREC=REC[STRT,MAX]
        SSTR = SSSTR
        CALL EB_FIND(STR.POS,WHOLE.WORDS:'')
    WHILE STR.POS AND (OCC/PGE<=(PDEPTH-2)) DO
        STR.LINE=DCOUNT(REC[1,STR.POS+STRT-1],AM)
        MREC=TRIM(REC<STR.LINE>)
        IF NOT(FOUND) THEN CRT @(0,0):CLEOP:; FOUND=1
        IF STR.LINE > LAST.AM THEN
            DIMON = BG
            DIMOFF = FG
        END ELSE DIMON = ''; DIMOFF = ''
        CALL EB_FIND(LN.POS,WHOLE.WORDS:'')
        MIDWAY = INT(SPWIDTH/2)
        IF LN.POS > MIDWAY THEN
            LN.POS -= MIDWAY
            IF LN.POS < 1 THEN LN.POS = 1
            MREC = '...':TRIM(MREC[LN.POS, MAX])
        END
        CRT DIMON:STR.LINE "R#4":DIMOFF:" ":OCONV(MREC[1,SPWIDTH-5],'MCP')
        CALL EB_FIND(LPOS,WHOLE.WORDS:'')
        DIFF=STR.POS-LPOS+LEN(MREC)
        STRT+=DIFF
        MAX-=DIFF
        OCC+=1
    REPEAT
    IF FOUND THEN CRT ELSE GO 4099
4120 !
    RefreshRequired=TRUE
    IF NOT(STR.POS) THEN
        CRT MSG.CLR:"That's all!  ":PR:" for original page, or line number? ":
    END ELSE
        CRT MSG.CLR:PR:" for next page, or line number? ":
    END
    LOOP
        L=6; Z=""; INPTYPE='N0'
        GOSUB INPT
    WHILE FG_TIMEDOUT DO
        FG_TIMEDOUT = FALSE
    REPEAT
    Y=Z
    CRT MSG.DSP:
    IF Y=ESC THEN GO 4199
    IF NOT(Y MATCHES "1N0N") THEN
        IF STR.POS THEN PGE+=1; GO 4110 ELSE GO 4199
    END
    IF Y<1 THEN Y=1
    INDROW=Y; ROW=0; COL=5
4199 !
    IF RefreshRequired THEN
        SCR.UD=1
        CALL EB_REFRESH       ;! INDROW=-INDROW
    END
    SSTR=''
    GO 4099
INPT: !
    POS=1
    EDITED=FALSE
    CALL EB_UT_WP(Z,INPTYPE,L,1,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',ESC)
    INPTYPE='AN'
    RETURN
FORMAT: !
    CALL EB_FORMAT(DUMMY,I,LNM)
    RETURN
6000 ! Incorporate changed lines into dynamic array, REC.
    FOR I=1 TO PDEPTH
        IF CHANGES(I) THEN
            CALL EB_TRIM(RDSP(I),RDSP(I):'',' ','T')
            REC<I+INDROW-1>=RDSP(I)
        END
    NEXT I
    CHANGED=FALSE; MAT CHANGES=FALSE
    RETURN
SETMREC:
    MREC=REC[RPOS,EPOS]
    IF CASE.INSENSITIVE THEN MREC = UPCASE(MREC)
    RETURN
CRT.LN: !
    IF LEN(colors<1,1>) EQ 0 THEN
        CRT CRTLN:
        RETURN
    END
    CRTLN = CHANGE(CRTLN, BG, '_bg_')
    CRTLN = CHANGE(CRTLN, FG, '_fg_')
    CRTLN = LOWER(lexLine(RDSP(J-INDROW+1),CRTLN,colors) )
    tokens = RAISE(RAISE(CRTLN<1,2>))
    sitokenCounte = 0
    FOR tokenCount = 1 TO DCOUNT(tokens,@FM)
        io = tokens<tokenCount,3>:tokens<tokenCount,1>
        io = CHANGE(io, '_bg_', RVON)
        io = CHANGE(io, '_fg_', RVOFF)
        CRT io:
        sitokenCounte += tokens<tokenCount,4>
    NEXT tokenCount
    CRT WHITE<1,1>:FG:
    RETURN
DisplayPrompt: !
    CRT MSG.CLR:MSG:
    RETURN
