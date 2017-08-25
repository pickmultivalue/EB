    SUBROUTINE EB_SEARCH
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(100),EB.FILE.LIST
    COM RDSP(100), CHANGES(100)
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS SCREEN.PARAMS
    MAIN$:!
    MAX=LEN(REC)
    LAST.AM=DCOUNT(REC,AM)
    PR="--Press <RETURN>"
    RefreshRequired=FALSE
    DELIMS=' ():;+-*/,&!^#=<>[]@{}':AM:VM:SVM:TAB
!
    MSG='String (A{+-}/for all;V/vars;C/char) '
    ICOL=LEN(MSG); IROW=(PDEPTH-1)
    Indent=ITAB<ITABPOS>
    CRT MSG.CLR:MSG:
    SAVEROW=ROW
    L=PWIDTH-1-ICOL; SSTR=''; SRCH.STR=''
    OPTIONS=''
    RPOS=1
    IF FG$ACT.CODE=FG$MULTI.CODE THEN FG$ACT.CODE=FALSE ELSE FG$LAST.ACT.CODE=FG$ACT.CODE
    RSEARCH=(FG$LAST.ACT.CODE=FG$BSEARCH.CODE)
    IF FG$ACT.CODE THEN
        CONVERT AM TO VM IN SSS
        LOOP
            CRT @(ICOL,IROW):
            IF RPOS AND RPOS<21 THEN Z=SSS<1,RPOS>
            INPTYPE='LIT'; GOSUB INPT
            LOCATE FG$ACT.CODE IN CYCLES<am_start> SETTING POS ELSE POS=FALSE
        WHILE POS DO
            BEGIN CASE
            CASE FG$ACT.CODE=FG$OPT.CODE
                CALL EB_CHOICES(50,8,31,10,'',SSS,Z,1,RPOS,1,'L#30','Previous Searches')
            CASE FG$ACT.CODE=FG$SKP.CODE OR FG$ACT.CODE=FG$MULTI.CODE; RPOS+=1
            CASE 1; RPOS-=1
            END CASE
            IF RPOS<1 THEN RPOS=0
            FG$ACT.CODE=FALSE
        REPEAT
        CONVERT VM TO AM IN SSS
    END ELSE
        IF COUNT(PSSTR,AM) THEN PSSTR = PSSTR<2>:'/':PSSTR<1>
        Z=PSSTR
        IF LEN(Z)=0 THEN Z=SSS<RPOS>
    END
    IF FG$ACT.CODE=FG$SEARCH.CODE THEN FG$ACT.CODE=FALSE
    IF FG$ACT.CODE THEN GO 4099
    LOCATE Z IN SSS<am_start> SETTING POS ELSE
        IF POS<20 THEN POS=0 ELSE POS=20
    END
    IF POS THEN DEL SSS<POS>
    INS Z BEFORE SSS<1>
    CRT MSG.AKN:
    STR.POS=0
    SSTR=Z
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
    IF SSTR='//' OR SSTR='\\' THEN
        PSSTR=SSTR
        IF SSTR='//' THEN LNM=1 ELSE LNM=-1
        DUMMY=RDSP(LROW); GOSUB FORMAT
        SPOS=INDEX(REC,AM,INDROW+ROW)
        curlies = 0
        IF DUMMY='' THEN GO 4099 ELSE
            STRT=INDROW+ROW+LNM
            IF DUMMY[1,3]#'!#!' THEN
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
            CALL EB_TABS(LINE,PWIDTH)
            LOOP
                done = (STRT=1 AND LNM<0)
                IF curlies THEN
                    curlies += COUNT(LINE,RSTR) - COUNT(LINE, SSTR)
                    done += (curlies < 1)
                END ELSE
                    done += (NOT(TRIM(LINE)[1,COMMENTLEN]=COMMENT) AND LINE[1,Y] MATCHES SSTR AND TRIM(LINE[1,Y])#'')
                END
                done += (LNM>=LAST.AM)
            UNTIL done DO
                STRT+=LNM
                INCLUDE EB.OS.INCLUDES REMOVE.LINE
                CALL EB_TABS(LINE,PWIDTH)
                IF DUMMY[1,3]#'!#!' THEN
                    IF INDEX(COMMENT,TRIM(LINE)[1,COMMENTLEN],1) THEN LINE=COMMENT
                END
            REPEAT
            IF LINE#'' THEN
                IF NOT(INDEX('{}',SSTR,1)) THEN
                    IF DUMMY[1,3]#'!#!' THEN
                        SSTR=FIELD(LINE[Y,9],' ',1); LINE=OCONV(LINE[Y,20],'G 2')
                    END ELSE SSTR=FIELD(LINE,' ',1)
                    LOCATE SSTR IN DUMMY<am_start> SETTING POS ELSE
                        LOCATE LINE IN DUMMY<am_start> SETTING POS ELSE
                            DUMMY=SWAP(DUMMY,AM,' or ')
                            CRT @(0,(PDEPTH-1)):BELL:CLEOL:'Expected ':DUMMY[1,PWIDTH-14]:'...':; RQM
                        END
                    END
                END
                IF STRT<INDROW OR STRT>(INDROW+(PDEPTH-1)) THEN
                    IF CHANGED THEN GOSUB 6000
                    INDROW=STRT; SCR.UD=TRUE; OFFSET=0; COL=5; ROW=0
                END ELSE ROW=STRT-INDROW; COL=I+Indent
                SCR.UD=1
                CALL EB_REFRESH
                RETURN
            END ELSE GO 4099
        END
    END
    IF SSTR=ESC THEN GO 4099
    IF SSTR="" AND PSSTR="" THEN GO 4099
    REPEATSEARCH = (SSTR=PSSTR)
    OPTIONS=FIELD(SSTR,'/',1)
    SSTR=SSTR[COL2()+1,MAX]
    IF SSTR='' THEN SSTR=OPTIONS; OPTIONS='' ELSE OPTIONS=OCONV(OPTIONS,'MCU')
    WHOLE.WORDS=INDEX(OPTIONS,'V',1)
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
    IF OPTIONS#'' THEN PSSTR=OPTIONS:'/':SSTR
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
    IF RSEARCH THEN FG$ACT.CODE=FG$BSEARCH.CODE
    IF STRT>1 THEN
        IF STRT>LAST.AM THEN
            STR.POS = FALSE
        END ELSE
            CALL EB_TRIM(TMP,RDSP(LROW)[1,LCOL],' ','T')
            LINE.POS=INDEX(REC,AM,STRT-1)+LEN(TMP)+2
            IF FG$ACT.CODE=FG$BSEARCH.CODE THEN
                RPOS=1; EPOS=LINE.POS
            END ELSE
                RPOS=LINE.POS; EPOS=MAX
            END
            MREC=REC[RPOS,EPOS]
            CALL EB_FIND(STR.POS,WHOLE.WORDS)
        END
        IF STR.POS ELSE GO 4095
        STR.LINE=COUNT(MREC[1,STR.POS],AM)
    END ELSE
        IF FG$ACT.CODE=FG$BSEARCH.CODE THEN
            RPOS=1; EPOS=LCOL
        END ELSE
            RPOS=LCOL+1; EPOS=MAX
        END
        MREC=REC[RPOS,EPOS]
        CALL EB_FIND(STR.POS,WHOLE.WORDS)
        IF NOT(STR.POS) THEN GO 4095
        STR.LINE=COUNT(REC[RPOS,STR.POS],AM)      ;!+(SSTR<1>='')
    END
    FG$ACT.CODE=FALSE
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
        IF I#INDROW THEN
            LCOL=1
            DUMMY=INDROW+(PDEPTH-1)
            FOR J=INDROW TO DUMMY
                RDSP(J-INDROW+1)=REC<J>
            NEXT J
        END
    END
    STR.LINE+=(STRT-INDROW+1)
4095      !
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
        CALL EB_FIND(STR.POS,WHOLE.WORDS)
        IF NOT(STR.POS) THEN
            LCOL=0
            COL=0
            STR.LINE++
            ROW++
            MREC=RDSP(STR.LINE)
            CALL EB_FIND(STR.POS,WHOLE.WORDS)
        END
        LCOL=STR.POS+COL
        CALL EB_TABCOL(RDSP(STR.LINE),COL,LCOL,FALSE)
        IF OFFSET AND LCOL<PWIDTH OR OFFSET#OLD.OFFSET THEN SCR.LR=1
        IF SCR.LR=1 THEN
            SCR.LR=0
            DUMMY=INDROW+(PDEPTH-2)
            FOR J=INDROW TO DUMMY
                Y=RDSP(J-INDROW+1)[1+OFFSET,PWIDTH-4]
                IF TAB.MODE THEN CALL EB_TABS(Y,PWIDTH)
                Y=OCONV(Y[1+OFFSET,PWIDTH-4],'MCP')
                LINE.POS=INDEX(Y,SSTR,1)
                IF LINE.POS THEN
                    IF WHOLE.WORDS THEN
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
                CRT @(0,J-INDROW):CLEOL:DIMON:J"R#4":DIMOFF:" ":Y:
            NEXT J
        END
        LROW=STR.LINE
    END
4099      !
    CRT MSG.DSP:
    PSSTR=SRCH.STR:AM:OPTIONS
    MREC=""; SSTR=""
    RETURN
!========
4100      ! Display all occurrences of a string.
    IF NOT(INDEX(REC,SSTR,1)) THEN GO 4099
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
4110      !
    LOOP
        MREC=REC[STRT,MAX]
        CALL EB_FIND(STR.POS,WHOLE.WORDS)
    WHILE STR.POS AND (OCC/PGE<=(PDEPTH-2)) DO
        STR.LINE=DCOUNT(REC[1,STR.POS+STRT-1],AM)
        MREC=REC<STR.LINE>
        IF NOT(FOUND) THEN CRT @(0,0):CLEOP:; FOUND=1
        IF STR.LINE > LAST.AM THEN
            DIMON = BG
            DIMOFF = FG
        END ELSE DIMON = ''; DIMOFF = ''
        CRT DIMON:STR.LINE "R#4":DIMOFF:" ":OCONV(MREC[1,PWIDTH-5],'MCP')
        CALL EB_FIND(LPOS,WHOLE.WORDS)
        DIFF=STR.POS-LPOS+LEN(MREC)
        STRT+=DIFF
        MAX-=DIFF
        OCC+=1
    REPEAT
    IF FOUND THEN CRT ELSE RETURN
4120      !
    RefreshRequired=TRUE
    IF NOT(STR.POS) THEN
        CRT MSG.CLR:"That's all!  ":PR:" for original page, or line number? ":
    END ELSE
        CRT MSG.CLR:PR:" for next page, or line number? ":
    END
    L=6; Z=""; INPTYPE='N0'
    GOSUB INPT
    Y=Z
    CRT MSG.DSP:
    IF Y="" THEN
        IF STR.POS THEN PGE+=1; GO 4110 ELSE GO 4199
    END
    IF Y=ESC THEN GO 4199
    IF Y<1 THEN Y=1
    INDROW=Y; ROW=0; COL=5
4199      !
    IF RefreshRequired THEN
        SCR.UD=1
        CALL EB_REFRESH       ;! INDROW=-INDROW
    END
    SSTR=''
    RETURN
INPT:     !
    POS=1
    EDITED=FALSE
    CALL EB_UT_WP(Z,INPTYPE,L,1,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',ESC)
    INPTYPE='AN'
    RETURN
FORMAT:   !
    CALL EB_FORMAT(DUMMY,I,LNM)
    RETURN
6000      ! Incorporate changed lines into dynamic array, REC.
    FOR I=1 TO PDEPTH
        IF CHANGES(I) THEN
            CALL EB_TRIM(RDSP(I),RDSP(I):'',' ','T')
            REC<I+INDROW-1>=RDSP(I)
        END
    NEXT I
    CHANGED=FALSE; MAT CHANGES=FALSE
    RETURN
END
