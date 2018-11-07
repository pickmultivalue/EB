    SUBROUTINE EB_PASTE(G60)
    COMMON /EB_LEXER/reservedWords, colors, comments, commentlen, incomment
    INCLUDE EB.INCLUDES lex.h
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
    EQU MAX TO 999999
MAIN$:!
    G60=FALSE
!
    IF CHANGED THEN GOSUB 6000
    INPTYPE='LIT'
    SCOL=''
    IF FG$ACT.CODE=FG$PASTE.CODE THEN
        MSG='Enter Paste Name or 0 for previous deleted lines '
        ICOL=LEN(MSG); IROW=(PDEPTH-1)
        CRT MSG.CLR:MSG:
        L=PWIDTH-1-ICOL; Z=0
        GOSUB INPT
        CRT MSG.CLR:
        IF INDEX(ESC,Z,1) THEN G60=TRUE; RETURN
        SHELL.CMD=FALSE
        SHOW.PASTE=FALSE
        IF Z EQ '^' THEN
            STMP = ''
            CRT MSG.CLR:"Paste in your code now..."
            CRT
            RQM
            LOOP
                L=SYSTEM(14)
                INPUT Z,L
            UNTIL Z EQ ESC DO
                IF LEN(Z) THEN
                    STMP<-1> = Z
                END ELSE STMP := @AM
                IF NOT(SYSTEM(14)) THEN
                    RQM
                    IF NOT(SYSTEM(14)) THEN BREAK
                END
            REPEAT
        END ELSE
            IF Z[1,1]='!' THEN
                SHELL.CMD=TRUE
                Z=Z[2,MAX]
            END
            IF Z[1,1]='?' THEN
                SHOW.PASTE=TRUE
                Z=Z[2,MAX]
            END
            IF SHELL.CMD THEN
                shell = @IM:'k'
                shellend = ' 2>&1'
                EXECUTE shell:Z:shellend CAPTURING STMP
            END ELSE
                IF INDEX(0,Z,1) THEN
                    IF DCOUNT(DEL.LINES,AM) THEN
                        STMP=DEL.LINES<1>:AM
                        IF NOT(SHOW.PASTE) THEN DEL DEL.LINES<1>
                    END ELSE STMP=''
                END ELSE
                    IF NUM(Z) THEN Z='PASTE*':FG$LOGNAME:'*':Z
                    READ STMP FROM JET.PASTE,Z ELSE STMP=''
                END
            END
        END
        IF NOT(LEN(STMP)) THEN G60=TRUE; RETURN
        NO.I.L=DCOUNT(STMP,AM)
        IF SHOW.PASTE THEN
            X=1
            LOOP
                Y=X+PDEPTH-2
                FOR K=X TO Y
                    CRT @(0,K-X):CLEOL:K"R#4":" ":
                    CRTLN=STMP<K>;CRT.X=1;CRT.Y=PWIDTH-4; GOSUB CRT.LN
                NEXT K
                CRT MSG.CLR:"Press <return>":
                L=30; Z=''
                GOSUB INPT          ;! input the field
                IF INDEX(ESC,Z,1) THEN X=NO.I.L+1 ELSE X=K
            UNTIL X>NO.I.L DO REPEAT
            SCR.UD=TRUE; RETURN
        END
        PASTE=1
        IF STMP<NO.I.L>='' THEN LCOL=1; COL=5   ;! always paste whole lines as whole
    END ELSE
        PASTE=0; NO.I.L=1; INS.MODE=TRUE; STMP=''
        SCOL=0
    END
    SCR.UD = TRUE
!    IF PASTE AND LROW+NO.I.L>(PDEPTH-1) THEN SCR.UD=TRUE ELSE
!        IF INS.LINE='' THEN SCR.UD=TRUE ELSE
!            SCR.UD=-1
!            IF NOT(PASTE) THEN CRT @(0,ROW):INS.LINE: ELSE CRT @(0,ROW):DEL.LINE:
!        END
!    END
    IF NO.I.L>0 THEN
        I=LROW+(LROW<3)
        LOOP WHILE I>2 AND RDSP(I-1)[1,COMMENTLEN]=COMMENT DO I-=1 REPEAT
        IF I>1 THEN CHECK.LINE=RDSP(I-1) ELSE CHECK.LINE=REC<INDROW-1>
        LLEN=LEN(CHECK.LINE)
        IDENT=CHECK.LINE; LNM=1; GOSUB FORMAT
        IF SCOL='' THEN SCOL=LCOL
        TABLEN=ITAB<ITABPOS>
        IF PASTE THEN
! first merge current line into paste
            CHECK.LINE=REC<INDROW+ROW>
            STMP=CHECK.LINE[1,LCOL-1]:STMP:CHECK.LINE[LCOL,MAX]
            DEL REC<INDROW+ROW>
            IF I THEN
                IDENT=STMP<1>
                CALL EB_TABS(IDENT,PWIDTH,0,0)
                IF FIELD(TRIM(IDENT),' ',1)='}' OR FIELD(TRIM(IDENT),' ',1)='END' OR FIELD(TRIM(IDENT),' ',1)='CASE' THEN I-=TABLEN
                LCOL=I; SCRL=ROW
                J=1
                LOOP WHILE IDENT[J,1]=' ' AND IDENT[J,1]#'' DO J+=1 REPEAT
                I-=J
                IDENT=ABS(I)
            END ELSE IDENT=0; I=-1
        END ELSE LCOL=I; SCRL=ROW; IDENT=ABS(I)
        IF TAB.MODE THEN TABCH=TAB ELSE TABCH=SPACE(TABLEN)
        IDENT=STR(TABCH,INT(IDENT/TABLEN))
!
        FOR J=NO.I.L TO 1 STEP -1
            IF PASTE THEN
                CHECK.LINE=STMP<J>
                IF CHECK.LINE[1,COMMENTLEN]=COMMENT OR INDEX(CHECK.LINE,':!',1) OR CHECK.LINE[1,1]='#' ELSE
                    IF J#NO.I.L THEN
                        IF I<0 THEN
                            CALL EB_EREPLACE(CHECK.LINE,IDENT,'',1,1)
                        END ELSE
                            CHECK.LINE=IDENT:CHECK.LINE
                        END
                    END
                END
                INS CHECK.LINE BEFORE REC<INDROW+ROW>
                IF SCR.UD=-1 THEN
                    CRT @(0,ROW):INS.LINE:@(5,ROW):
                    CRTLN=CHECK.LINE;CRT.X=1;CRT.Y=PWIDTH-4
                    GOSUB CRT.LN
                END
            END ELSE INS IDENT BEFORE REC<INDROW+ROW>
            CHECK.LINE=REC<INDROW+ROW>
        NEXT J
        CALL EB_MARKADJ(INDROW+ROW,NO.I.L,1)
        IF SCOL THEN
            LCOL=SCOL
            CALL EB_TABCOL(CHECK.LINE,COL,LCOL,FALSE)
        END
    END
    CHANGED=TRUE
    RETURN
!
INPT: !
    POS=1
    EDITED=FALSE
    CALL EB_UT_WP(Z,INPTYPE,L,1,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',ESC)
    IF FG$ACT.CODE = FG$OPT.CODE THEN
        FG$ACT.CODE=FALSE
        CALL EB_CHOICES(20,3,'',10,'JET.PASTE','',Z,1,1,0:SVM:1,'L#20':SVM:'L#40':CTRL.C:'MCP','Paste items':SVM:'Item')
    END
    INPTYPE='AN'
    RETURN
    INCLUDE EB.INCLUDES CRT.LN
FORMAT: !
    CALL EB_FORMAT(IDENT,I,LNM)
    RETURN
6000 ! Incorporate changed lines into dynamic array, REC.
    CHANGES(LROW)=TRUE
    FOR I=1 TO PDEPTH
        IF CHANGES(I) THEN
            CALL EB_TRIM(RDSP(I),RDSP(I):'',' ','T')
            REC<I+INDROW-1>=RDSP(I)
        END
    NEXT I
    CHANGED=FALSE; MAT CHANGES=FALSE
    RETURN
