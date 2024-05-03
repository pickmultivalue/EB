    SUBROUTINE EB_CUT(G60)
    INCLUDE EB.EQUS EB.COMMON
    GO MAIN$
    INCLUDE JBC.h
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS SCREEN.PARAMS
    DEFFUN EBGETHOME()
    EQU SDEL TO CHAR(250)     ;* Delete cut item
    EQU MAX TO 999999
MAIN$:!
    G60=FALSE
    INPTYPE='AN'
    IF FG_ACT.CODE=FG_CUT.CODE OR FG_ACT.CODE=FG_SEL.CODE THEN CHR=SDEL
    IF CHR=SDEL THEN
        IF FG_ACT.CODE=FG_CUT.CODE AND CUT.POS='' THEN
            INS RDSP(LROW)[LCOL,MAX] BEFORE DEL.LIST<1>
            RDSP(LROW)=(RDSP(LROW)[1,LCOL-1])
            LLEN=LEN(RDSP(LROW))
            CRT CLEOL:
            GOSUB CHG.LROW
            NO.D.L=0
        END ELSE
            IF LEN(CUT.POS) THEN
                SECOND.CUT=INDROW+ROW
                IF (SECOND.CUT:SVM:LCOL)=CUT.POS THEN
                    SECOND.CUT+=1
                    ROW+=1
                    LCOL=1
                    CUT.POS<1,1,2>=1
                END
                SECOND.CUT<1,1,2>=LCOL
            END
            CRT @(COL-1+(CUT.POS NE ''),ROW):BG:
            IF CUT.POS='' THEN CRT '[': ELSE CRT ']':
            CRT FG:
            IF CUT.POS='' THEN
                CUT.POS=INDROW+ROW:SVM:LCOL
                G60=TRUE
                RETURN
            END
            SAVE.ACT=FG_ACT.CODE
            LOOP
                CRT MSG.CLR:"Enter Paste Name or Number (F1) ":
                L=20; Z=''
                GOSUB INPT
            WHILE FG_ACT.CODE NE FG_JMP.CODE AND INDEX(0,Z,1) DO
                IF FG_ACT.CODE = FG_HLP.CODE THEN
                    CALL EB_HELP('EBCUT', @FALSE)
                END
            REPEAT
            IF FG_ACT.CODE EQ FG_JMP.CODE THEN Z = '^^'
            IF INDEX(ESC,Z,1) THEN
                Y=CUT.POS<1,1,2>-OFFSET
                CUT.POS=CUT.POS<1,1,1>-OFFSET
                CUT.POS-=INDROW
                IF CUT.POS>=0 AND CUT.POS<=(PDEPTH-1) THEN
                    CRTLN=REC<CUT.POS+INDROW>[Y,1]
                    CONVERT VM:SVM TO ']\' IN CRTLN
                    CRT @(Y+4,CUT.POS):OCONV(CRTLN,'MCP'):
                END
                CUT.POS=''; G60=TRUE; RETURN
            END
            IF CHANGED THEN GOSUB 6000
            FG_ACT.CODE=SAVE.ACT
            IF SECOND.CUT<1,1,1><CUT.POS<1,1,1> THEN
                INDROW=CUT.POS<1,1,1>+1; ROW=0
                INS SECOND.CUT BEFORE CUT.POS<1>
            END ELSE
                ROW=CUT.POS<1,1,1>-INDROW
                CUT.POS<2>=SECOND.CUT
            END
            NO.D.L=CUT.POS<2,1,1>-CUT.POS<1,1,1>+1
        END
    END ELSE
        NO.D.L=1; Z=''
        IF CHANGED THEN GOSUB 6000
    END
    IF NO.D.L THEN
        CUT.TEXT=""
        SCR.UD=(DEL.LINE NE '' AND (ROW+NO.D.L)<=(PDEPTH-1) AND CHR=SDEL AND ROW>=0 AND CUT.POS<1,1,1><=CUT.POS<2,1,1> OR CHR NE SDEL)
        Y=INDROW
        IF Z='!' THEN
            FG_ACT.CODE=FG_SEL.CODE
            ZFLAG=REC<INDROW+ROW>[1,COMMENTLEN] NE COMMENT<1,1,1>
        END
        NEW.D.L = NO.D.L
        IF DOWNCASE(Z) EQ '!v' THEN
            sline = INDROW+ROW
            IF GETCWD(currdir) AND CHDIR(FLNM) THEN
                EXECUTE 'git log -L':sline:',':sline+NO.D.L-1:':':ITNM
                rc = CHDIR(currdir)
            END ELSE
                CRT @(-1):FLNM:' is not part of a git repository'
            END
            CRT MSG.CLR:"Press <enter>...":
            L=0; Z=''
            GOSUB INPT
            SCR.LR=1
            CALL EB_REFRESH
            RETURN
        END
        IF Z [1,1] EQ '^' THEN
            ROTATE = ''
            INDROW--
            FOR J=1 TO NO.D.L
                ROTATE<J> = REC<INDROW+ROW+J>
            NEXT J
            IF Z EQ '^' THEN
                CALL EB_ROTATE(ROTATE, NEW.D.L)
            END ELSE
                IF UPCASE(Z[2,1]) EQ 'S' THEN ;!
                    SORTED = ''
                    SORTBY = Z[3,2]
                    IF SORTBY = '' THEN SORTBY = 'A'
                    IF LEN(SORTBY) EQ 1 THEN SORTBY := 'L'
                    FOR J = 1 TO NO.D.L
                        Y = ROTATE<J>
                        LOCATE Y IN SORTED BY SORTBY SETTING POS ELSE
                            INS Y BEFORE SORTED<POS>
                        END
                    NEXT J
                    ROTATE = SORTED
                    Y = ''
                    Z = '^^'
                END ELSE
                    path = EBGETHOME()
                    Y ='.PASTE*':FG_LOGNAME:'*'
                    WRITE ROTATE ON JET.PASTE,Y
                    EXECUTE 'EB ':path:'JET.PASTE ':Y
                    READ ROTATE FROM JET.PASTE,Y
                END
                FOR J=1 TO NO.D.L
                    REC<INDROW+ROW+J> = ROTATE<J>
                NEXT J
                IF LEN(Y) THEN
                    DELETE JET.PASTE,Y
                END
            END
            INDROW++
            IF Z EQ '^^' THEN
                SCR.UD = TRUE
                CHANGED = TRUE
                CUT.POS = ''
                RETURN
            END
        END
        TABLEN=ITAB<1,1>
        MODIFY=INDEX('!<>^',Z,1) AND LEN(Z)
        IF MODIFY AND NEW.D.L NE NO.D.L THEN
            IF NEW.D.L GT NO.D.L THEN
                NL = NEW.D.L - 1
                FOR J = NO.D.L TO NL
                    INS '' BEFORE REC<INDROW+ROW>
                NEXT J
            END ELSE
                NL = NO.D.L - 1
                FOR J = NEW.D.L TO NL
                    DEL REC<INDROW+ROW>
                NEXT J
            END
            NO.D.L = NEW.D.L
        END
        FOR J=1 TO NO.D.L
            IF MODIFY THEN
                LINE=REC<INDROW+ROW>
                BEGIN CASE
                    CASE Z EQ '^'
                        LINE=ROTATE<J>
                    CASE Z='!' ;  ;! comment out lines
                        IF ZFLAG THEN
                            LINE=COMMENT<1,1,1>:LINE:COMMENT<1,1,2>
                        END ELSE
                            IF LINE[1,COMMENTLEN]=COMMENT<1,1,1> THEN
                                LINE=LINE[COMMENTLEN+1,MAX]
                                IF LINE[LEN(LINE)-1,COMMENTLEN]=COMMENT<1,1,2> THEN
                                    LINE=LINE[1,LEN(LINE)-COMMENTLEN]
                                END
                            END
                        END
                    CASE Z='<'    ;! unindent
                        IF LINE[1,1]=TAB THEN
                            LINE=LINE[2,MAX]
                        END ELSE
                            IF LINE[1,TABLEN]=SPACE(TABLEN) THEN
                                LINE=LINE[TABLEN+1,MAX]
                            END
                        END
                    CASE Z='>'    ;! indent
                        IF LINE[1,1]=TAB THEN
                            LINE=TAB:LINE
                        END ELSE
                            IF LINE[1,TABLEN]=SPACE(TABLEN) THEN
                                LINE=SPACE(TABLEN):LINE
                            END
                        END
                END CASE
                REC<INDROW+ROW>=LINE
                INDROW+=1
            END ELSE
                CUT.TEXT<-1>=REC<INDROW+ROW>
                IF FG_ACT.CODE NE FG_SEL.CODE THEN   ;! Cut
                    IF SCR.UD AND (NOT(CHR=SDEL) OR J NE NO.D.L) THEN CRT @(0,ROW):DEL.LINE:
                    DEL REC<INDROW+ROW>
                    CALL EB_MARKADJ(INDROW+ROW,1,-1)
                    CHANGED=TRUE
                END ELSE INDROW+=1
            END
        NEXT J
        IF Z NE '!' THEN
            IF CHR=SDEL THEN
                IF NUM(Z) THEN Z='PASTE*':FG_LOGNAME:'*':Z
                PASTE.TEXT=CUT.TEXT<1>[1,CUT.POS<1,1,2>-1]:CUT.TEXT<NO.D.L>[CUT.POS<2,1,2>,MAX]
                IF FG_ACT.CODE=FG_CUT.CODE THEN
                    INS PASTE.TEXT BEFORE REC<INDROW+ROW>
                    CALL EB_MARKADJ(INDROW+ROW,DCOUNT(PASTE.TEXT,AM),1)
                    LCOL=CUT.POS<1,1,2>
                    COL=LCOL+4-OFFSET
                    IF COL GT 0 THEN
                        CRTLN=PASTE.TEXT[LCOL,PWIDTH-COL]
                        CRT @(COL,ROW):
                        CHR1=PASTE.TEXT[1,COMMENTLEN]
                        IF CHR1=COMMENT<1,1,1> THEN CRT BG:
                        CONVERT VM:SVM TO ']\' IN CRTLN
                        CRT OCONV(CRTLN,'MCP'):CLEOL:@(COL,ROW):
                        IF CHR1=COMMENT<1,1,1> THEN CRT FG:
                    END ELSE
                        SCR.LR = 1
                        ADJUST = (PWIDTH-5)
                        OFFSET -= ADJUST
                        COL += ADJUST
                    END
                    CHANGED=TRUE
                END
                IF NO.D.L>1 THEN
                    CUT.TEXT<1>=CUT.TEXT<1>[CUT.POS<1,1,2>,MAX]
                    CUT.TEXT<NO.D.L>=CUT.TEXT<NO.D.L>[1,CUT.POS<2,1,2>-1]
                END ELSE CUT.TEXT=CUT.TEXT[CUT.POS<1,1,2>,CUT.POS<2,1,2>-CUT.POS<1,1,2>]
! hack for case where JET.PASTE is directory
                IF CUT.TEXT[LEN(CUT.TEXT),1] = AM THEN
                    status = ''
                    rc = IOCTL(JET.PASTE,JIOCTL_COMMAND_FILESTATUS,status)
                    IF status<1> = 'UD' THEN
                        CUT.TEXT := AM
                    END
                END
                WRITE CUT.TEXT ON JET.PASTE,Z
            END ELSE
                INS CUT.TEXT BEFORE DEL.LINES<1>
            END
        END ELSE SCR.UD=TRUE; G60=TRUE
        IF FG_ACT.CODE NE FG_SEL.CODE THEN ;! Copy
            IF SCR.UD THEN
                SCR.UD=NO.D.L-2*NO.D.L*(DEL.LINE NE '')
            END ELSE
                IF ROW<0 THEN ROW=0
                SCR.UD=TRUE
            END
            CHANGED=TRUE
            SCRL=ROW
            IF ROW+NO.D.L>PDEPTH THEN
                INDROW-=(NO.D.L+1)
                IF INDROW<1 THEN INDROW=1
            END
        END ELSE INDROW=Y
        CUT.POS=''
    END
    RETURN
CHG.LROW: CHANGED=TRUE; CHANGES(LROW)=TRUE; RETURN
INPT: !
    POS=1
    EDITED=FALSE
    CALL EB_UT_WP(Z,INPTYPE,L,1,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',ESC)
    INPTYPE='AN'
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
