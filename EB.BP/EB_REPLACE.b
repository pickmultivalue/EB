    SUBROUTINE EB_REPLACE(ENDL,STRT,LINE.POS,END.POS,LEN.DIFF,MAT RPL.PARMS,MAT RPL.PROMPTS,MAT RPL.COLS)
    INCLUDE EB.EQUS EB.COMMON
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
!
! This routine is called from EB to handle string replacements.
! The variables RSTR and WSTR are the original search and replace sets.
! Both RSTR and WSTR may contain @n strings throughout (ie @1 @2 etc)
! or simply ... which implies @1 (though it is not wise to do so).
! WSTR may also contain the literal (x) (note: x must be in lower case)
! which will generate automatic sequence numbers in parentheses which
! can be used when creating/modifying Dimensioned array equates.
! The DELIMS equate is used if the Whole Words was selected to ensure
! the string to be replaced is a valid PICK/BASIC variable (not a whole
! word as such).
!
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS SCREEN.PARAMS
!
    EQU ESC TO CHAR(27), TAB TO CHAR(9), TRUE TO 1, FALSE TO 0
    EQU DELIMS TO ' ():;+-*/,&!^#=<>[]@':@VM:@SVM:TAB
    EQU MAX TO 999999
    DEFFUN EB_REGEX()
MAIN$:!
    DIM RPL.PARMS(3),RPL.PROMPTS(3),RPL.COLS(3)
    EQU WHOLE TO RPL.PARMS(1)
    EQU ALOC TO RPL.PARMS(2)
    EQU CONFIRM TO RPL.PARMS(3)
    IF EMBED.ATTR<1,1> THEN HILON=RVOFF; HILOFF=RVON ELSE HILON=BG; HILOFF=FG
    HILRESET=FG:RVOFF
    WHOLE.WORDS=WHOLE
    CASE.INSENSITIVE=WHOLE.WORDS[4,1]
    REGEX.SEARCH=WHOLE.WORDS[3,1]
    NOCOMMENTS=WHOLE.WORDS[2,1]
    WHOLE.WORDS=WHOLE.WORDS[1,1]
!
    TMP=RSTR; GOSUB CONV.CHARS
    RSTRL=LEN(TMP)
    RSTR=SWAP(RSTR,'...','@1')
    WSTR=SWAP(WSTR,'...','@1')
    STR.CNT=0
    OK=1
    OCCURS=''
    FIRST.DISP=TRUE
!
! Initialise counter
!
    SOP=1
    ACNT=''
    LOOP
        POS=INDEX(WSTR,'@',SOP)
        IF POS THEN
            THE.REST=WSTR[POS+1,9]
            ACNT=INDEX(THE.REST,'x',1)
            IF ACNT THEN
                ACNT++
                APOS=ACNT
                LOOP WHILE THE.REST[APOS,1] MATCHES "1N0N" DO APOS++ REPEAT
                ACNT = THE.REST[ACNT,APOS-ACNT]
                WSTR=WSTR[1,POS]:'x':THE.REST[APOS,MAX]
                IF ACNT='' THEN ACNT=1
            END
        END
    UNTIL ACNT OR NOT(POS) DO SOP+=1 REPEAT
    LHASH='L#':PWIDTH-4
!
! Break-up wild-cards and literals
!
    LOOP
        STR.POS=INDEX(RSTR,'@':STR.CNT+1,1)
    WHILE STR.POS DO
        STR.CNT+=1
        RSTR=RSTR[1,STR.POS-1]:@AM:RSTR[STR.POS+2,MAX]
    REPEAT
    USE.THE.REST=(RSTR[LEN(RSTR),1]=@AM)          ;! ie. last wild-card goes to eol
!
! Break-up wild-cards and literals and insert wild-card number where applicable
!
    FOR CNT=1 TO STR.CNT
        LOOP
            STR.POS=INDEX(WSTR,'@':CNT,1)
            STR.POS+=INDEX(WSTR,'@l':CNT,1)
            STR.POS+=INDEX(WSTR,'@u':CNT,1)
            STR.POS+=INDEX(WSTR,'@c':CNT,1)
        WHILE STR.POS DO
            WCNT = CNT
            op = WSTR[STR.POS+1,1]
            IF NOT(NUM(op)) THEN WCNT = op:WCNT
            WSTR=WSTR[1,STR.POS-1]:@AM:WCNT:@VM:WSTR[STR.POS+1+LEN(WCNT),MAX]
        REPEAT
    NEXT CNT
    WSTR.CNT=COUNT(WSTR,@VM)
!
    IF INDEX(RSTR,@AM,1) THEN
        RRSTR = ''
        RSTRL = 0
    END ELSE RRSTR=RSTR
    IF INDEX(WSTR,@AM,1) THEN WWSTR='' ELSE WWSTR=WSTR
!
! Find first literal to be used for searching through text
!
    FIRST=RSTR<1>
    TMP=FIRST; GOSUB CONV.CHARS; FIRST=TMP
    INIT=FIRST
    STR.CNT=STR.CNT+(RSTR<STR.CNT+1> NE '')
    FOR I=1 TO STR.CNT UNTIL INIT NE ''
        INIT=RSTR<I>
        TMP=INIT; GOSUB CONV.CHARS; INIT=TMP
    NEXT I
!
! Start of main search/replace loop
!
    PREV.LINE.POS=0
    PREV.LINE.NO=0
    LOOP
        STR.POS=REC[LINE.POS+1,END.POS-LINE.POS]
        IF INIT NE '' THEN
            IF REGEX.SEARCH THEN
                STR.POS=EB_REGEX(STR.POS,INIT, @FALSE)
            END ELSE
                STR.POS=INDEX(STR.POS,INIT,1)
            END
        END ELSE
            IF LEN(TRIM(STR.POS)) THEN
                STR.POS = @AM:STR.POS
                LINE.NO=LEN(STR.POS)
                FOR I=1 TO LINE.NO WHILE(INDEX(@AM,TRIM(STR.POS[I,1]),1)); NEXT I
                I+=am_start
                STR.POS=I-1
            END ELSE STR.POS=0
        END
    UNTIL NOT(STR.POS) DO
        LINE.NO=COUNT(REC[1,LINE.POS+STR.POS],@AM)+(REC[LINE.POS+STR.POS,1] NE @AM)
        IF LINE.NO>ENDL THEN GO RTN    ;! finished
        THIS.LINE.CHANGED=FALSE
        IF LINE.NO = PREV.LINE.NO AND NOT(ALOC) THEN CONTINUE
        PREV.LINE.NO = LINE.NO
        LINE=REC<LINE.NO>
        LEADWS = ''; TRAILWS = ''
        IF NOT(INDEX(RSTR, TAB, 1)) THEN
            FOR I = 1 TO LEN(LINE)
                CH = LINE[I,1]
                IF INDEX(TAB,CH,1) THEN
                    LEADWS := CH
                END ELSE
                    BREAK
                END
            NEXT I
            LINE = LINE[I,LEN(LINE)]
            FOR I = LEN(LINE) TO 1 STEP -1
                CH = LINE[I,1]
                IF INDEX(TAB,CH,1) THEN
                    TRAILWS := CH
                END ELSE
                    BREAK
                END
            NEXT I
            LINE = LINE[1,I]
        END
        ORIG.LINE=LINE
!
! Check that all matching strings are there
!
        IF STR.CNT THEN
            SLINE=LINE
            OK=TRUE
            FOR CNT=1 TO STR.CNT WHILE OK
                NEW.LINE=RSTR<CNT>
                IF NEW.LINE NE '' THEN
                    TMP=NEW.LINE; GOSUB CONV.CHARS; NEW.LINE=TMP
                    IF REGEX.SEARCH THEN
                        POS=EB_REGEX(SLINE,NEW.LINE, @FALSE)
                    END ELSE
                        POS=INDEX(SLINE,NEW.LINE,1)
                    END
                    IF POS THEN
                        SLINE=SLINE[POS+LEN(NEW.LINE),MAX]
                    END ELSE OK=FALSE
                END
            NEXT CNT
        END
        IF OK THEN
            OCC=0; OCCURS=0
            IF CONFIRM AND ALOC THEN
                CRT @(0,20):CLEOP:
                IF LINE.NO>1 THEN CRT LINE.NO-1 'R%4 ':REC<LINE.NO-1>[1,PWIDTH-4] ELSE CRT
                CRT LINE.NO 'R%4 ':REC<LINE.NO>[1,PWIDTH-4]
                CRT LINE.NO+1 'R%4 ':REC<LINE.NO+1>[1,PWIDTH-4]
                CRT @(0,PDEPTH):"Replace line ":LINE.NO:" ? (<Y>/N/Last) ":
                CALL EB_UT_INPUT_ZERO(DMY,MAT EB_CHARS,FG_ACT.CODE,35,PDEPTH,FG_INPUT.CODES,'Y':@VM:'N',1,FG_TIMEOUT)
                IF FG_ACT.CODE THEN GO RTN
                IF INDEX('YL',DMY,1) ELSE GO 3090
            END ELSE DMY=''
            LOOP
                OCC+=1
                IF FIRST='' THEN
                    SPOS=1
                    LOOP WHILE LINE[SPOS,1]=' ' DO SPOS+=1 REPEAT
                END ELSE
                    SPOS=INDEX(LINE,FIRST,OCC)
                    IF REGEX.SEARCH THEN
                        SPOS=EB_REGEX(SLINE,FIRST, @FALSE)
                    END ELSE
                        SPOS=INDEX(LINE,FIRST,OCC)
                    END
                END
                OCCURS<OCC>=SPOS
            WHILE ALOC AND SPOS AND FIRST NE '' DO REPEAT
            OCCS=OCC-(SPOS=0)
            FOR I=OCCS TO 1 STEP -1
                SPOS=OCCURS<I>
                IF WHOLE.WORDS THEN
                    LOOP
                        OK=((INDEX(DELIMS,LINE[SPOS-1,1],1) OR SPOS=1) AND (NOT(RSTRL) OR INDEX(DELIMS,LINE[SPOS+RSTRL,1],1)))
                    UNTIL OK DO
                        NPOS=INDEX(LINE[SPOS+1,MAX],FIRST,OCC)
                        IF NOT(NPOS) THEN BREAK
                        SPOS += NPOS
                    REPEAT
                END ELSE OK=TRUE
                IF OK THEN
                    IF CONFIRM THEN
                        CRT @(0,20):@(-3)
                        CRT @(0,20):CLEOP:
                        IF LINE.NO>1 THEN CRT LINE.NO-1 'R%4 ':REC<LINE.NO-1>[1,PWIDTH-4] ELSE CRT
                        CRT LINE.NO 'R%4 ':HILON:
                        CRT LINE[1,SPOS-1]:HILOFF:LINE[SPOS,RSTRL]:HILON:LINE[SPOS+RSTRL,MAX]:HILOFF
                        CRT HILRESET:
                        CRT LINE.NO+1 'R%4 ':REC<LINE.NO+1>[1,PWIDTH-4]
                        CRT @(0,PDEPTH):"Replace line ":LINE.NO:" ? (Y/<N>/Last) ":
                        CALL EB_UT_INPUT_ZERO(DMY,MAT EB_CHARS,FG_ACT.CODE,35,PDEPTH,FG_INPUT.CODES,'Y':@VM:'N':@VM:'L',1,FG_TIMEOUT)
                        IF FG_ACT.CODE THEN GO RTN
                    END ELSE DMY='Y'
                END ELSE DMY='N'
                IF INDEX('N',DMY,1) ELSE
                    IF RSTR=RRSTR THEN
                        TMP=WSTR; GOSUB CONV.CHARS
                        LINE=LINE[1,SPOS-1]:TMP:LINE[SPOS+RSTRL,MAX]
                    END ELSE
!
! Build up wild-card replacements
!
                        SLINE=LINE[1,SPOS-1]; THE.REST=LINE[SPOS+LEN(FIRST),MAX]
                        NEW.LINE=SLINE
                        POS=TRUE
                        FOR CNT=2 TO STR.CNT WHILE POS
                            TMP=RSTR<CNT>; GOSUB CONV.CHARS; RSTR<CNT>=TMP
                            POS=INDEX(THE.REST,TMP,1)
                            SLINE<CNT>=THE.REST[1,POS-1]
                            POS+=LEN(TMP)
                            THE.REST=THE.REST[POS,MAX]
                        NEXT CNT
                        CNT+=am_start
                        IF POS THEN
                            POS=RSTR<CNT>
                            IF POS NE '' THEN
                                TMP=POS; GOSUB CONV.CHARS; POS=TMP
                                POS=INDEX(THE.REST,POS,1)
                                SLINE<CNT>=THE.REST[1,POS-1]
                                THE.REST=THE.REST[POS+LEN(TMP),MAX]
                            END ELSE
                                IF USE.THE.REST THEN SLINE<CNT>=THE.REST; THE.REST=''
                                POS=TRUE
                            END
                        END
                        IF POS THEN
                            LEN.DIFF=LEN(LINE)
                            IF WWSTR NE '' THEN
                                TMP=WSTR; GOSUB CONV.CHARS
                                LINE=SLINE<1>:TMP:THE.REST
                            END ELSE
                                DEL SLINE<1>
                                PWSTR=WSTR
                                FOR CNT=1 TO WSTR.CNT
                                    TMP=PWSTR<1>; GOSUB CONV.CHARS; PWSTR<1>=TMP
                                    NEW.LINE:=PWSTR<1>
                                    DEL PWSTR<1>
                                    IF PWSTR NE '' THEN
                                        WCNT=PWSTR<1,1>
                                        IF NUM(WCNT) THEN
                                            op=''
                                        END ELSE
                                            op = WCNT[1,1]
                                            WCNT = WCNT[2,MAX]
                                        END
                                        WCNT=SLINE<WCNT>
                                        BEGIN CASE
                                            CASE op = ''
                                            CASE op = 'l'; op = 'MCL'
                                            CASE op = 'u'; op = 'MCU'
                                            CASE op = 'c'; op = 'MCT'
                                        END CASE
                                        IF LEN(op) THEN WCNT = OCONV(WCNT, op)
                                        NEW.LINE:=WCNT
                                        DEL PWSTR<1,1>
                                    END
                                NEXT CNT
                                TMP=PWSTR; GOSUB CONV.CHARS; PWSTR=TMP
                                LINE=NEW.LINE:PWSTR:THE.REST
                            END
                        END
                        LEN.DIFF=LEN(LINE)-LEN.DIFF+1
                    END
                    LOOP
                        POS=INDEX(LINE,'@x',1)
                    WHILE POS DO
                        LINE=LINE[1,POS-1]:ACNT:LINE[POS+2,MAX]
                        ACNT+=1
                    REPEAT
                    END.POS+=LEN.DIFF
                END
            NEXT I
            IF LINE NE ORIG.LINE THEN
                LINE = LEADWS:LINE:TRAILWS
                CRTLN=LINE
                CALL EB_TABS(CRTLN,PWIDTH,0,0)
                IF ENDL=STRT AND NOT(CONFIRM) THEN
                    CRT CRTLN[1,PWIDTH-4] LHASH:
                END ELSE
                    IF CONFIRM THEN
                        CRT @(0,19):CLEOL:
                    END ELSE
                        IF ALOC AND FIRST.DISP THEN
                            CRT @(-1)
                            FIRST.DISP=FALSE
                        END
                    END
                    CRT LINE.NO 'R%4>':CRTLN[1,PWIDTH-4] LHASH
                END
                REC<LINE.NO>=LINE
                CHANGED=TRUE
            END
            IF DMY='L' THEN GO RTN
        END ELSE STR.POS=STR.POS+LEN(LINE)
3090    !
        LINE.POS+=(STR.POS-OCCURS<1>+LEN(LINE)+1)
        IF LINE.POS=PREV.LINE.POS THEN
            LINE.POS+=OCCURS<1>
        END ELSE PREV.LINE.POS=LINE.POS
    REPEAT
RTN:
    RETURN
CONV.CHARS: ! convert ^nnn
    IF INDEX(TMP,'^',1) THEN
        L=LEN(TMP)
        FOR P=1 TO L
            C=TMP[P,1]
            IF C='^' THEN
                N=TMP[P+1,3]
                IF N MATCHES "3N" THEN
                    L-=3
                    TMP=TMP[1,P-1]:CHAR(N):TMP[P+4,L]
                END
            END
        NEXT P
    END
    RETURN
