    SUBROUTINE EB_REFRESH
    COMMON /EB_LEXER/reservedWords, colors, comments, commentlen, incomment
    INCLUDE EB.INCLUDES lex.h
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(100),EB.FILE.LIST
    COM RDSP(100),CHANGES(100)
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS ACT.CODES
MAIN$:!
!
    LAST.AM = DCOUNT(REC, AM)
*-- Default set
    IF LAST.ROW#(INDROW+ROW) THEN
        LAST.ROW-=INDROW
        RR=PDEPTH-LAST.ROW
        IF RR>=0 AND LAST.ROW>=0 THEN
            J = LAST.ROW+INDROW
            IF J > LAST.AM THEN
                DIMON = BG
                DIMOFF = FG
            END ELSE DIMON = ''; DIMOFF = ''
            CRT @(0,LAST.ROW):DIMON:J "R#4":DIMOFF:
        END
    END
    IF COL>PWIDTH AND LEN(LROW) THEN
        COL-=(PWIDTH-4)
        OFFSET+=(PWIDTH-5); COL+=1; SCR.LR=1
        IF NEW.CHARS#'' THEN
            CALL EB_ADD(START,LCOL,LLEN,INS.MODE,NEW.CHARS,RDSP(LROW))
            CHANGED=TRUE; CHANGES(LROW)=TRUE
        END
        CRT @(5,ROW):CLEOL:
        CRTLN=RDSP(LROW)
        CRT.X=1+OFFSET
        CRT.Y=PWIDTH-4
        GOSUB CRT.LN
    END
    IF COL<5 THEN
        COL=83-COL
        IF NOT(OFFSET) THEN ROW-=1 ELSE
            OFFSET-=(PWIDTH-5); COL-=1; SCR.LR=1
            IF OFFSET<0 THEN OFFSET=0
        END
    END
    IF ROW>(PDEPTH-2) THEN
        ROW=5; INDROW+=18; SCR.UD=TRUE
    END
    IF ROW<0 THEN
        ROW=17
        INDROW-=18
        IF INDROW<1 THEN INDROW=1
        SCR.UD=TRUE
    END
    IF SCR.UD THEN
        DUMMY=INDROW+(PDEPTH-1); Y=INDROW+SCRL
        FOR J=Y TO DUMMY
            RR=J-INDROW
            LROW=RR+1
            RDSP(LROW)=REC<J>
            IF SCR.UD#100 THEN
                IF J > LAST.AM THEN
                    DIMON = BG
                    DIMOFF = FG
                END ELSE DIMON = ''; DIMOFF = ''
                CRT @(0,RR):DIMON:J "R#4 ":DIMOFF:
                IF SCR.UD>0 OR (RR-(PDEPTH-2)-SCR.UD)>0 THEN
                    CRT CLEOL:
                    CRTLN=RDSP(LROW)
                    CRT.X=1+OFFSET
                    CRT.Y=PWIDTH-4
                    GOSUB CRT.LN
                END
            END ELSE IF J=CUT.POS THEN CRT @(4,RR):'[':
            IF J=CUT.POS<1,1,1> THEN
                CRT @(CUT.POS<1,1,2>+4-OFFSET,RR):BG:'[':FG:
            END
        NEXT J
        J += am_start
        CRT MSG.DSP:
        RR=J-INDROW
        LROW=RR+1
        RDSP(LROW)=REC<J>
        SCR.UD=FALSE; LLEN=LEN(RDSP(ROW+1)); SCRL=0
    END
    IF SCR.LR THEN
        DUMMY=1+SCRL
        LAST.ROW=PDEPTH-1
        LROW=PDEPTH-1+SCR.LR
        FOR J=DUMMY TO LAST.ROW
            AMPOS = J+INDROW-1
            IF AMPOS > LAST.AM THEN
                DIMON = BG
                DIMOFF = FG
            END ELSE DIMON = ''; DIMOFF = ''
            CRT @(0,J-1):DIMON:AMPOS "R#4":DIMOFF:
            IF (J+INDROW-1)#CUT.POS THEN CRT ' ': ELSE CRT '[':
            IF SCR.LR>0 OR (J-LROW)>0 THEN
                CRT CLEOL:
                S=LROW
                LROW=J
                CRTLN=RDSP(J)
                CRT.X=1+OFFSET
                CRT.Y=PWIDTH-4
                GOSUB CRT.LN
                S=LROW
            END
        NEXT J
        CRT MSG.DSP:
        SCR.LR=FALSE; SCRL=0
    END
    LROW=ROW+1
    LAST.NBR=FIRST.ALPHA
    LAST.ROW=INDROW+ROW
    IF LAST.ROW > LAST.AM THEN
        DIMON = BG
        DIMOFF = FG
    END ELSE DIMON = ''; DIMOFF = ''
    CRT @(0,ROW):HLON:DIMON:LAST.ROW "R#4":DIMOFF:HLOFF:
    RETURN
    INCLUDE EB.INCLUDES CRT.LN
