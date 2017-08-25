* @(#) PAINT.MOVE Ported to jBASE 15:13:18  17 APR 2000
    IF TAGGED<1>#'' THEN
        SEL.FLAG=FALSE
        ERROR.MSG=@(0,23):RVON:BG:'Now moving tagged fields, hit ':FG:'<Return>':BG:' when positioned correctly':RVOFF:FG:CLEOL
        POS=DCOUNT(TAGGED<1>,VM)
        FOR I=1 TO POS
            MOVE.FIELD=TAGGED<2,I>
            BLANK.FIELD=LEN(MOVE.FIELD)
            RR=TAGGED<3,I>
            CC=TRIM(RR[1,3]);RR=RR[4,2]
            IF CC#0 THEN HASH='L#':CC ELSE HASH=''
            LTEMP=THIS.SCREEN<RR+1>
            LTEMP=LTEMP[1,CC] HASH:SPACE(BLANK.FIELD):LTEMP[CC+BLANK.FIELD+1,999]
            THIS.SCREEN<RR+1>=LTEMP     ;! LINE.HASH
        NEXT I
        MOVE.FLAG=TRUE
    END ELSE
        GOSUB FIND.FIELD
        IF MV#'' THEN
            IF MSG='WINDOW' THEN
                ERROR.MSG=@(0,23):RVON:BG:'Use the Up & Down keys to change the depth of ':FG:MV:' * ':WINDOW.NAMES<1,MV>:RVOFF:CLEOL
                WIN.FLAG=TRUE
            END ELSE
                ERROR.MSG=@(0,22):RVON:BG:'Now MOVING Field ':CLEOL:@(0,23):FG:MV:' ':SCR.FLD.NAMES<1,MV>:BG:', hit ':FG:'<Return>':BG:' when positioned correctly':RVOFF:FG:CLEOL
                MOVE.FLAG=TRUE
                IF MSG='DISPLAY' THEN
                    LOCATE MV IN FIRST.WIN<1> SETTING WIN THEN
                        CRT @(COL,LAST.ROWS<1,WIN>):SPACE(LEN('<W':WIN:'>')):
                    END
                END
            END
            LTEMP=THIS.SCREEN<ROW+1>
            LTEMP=LTEMP[1,COL] HASH:SPACE(BLANK.FIELD):LTEMP[COL+BLANK.FIELD+1,999]
            THIS.SCREEN<ROW+1>=LTEMP    ;! LINE.HASH
        END
    END
    CRT ERROR.MSG:
