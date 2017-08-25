* @(#) PAINT.TAG Ported to jBASE 15:13:20  17 APR 2000
    IF MSG='DISPLAY' THEN ACT=MV ELSE ACT='L':MV:SVM:SMV
    IF TAGGING THEN
        LOCATE ACT IN TAGGED<1> SETTING POS THEN
            TAGGED=DELETE(TAGGED,1,POS,0)
            TAGGED=DELETE(TAGGED,2,POS,0)
            TAGGED=DELETE(TAGGED,3,POS,0)
            CRT @(COL,ROW):FG:MOVE.FIELD:
            IF MSG='DISPLAY' THEN
                LOCATE MV IN FIRST.WIN<1> SETTING WIN THEN
                    CRT @(COL,LAST.ROWS<1,WIN>):'<W':WIN:'>':
                END
            END
            IF FLD=ST THEN TAGGING=FALSE
        END ELSE
            TAGGED<1,POS>=ACT
            TAGGED<2,POS>=MOVE.FIELD
            TAGGED<3,POS>=COL 'R#3':ROW
            CRT @(COL,ROW):BG:MOVE.FIELD:FG:
            IF MSG='DISPLAY' THEN
                LOCATE MV IN FIRST.WIN<1> SETTING WIN THEN
                    CRT @(COL,LAST.ROWS<1,WIN>):SPACE(LEN('<W':WIN:'>')):
                END
            END
            ST=FIRST.FIELD; FI=LAST.FIELD
        END
    END ELSE
        LOCATE ACT IN TAGGED<1> SETTING POS THEN
            TAGGED=DELETE(TAGGED,1,POS,0)
            TAGGED=DELETE(TAGGED,2,POS,0)
            TAGGED=DELETE(TAGGED,3,POS,0)
            CRT @(COL,ROW):FG:MOVE.FIELD:
            IF MSG='DISPLAY' THEN
                LOCATE MV IN FIRST.WIN<1> SETTING WIN THEN
                    CRT @(COL,LAST.ROWS<1,WIN>):'<W':WIN:'>':
                END
            END
        END
    END
