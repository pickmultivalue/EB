    SUBROUTINE EB_FORMAT(CHECK.LINE,MARGIN,LNM)
!
    INCLUDE EB.EQUS EB.COMMON
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    EQU AM TO CHAR(254)
MAIN$:!
    CALL EB_TABS(CHECK.LINE,PWIDTH,0,0)
    TABLEN=ITAB<ITABPOS>
    IF UNFORMATTED OR (CHECK.LINE[1,1] NE ' ' AND CHECK.LINE NE '{') THEN
        MARGIN=1
        RETURN
    END
    CALL EB_TRIM(CHECK.LINE,CHECK.LINE:'',' ','T')
    CHECK.LINE=UPCASE(CHECK.LINE)
    LLEN=LEN(CHECK.LINE)
    SPOS=INDEX(CHECK.LINE,' ',1)
    IF SPOS THEN
        FIRST.WORD=CHECK.LINE[1,SPOS]
        IF INDEX(FIRST.WORD,':',1) OR INDEX(FIRST.WORD,COMMENT,1) AND NOT(INDEX(FIRST.WORD,'"',1) OR INDEX(FIRST.WORD,"'",1)) THEN          ;! label
            MARGIN=3
        END ELSE
            FOR MARGIN=SPOS TO LLEN UNTIL CHECK.LINE[MARGIN,1] NE " "; NEXT MARGIN
            CHECK.LINE=CHECK.LINE[MARGIN,9999]
            NBR.WORDS=INDEX(CHECK.LINE,';',1)
            IF NBR.WORDS THEN
                CHECK.LINE=CHECK.LINE[1,NBR.WORDS-1]
                CALL EB_TRIM(CHECK.LINE,CHECK.LINE:'',' ','T')
            END
            IF CHECK.LINE[1,3] NE '!#!' THEN
                IF TRIM(CHECK.LINE[1,COMMENTLEN])=COMMENT THEN MARGIN=SPOS; RETURN        ;! no format
            END
            MARGIN+=TABLEN
        END
    END ELSE MARGIN=3
!
    FIRST.WORD=FIELD(CHECK.LINE,' ',1)
    POS=INDEX(CHECK.LINE,';',1)
    IF POS THEN
        IF INDEX(CHECK.LINE[POS+1,999],'"',1) OR INDEX(CHECK.LINE[POS+1,999],"'",1) ELSE
            CHECK.LINE=TRIM(CHECK.LINE[1,POS-1],' ',"T")
        END
    END
    NBR.WORDS=DCOUNT(CHECK.LINE,' ')
    LAST.WORD=FIELD(CHECK.LINE,' ',NBR.WORDS)
    BEGIN CASE
        CASE LNM=1 AND LAST.WORD='THEN'
            CHECK.LINE='END':AM:'END IF':AM:'ELSE':AM:'ELSIF'
        CASE LNM=1 AND LAST.WORD='LOCKED'; CHECK.LINE='END'
        CASE LNM=1 AND LAST.WORD='ELSE'; CHECK.LINE='END'
        CASE LNM=1 AND FIRST.WORD='BEGIN'
            IF COMMENT='--' THEN
                CHECK.LINE='END'
                CHECK.LINE<-1>='END;'
            END ELSE CHECK.LINE='END CASE'
        CASE LNM=1 AND (CHECK.LINE='LOOP' OR LAST.WORD='DO')
            CHECK.LINE='REPEAT':AM:'WHILE':AM:'UNTIL'
        CASE FIRST.WORD='CASE' AND INDEX(CHECK.LINE[MARGIN+3,999],';',1); CHECK.LINE='CASE'; MARGIN=MARGIN-TABLEN
        CASE FIRST.WORD='CASE'; CHECK.LINE='CASE'
        CASE FIRST.WORD='METHOD'; CHECK.LINE='END METHOD'
        CASE LNM=1 AND FIRST.WORD='FOR' AND NOT(INDEX(CHECK.LINE[MARGIN+3,999],'NEXT ',1))
            CHECK.LINE='NEXT':AM:'END LOOP;'
        CASE LNM=-1 AND (CHECK.LINE='END' OR FIRST.WORD='ELSIF' OR LAST.WORD='ELSE' OR LAST.WORD='IF' OR OCONV(CHECK.LINE,'G 2')='END ELSE' OR OCONV(CHECK.LINE,'G 2')='END THEN')
            IF COMMENT='--' THEN
                IF FIRST.WORD='END' AND LAST.WORD NE 'IF' AND LAST.WORD NE 'LOOP' THEN CHECK.LINE='BEGIN' ELSE
                    CHECK.LINE='IF'
                    CHECK.LINE<-1>='ELSE'
                    CHECK.LINE<-1>='ELSIF'
                END
            END ELSE
                CHECK.LINE='IF':AM:'LOCATE':AM:'END ELSE'
                CHECK.LINE<-1>='READ':AM:'MATREAD'
                CHECK.LINE<-1>='END THEN':AM:'OPEN'
                CHECK.LINE<-1>='READU':AM:'MATREADU'
                CHECK.LINE<-1>='READV':AM:'READVU'
            END
        CASE LNM=-1 AND CHECK.LINE='END CASE'; CHECK.LINE='BEGIN CASE'
        CASE LNM=-1 AND FIRST.WORD='NEXT' OR CHECK.LINE='END LOOP'; CHECK.LINE='FOR'
        CASE LNM=-1 AND (LAST.WORD='REPEAT' OR LAST.WORD='DO'); CHECK.LINE='LOOP':AM:'WHILE':AM:'UNTIL'
        CASE FIRST.WORD='!#!INCLUDE'; CHECK.LINE='!#!'; MARGIN=0
        CASE FIRST.WORD='!#!' ; CHECK.LINE='!#!INCLUDE'; MARGIN=0
        CASE LNM=1 AND LAST.WORD='{'
            CHECK.LINE='}'
        CASE LNM=-1 AND LAST.WORD='}'
            CHECK.LINE='{'
            MARGIN-=TABLEN
        CASE 1; MARGIN-=TABLEN; CHECK.LINE=''
    END CASE
    RETURN
