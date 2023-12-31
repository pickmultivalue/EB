    SUBROUTINE EB_SHOWMEMBERS(WORD)
    $options jabba
    INCLUDE EB.INCLUDES EB_LEXER
    INCLUDE EB.INCLUDES lex.h
    INCLUDE EB.EQUS EB.COMMON
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS ACT.CODES
    EQU MAX TO 999
MAIN$:!
    DEFC INT JBASEEmulateGETINT(INT, INT)
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    vm_start=IF_COMPILED_PRIME
!
    VARS=''
    LOCATE WORD IN VARHEADERS<am_start> BY 'AL' SETTING VPOS THEN
        VARS=VARMEMBERS<VPOS>
    END ELSE
        IF WORD EQ 'this' THEN
            Z = LROW
            LOOP WHILE Z GT 1 DO
                line = TRIM(REC<Z>)
                IF INDEX(line, '::', 1) AND FIELD(line, ' ', 1) EQ 'method' THEN
                    WORD = FIELD(line[COL2()+1,-1], ':', 1)
                    BREAK
                END
                --Z
            REPEAT
        END
        methods = $system->getroutines(WORD,0)
        IF methods->$isobject THEN
            for m in methods
                m = FIELD(m->name,':',3)
                LOCATE m IN VARS<1,1> BY 'AL' SETTING pos ELSE
                    INS m BEFORE VARS<1,1,pos>
                END
            next
        END
    END
    IF LEN(VARS) EQ 0 THEN
        READ tags FROM F.currdir,'tags' THEN
            POS=INDEX(tags,WORD:TAB,1)
            IF POS THEN
                IDATA=tags[POS,99]
                TEXT=FIELD(IDATA,TAB,2)
                TEXT=IDATA[COL2()+1,MAX]
                IDATA=TEXT
                CONVERT TAB TO SPC IN IDATA
                IDATA=TRIM(IDATA)
                IF FIELD(IDATA,SPC,1)='/^' THEN IDATA=FIELD(IDATA,SPC,2)
                IF FIELD(IDATA,SPC,1)='static' THEN IDATA=FIELD(IDATA,SPC,2)
                IDATA=FIELD(IDATA,'*',1)
                IGNORE.TYPES='char':@AM:'double':@AM:'int':@AM:'long':@AM:'short':@AM:'unsigned':@AM:'void'
                LOCATE IDATA IN IGNORE.TYPES<am_start> BY 'AL' SETTING IPOS THEN POS=FALSE
            END
        END ELSE POS=FALSE
        IF NOT(POS) THEN
            CRT MSG.CLR:"Enter var type for ":WORD:" ":
            INPTYPE='AN'; ILEN=63; IDATA=''
            GOSUB 1500          ;! input the field
            CRT MSG.AKN:
            methods = $system->getroutines(IDATA,0)
            IF methods->$isobject THEN
                for m in methods
                    m = FIELD(m->name,':',3)
                    LOCATE m IN VARS<1,1> BY 'AL' SETTING pos ELSE
                        INS m BEFORE VARS<1,1,pos>
                    END
                next
                GO gotvars
            END
        END
        C=1
        LOOP
            POS = INDEX(HEADERCODE, IDATA, C)
        WHILE POS DO
            C+=1
            TEXT=TRIM(HEADERCODE[POS-50,1000])
            CPOS = INDEX(TEXT, 'class ':IDATA, 1)
            IF CPOS THEN
                TEXT = HEADERCODE[POS + CPOS - 50, 1000]
                TEXT = TEXT[INDEX(TEXT, '{', 1), 1000]
                NDATA = '};'
            END ELSE NDATA = '} ':IDATA
            IF INDEX(TEXT, NDATA ,1) THEN
!                C=COUNT(HEADERCODE[1,POS],AM)
                C=COUNT(TEXT[1,POS],AM)
                LOOP
!                    LINE=FIELD(TRIM(HEADERCODE<C>),';',1)
                    LINE=FIELD(TRIM(TEXT<C>),';',1)
                UNTIL LINE='{' OR C=1 DO
                    C-=1
                    VarType=FIELD(LINE,' ',1)
                    IF VarType='unsigned' THEN
                        VarType:=' ':FIELD(LINE,' ',2)
                        fpos=3
                    END ELSE fpos=2
                    IF VarType='/*' OR VarType='*' OR VarType[1,1]='#' OR VarType='//' ELSE
                        CONVERT '*' TO '' IN LINE
                        LINE=TRIM(LINE)
                        v=FIELD(LINE,' ',fpos)
                        IF v#'' THEN
                            VARS<1,1,-1>=v
                            VARS<1,2,-1>=VarType
                        END
                    END
                REPEAT
                LOCATE WORD IN VARHEADERS<am_start> BY 'AL' SETTING VPOS ELSE
                    INS WORD BEFORE VARHEADERS<VPOS>
                    INS VARS BEFORE VARMEMBERS<VPOS>
                END
                BREAK
            END
        REPEAT
    END
gotvars:
    IF VARS#'' THEN
        CONVERT SVM:VM TO VM:AM IN VARS
        CALL EB_CHOICES(50,5,'','','',VARS,IDATA,1,1,1:SVM:2,'L#30':SVM:'L#30',WORD:' members')
        IF IDATA#'' THEN
            IF LCOL>1 THEN
                HASH='L#':LCOL-1
                Y=RDSP(LROW)[1,LCOL-1] HASH
            END ELSE Y=''
            RDSP(LROW)=Y:IDATA:RDSP(LROW)[LCOL,MAX]
            LCOL += LEN(IDATA)
            COL += LEN(IDATA)
        END
        SCR.UD=1
    END
    RETURN
!
    INCLUDE EB.INCLUDES CRT.LN
!
1500 !
!
    ECHO ON
    CALL EB_UT_WP(IDATA,INPTYPE,ILEN,1,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',ESC)
    FG_TIMEDOUT=FALSE
    INPTYPE='AN'
    RETURN
