    INCLUDE JBC.h
    $option jabba
    DEFC INT JBASEEmulateGETINT(INT, INT)
    DEFFUN EBGETHOME()
    path = EBGETHOME()
    jelf = new object('jelf_helper')
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    mv_start=IF_COMPILED_PRIME
    SENT=SENTENCE()
    EQU OPER TO 201,RDER TO 202
    EQU SLER TO 404
    EQU AM TO CHAR(254), VM TO CHAR(253), SVM TO CHAR(252), TRUE TO 1, FALSE TO 0
    bad_suffixes = 'o':@AM:'a':@AM:'obj':@AM:'so':@AM:'sl':@AM:'dll':@AM:'exe':@AM:'ncb':@AM:'sln':@AM:'vcproj':@AM:'el'
    bad_suffixes<-1> = 'sbr':@AM:'idb':@AM:'pdb':@AM:'plg':@AM:'suo':@AM:'vspscc':@AM:'vssscc':@AM:'dsw':@AM:'dsp':@AM:'opt'
    bad_suffixes<-1> = 'buf':@AM:'ilk':@AM:'j':@AM:'lib'
    OPTIONS=OCONV(FIELD(SENT,'(',2),'MCU')
    B.OPTION=INDEX(OPTIONS,'B',1)
    T.OPTION=INDEX(OPTIONS,'T',1)
    V.OPTION=INDEX(OPTIONS,'V',1)
    S.OPTION=INDEX(OPTIONS,'S',1)
    A.OPTION=INDEX(OPTIONS,'A',1)
    M.OPTION=INDEX(OPTIONS,'M',1)
    I.OPTION=INDEX(OPTIONS,'I',1)
    O.OPTION=INDEX(OPTIONS,'O',1)
    R.OPTION=INDEX(OPTIONS,'R',1)
    W.OPTION=INDEX(OPTIONS,'W',1)
    ONE_DIFF = NOT(W.OPTION)
    COMPARE.ITEM=INDEX(OPTIONS,'C',1)
    IF LEN(OPTIONS) THEN SENT=SENT[1,COL1()-1]
    FLNM=FIELD(SENT,' ',2)
    IF FLNM='DICT' THEN
        FLNM='DICT ':FIELD(SENT,' ',3)
        N=4
    END ELSE N=3
    SFLNM=FIELD(SENT,' ',N)
    IF SFLNM='DICT' THEN
        N+=1
        SFLNM='DICT ':FIELD(SENT,' ',N)
    END
    IGNORE.LIST=FIELD(SENT,' ',N+1)
    CONVERT ',' TO AM IN IGNORE.LIST
    IF FLNM='' OR SFLNM='' THEN
        CRT 'Syntax: COMPL first_file second_file {(CTM}'
        CRT 'T option trims all spaces'
        CRT 'A option trims trailing AMs'
        CRT 'V option trims trailing VMs'
        CRT 'C option starts off COMPARE_ITEM for the different items'
        CRT 'I option suppress output'
        CRT 'O option include object code'
        CRT 'M include missing items'
        CRT 'R recurse each dir'
        CRT 'W hole record is parsed for differences.'
        STOP
    END
    IF SFLNM='DICT' THEN
        SFLNM='DICT ':FIELD(SENT,' ',N+1)
    END
    IF FLNM=SFLNM THEN CRT 'I think comparing items from the same file is wasting my time'; STOP
    OPEN 'UPG.WORKFILE' TO F.UPG.WORKFILE THEN
        UPG=TRUE
        USEMODE=''; PASSWD=''
    END ELSE UPG=FALSE
    LIST=''
    EQU COMMENTS TO '!*'
!    OPEN 'SAVEDLISTS' THEN
!        UNIDATA=1
!    END ELSE
    UNIDATA=0
    OPEN path:'POINTER-FILE' ELSE STOP 201,path:'POINTER-FILE'
!    END
    IF R.OPTION THEN
        PFLNM = FLNM; PSFLNM = SFLNM
        OPEN PFLNM TO F.parent ELSE STOP 201,PFLNM
        SELECT F.parent TO paths
        LOOP WHILE READNEXT path FROM paths DO
            FLNM = PFLNM:DIR_DELIM_CH:path
            SFLNM = PSFLNM:DIR_DELIM_CH:path
            OPEN FLNM THEN
                OPEN SFLNM THEN
                    CRT 'Processing ':FLNM
                    RQM
                    GOSUB compare
                END
            END
        REPEAT
    END ELSE
        GOSUB compare
    END
    STOP
compare:
    OPEN FLNM TO FIRST ELSE STOP OPER,FLNM
    OPEN SFLNM TO SECOND ELSE STOP OPER,SFLNM
    IF NOT(SYSTEM(11)) THEN
        SELECT FIRST
    END
    FOBJ = FIELD(FLNM, ',', 2) EQ 'OBJECT'
    SFOBJ = FIELD(SFLNM, ',', 2) EQ 'OBJECT'
    IF FOBJ THEN
        rc = IOCTL(FIRST, JBC_COMMAND_GETFILENAME, FOBJ)
        FOBJ := DIR_DELIM_CH
    END
    IF SFOBJ THEN
        rc = IOCTL(SECOND, JBC_COMMAND_GETFILENAME, FOBJ)
        SFOBJ := DIR_DELIM_CH
    END
    EOF=0
    LOOP
        READNEXT ID ELSE EOF=1
    UNTIL EOF OR SYSTEM(14) DO
        IF FOBJ THEN
            ID = ID[1, LEN(ID)-3]
        END
        sfx = FIELD(ID,'.',DCOUNT(ID,'.'))
        IF O.OPTION THEN
            bpos = @FALSE
        END ELSE
            LOCATE sfx IN bad_suffixes<am_start> SETTING bpos ELSE
                bpos = (ID[1,1]='_' OR ID[1,1]='!' OR ID[1,1]='$')
            END
        END
        IF NOT(bpos) THEN
            IF FOBJ THEN
                FITEM = jelf->getobject(FOBJ:ID:'.so')->embed_source
            END ELSE
                READ FITEM FROM FIRST,ID THEN
                    IF NOT(B.OPTION) THEN
                        LINE = FITEM<1>
                        IF LINE NE OCONV(LINE,'MCP') THEN CONTINUE
                    END
                END ELSE FITEM=''
            END
            UPGITEM=FITEM; UPGFNAME=FLNM; UPGINAME=ID
            FITEM=UPGITEM
            IF SFOBJ THEN
                SFITEM = jelf->getobject(SFOBJ:ID:'.so')->embed_source
            END ELSE
                READ SFITEM FROM SECOND,ID THEN
                    IF NOT(B.OPTION) THEN
                        LINE = SFITEM<1>
                        IF LINE NE OCONV(LINE,'MCP') THEN CONTINUE
                    END
                END ELSE SFITEM=''
            END
            UPGITEM=SFITEM; UPGFNAME=SFLNM
            SFITEM=UPGITEM
            IF FITEM='' OR SFITEM='' AND NOT(M.OPTION) ELSE
                IF A.OPTION THEN
                    FITEM=TRIM(FITEM,AM,"T")
                    SFITEM=TRIM(SFITEM,AM,"T")
                END
                FCOUNT=DCOUNT(FITEM,AM)
                SCOUNT=DCOUNT(SFITEM,AM)
                IF T.OPTION THEN
                    FITEM=TRIM(FITEM,' ',"A")
                    SFITEM=TRIM(SFITEM,' ',"A")
                END
                IF FITEM NE SFITEM THEN
                    I=1
                    FPOS=0; SPOS=0
                    LOOP
                        LOCATE I IN IGNORE.LIST<am_start> BY 'AR' SETTING IPOS THEN
                            DIFF=FALSE
                        END ELSE
                            IF T.OPTION THEN
                                LINEF=TRIM(FITEM<I>); LINES=TRIM(SFITEM<I>)
                            END ELSE
                                LINEF=FITEM<I>; LINES=SFITEM<I>
                            END
                            IF S.OPTION THEN
                                FCNT=DCOUNT(LINEF,VM)
                                FOR S=1 TO FCNT
                                    VALF=LINEF<1,S>
                                    VALF=TRIM(VALF,SVM,"T")
                                    LINEF<1,S>=VALF
                                NEXT S
                                SCNT=DCOUNT(LINES,VM)
                                FOR S=1 TO SCNT
                                    VALS=LINES<1,S>
                                    VALS=TRIM(VALS,SVM,"T")
                                    LINES<1,S>=VALS
                                NEXT S
                            END
                            IF V.OPTION THEN
                                LINEF=TRIM(LINEF,VM,"T")
                                LINES=TRIM(LINES,VM,"T")
                            END
                            IF LEN(LINEF) THEN
                                IF INDEX(COMMENTS,TRIM(LINEF)[1,1],1) THEN LINEF=COMMENTS
                            END
                            IF LEN(LINES) THEN
                                IF INDEX(COMMENTS,TRIM(LINES)[1,1],1) THEN LINES=COMMENTS
                            END
                            DIFF=((LINEF NE LINES))
                        END
                        IF DIFF THEN
                            LOCATE ID IN LIST<am_start> BY 'AL' SETTING POS THEN
                                POS = ONE_DIFF
                            END ELSE
                                INS ID BEFORE LIST<POS>
                                POS = @FALSE
                            END
                            IF NOT(POS) THEN
                                IF NOT(I.OPTION) THEN
                                    CRT ID 'L#20 ':'(':I 'R%3':') >>':LINEF 'L#50<<'
                                    CRT SPACE(21):'(':I 'R%3':') >>':LINES 'L#50<<'
                                END
                            END
                        END
                    UNTIL DIFF AND ONE_DIFF OR (I>=SCOUNT AND I>=FCOUNT) DO I+=1 REPEAT
                END
            END
        END
    REPEAT
    IF LEN(LIST) THEN
        IF UNIDATA THEN SUFFIX='000' ELSE SUFFIX=''
        WRITELIST LIST ON FLNM:'v':SFLNM:SUFFIX
        LIST = ''
        IF COMPARE.ITEM THEN
            DATA.STACK='COMPARE_ITEM ':FLNM:' ':SFLNM
            IF T.OPTION THEN DATA.STACK := ' (T'
            DATA DATA.STACK
            EXECUTE 'GET-LIST ':FLNM:'v':SFLNM
        END ELSE
            CRT FLNM:'v':SFLNM:' list saved'
        END
    END ELSE CRT 'No differences'
    RETURN
