! PROGRAM GETSAME
    INCLUDE JBC.h
    DEFC INT JBASEEmulateGETINT(INT, INT)
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
    T.OPTION=INDEX(OPTIONS,'T',1)
    V.OPTION=INDEX(OPTIONS,'V',1)
    S.OPTION=INDEX(OPTIONS,'S',1)
    A.OPTION=INDEX(OPTIONS,'A',1)
    M.OPTION=INDEX(OPTIONS,'M',1)
    I.OPTION=INDEX(OPTIONS,'I',1)
    O.OPTION=INDEX(OPTIONS,'O',1)
    COMPARE.ITEM=INDEX(OPTIONS,'C',1)
    IF OPTIONS#'' THEN SENT=SENT[1,COL1()-1]
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
        CRT 'C option starts off COMPARE.ITEM for the different items'
        CRT 'I option suppress output'
        CRT 'O option include object code'
        CRT 'M include missing items'
        STOP
    END
    IF SFLNM='DICT' THEN
        SFLNM='DICT ':FIELD(SENT,' ',N+1)
    END
    IF FLNM=SFLNM THEN CRT 'I think comparing items from the same file is wasting my time'; STOP
    OPEN FLNM TO FIRST ELSE STOP OPER,FLNM
    OPEN SFLNM TO SECOND ELSE STOP OPER,SFLNM
    IF NOT(SYSTEM(11)) THEN SELECT FIRST
    OPEN 'UPG.WORKFILE' TO F.UPG.WORKFILE THEN
        UPG=TRUE
        USEMODE=''; PASSWD=''
    END ELSE UPG=FALSE
    LIST=''
    EQU COMMENTS TO '!*'
    OPEN 'SAVEDLISTS' THEN
        UNIDATA=1
    END ELSE
        UNIDATA=0
        OPEN 'POINTER-FILE' ELSE STOP 201,'POINTER-FILE'
    END
    EOF=0
    LOOP
        READNEXT ID ELSE EOF=1
    UNTIL EOF OR SYSTEM(14) DO
        sfx = FIELD(ID,'.',DCOUNT(ID,'.'))
        IF O.OPTION THEN
            bpos = @FALSE
        END ELSE
            LOCATE sfx IN bad_suffixes<am_start> SETTING bpos ELSE
                bpos = (ID[1,1]='_' OR ID[1,1]='!' OR ID[1,1]='$')
            END
        END
        IF NOT(bpos) THEN
            READ FITEM FROM FIRST,ID ELSE FITEM=''
            UPGITEM=FITEM; UPGFNAME=FLNM; UPGINAME=ID
            GOSUB DECRYPT
            FITEM=UPGITEM
            READ SFITEM FROM SECOND,ID ELSE SFITEM=''
            UPGITEM=SFITEM; UPGFNAME=SFLNM
            GOSUB DECRYPT
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
                IF FITEM EQ SFITEM THEN
                    LOCATE ID IN LIST BY 'AL' SETTING POS ELSE
                        INS ID BEFORE LIST<POS>
                    END
                END
            END
        END
    REPEAT
    IF LIST#'' THEN
        IF UNIDATA THEN SUFFIX='000' ELSE SUFFIX=''
        WRITELIST LIST ON FLNM:'v':SFLNM:SUFFIX
        IF COMPARE.ITEM THEN
            DATA.STACK='COMPARE_ITEM ':FLNM:' ':SFLNM
            IF T.OPTION THEN DATA.STACK := ' (T'
            DATA DATA.STACK
            EXECUTE 'GET-LIST ':FLNM:'v':SFLNM
        END ELSE
            CRT FLNM:'v':SFLNM:' list saved'
        END
    END ELSE CRT 'No matches'
    STOP
DECRYPT: !
    IF UPG THEN
        CALL UPGCHKENCRYPT(UPGITEM,ENCRYPTED,CHKSUM,SIZE,VERSION)
    END ELSE ENCRYPTED=''
    IF ENCRYPTED THEN
        IF NOT(UPG) THEN
            CRT 'Encrypted program (':ID:')...required UPG.WORKFILE'
            STOP
        END
    END
    IF ENCRYPTED THEN
        IF USEMODE='' THEN CALL UPGMODE (USEMODE)
        IF PASSWD='' THEN CALL UPGPASSWD (PASSWD,F.UPG.WORKFILE)
        CALL UPGCONVERT(UPGFNAME,UPGINAME,UPGITEM,'DQ',USEMODE,F.UPG.WORKFILE,PASSWD,CONVOK)
        IF NOT(CONVOK) THEN STOP
    END
    RETURN
