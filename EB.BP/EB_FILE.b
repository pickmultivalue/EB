    SUBROUTINE EB_FILE(SKIP.PATCH,K.PATCHFILE,MAT PATCH,COMPILE.IT,ENCRYPTED,UPG)
    INCLUDE JBC.h
    INCLUDE EB.EQUS EB.COMMONS
    DEFFUN GETFLNM()

    DEFFUN SRC_GETORIGPATH()
    DEFFUN SRC_READHIST()
    DEFFUN SRC_UPDATEHIST()
    DEFFUN SRC_OPENHIST()
    DEFFUN SRC_CHECKOUT()
!
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(100),EB.FILE.LIST
    COM RDSP(100), CHANGES(100)
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS SCREEN.PARAMS
    EQU MAX TO 9999
MAIN$:!
    DIM PATCH(11)
    EQU SPRNO TO PATCH(1)
    EQU APPL TO PATCH(2)
    EQU DESCRIPTION TO PATCH(3)
    EQU TYPE TO PATCH(4)
    EQU FilePath TO PATCH(8)
    EQU CHANGED.BY TO PATCH(9)
    EQU REL.NO TO PATCH(10)
    EQU PATCH.ITEM TO PATCH(11)
!
    CALL EB_OPEN('','.',F.currdir,1,0)
    IF COMMENT NE '# ' THEN ;!OR INDEX(REC, ';':@AM,1) OR INDEX(REC,'IF ',1) THEN
        whitespace = ' ':CHAR(9)
        dc = DCOUNT(REC, @AM)
        FOR l = 1 TO dc
            line = REC<l>
            tline = line
            CONVERT whitespace TO '' IN tline
            IF NOT(LEN(tline)) THEN
                REC<l> = ''
            END ELSE
                FOR c = LEN(line) TO 1 STEP -1
                    ch = line[c,1]
                    IF NOT(INDEX(whitespace, ch, 1)) THEN
                        IF c LT LEN(line) THEN
                            REC<l> = line[1,c]
                        END
                        BREAK
                    END
                NEXT c
            END
        NEXT l
    END
    WRITE REC ON FIL,ITNM SETTING setvar ON ERROR
        YNL='Permission denied: add +w ? (Y/N) '
        CRT MSG.CLR:YNL:
        YNC=LEN(YNL); YNR=(PDEPTH-1); YNCHRS='Y':VM:'N':AM:AM:'Y'; YNL=1; GOSUB GET.CHAR
        CRT MSG.CLR:
        IF Y='Y' THEN
            EXECUTE 'jchmod +w ':SRC_GETORIGPATH(FLNM):DIR_DELIM_CH:ITNM CAPTURING result
            WRITE REC ON FIL,ITNM SETTING setvar ON ERROR
                CRT MSG.CLR:' unable to update (':setvar:')'
                Y='N'
            END
        END
        IF Y#'N' THEN RETURN
    END
    Y=FLNM:'*':ITNM
    DELETE JET.PASTE,ITNM:'.sav'
    UPDHIST = SRC_OPENHIST(F.HISTFILE, FLNM)
    COMPILE.IT=''
    CALL EB_ASSIGN_OPEN('EB.STDPARAMS',FPOS,F.STD.PARAMS,0)
    FULLFLNM=TRIM(DCT<2>:' ':DCT<1>)
    IF FPOS THEN
        READ UPDATE.RULES FROM F.STD.PARAMS,'UPDATE.RULES' ELSE UPDATE.RULES=''
        LOCATE FULLFLNM IN UPDATE.RULES<1,vm_start> SETTING FPOS THEN
            LOCATE ITNM IN UPDATE.RULES<3,FPOS,vm_start> SETTING POS THEN
                UPDATE.RULE=UPDATE.RULES<4,FPOS,POS>
            END ELSE UPDATE.RULE=UPDATE.RULES<2,FPOS>
            IF UPDATE.RULE#'' THEN
                UPDATE.TYPE=FIELD(UPDATE.RULE,'*',1)
                UPDATE.RULE=UPDATE.RULE[COL2()+1,MAX]
                BEGIN CASE
                    CASE UPDATE.TYPE='T'
                        EXECUTE UPDATE.RULE
                    CASE UPDATE.TYPE='B'
                        CALL @UPDATE.RULE
                END CASE
            END
        END
    END
    LAST.WORD=FIELD(FULLFLNM,'.',DCOUNT(FULLFLNM,'.'))
    SKIP.PATCH = SKIP.PATCH OR INDEX(ITNM,'%',1) OR INDEX(FULLFLNM,'PATCH',1) OR LAST.WORD='PATCHES'
    IF SKIP.PATCH THEN
        TYPE='ITEM'
        RETURN
    END
    INPTYPE='AN'
    EQU TYPE.FLD TO 1
    EQU TIME.FLD TO 2
    EQU DURATION.FLD TO 3
    EQU DESC.FLD TO 4
    EQU APPL.FLD TO 5
    EQU REL.FLD TO 6
    EQU PR.FLD TO 7
    EQU COMP.FLD TO 8
    EQU LAST.FLD TO 9
!
    PATCH.FLD=TYPE.FLD
!
    INCLUDE EB.OS.INCLUDES OS
    IF FULLFLNM='EB.MISC' THEN TYPE='BASIC'; PATCH.FLD=PR.FLD; GO COMPILE
    PFLNM = SRC_GETORIGPATH(FLNM)
    IF OPEN.PATCH OR UPDHIST THEN
        FileName = GETFLNM(FLNM)
        DAPPL=FIELD(FileName,'.',1); THE.REST=FileName[COL2()+1,MAX]
        LOCATE FULLFLNM IN BP.FILES<1,vm_start> SETTING BPOS THEN
            DTYPE=BP.FILES<2,BPOS>
            DAPPL=BP.FILES<3,BPOS>
        END ELSE
            CALL EB_OPEN('', FLNM:',OBJECT', F.OBJ, FALSE, OPOS)
            IF OPOS THEN
                DTYPE = 'BASIC'
            END ELSE
                BEGIN CASE
                    CASE DCT<2>='DICT'
                        DTYPE='ITEM'
                    CASE THE.REST='OS.INCLUDES'
                        DAPPL='EB'
                        DTYPE='ITEM'
                    CASE OCONV(FULLFLNM,'MCU') 'R#2'='BP' OR FULLFLNM 'R#3'='UTL' OR FULLFLNM 'R#2'='BT' OR ITNM 'R#2'='.b' OR ITNM 'R#6'='.jabba'
                        DTYPE = 'BASIC'
                    CASE COMMENT='--'
                        DTYPE='SQL'
                    CASE 1
                        LOCATE DAPPL IN APPLICATION.CODES<1,vm_start> SETTING POS ELSE
                            rc = DCOUNT(DAPPL, DIR_DELIM_CH)
                            DAPPL = FIELD(DAPPL, DIR_DELIM_CH, rc)
                        END
                        DTYPE='ITEM'
                END CASE
            END
            IF ORIG.REC=REC AND DTYPE='BASIC' THEN DTYPE='RECOMPILE'
        END
        MAT PATCH=''
        DESCRIPTION=LAST.DESC
        TYPE = DTYPE
        APPL = DAPPL
        HISTORY = SRC_READHIST(1, FLNM, ITNM, DATE(), '', '', FG_LOGNAME)
        IF OPEN.PATCH THEN
            PFLNM=SWAP(PFLNM,'@','<{AT}>')
            PITNM=SWAP(ITNM,'@','<{AT}>')
            IF PITNM='EB.INSTALL.PATCHES' THEN K.PATCHFILE=PITNM ELSE
                K.PATCHFILE = PFLNM:'@':PITNM:'@':DATE()
            END
            MATREAD PATCH FROM F.PATCHFILE,K.PATCHFILE THEN
                STIMES = DESCRIPTION<1,2>
                S.PATCHFILE = K.PATCHFILE:'@':STIMES<1, 1, DCOUNT(STIMES, @SVM)>
                MATWRITE PATCH ON F.PATCHFILE,S.PATCHFILE
                PATCH.ITEM=''
            END
        END
        PREV.HIST=DESCRIPTION
        IF LEN(HISTORY) THEN
            PREV.HIST = CHANGE(HISTORY<1>, @VM, @SVM)
            PREV.HIST<1, 2> = CHANGE(HISTORY<3>, @VM, @SVM)
            PREV.HIST<1, 3> = CHANGE(HISTORY<4>, @VM, @SVM)
        END
        PREV.HIST = RAISE(PREV.HIST)
        PDESC=LAST.DESC
        PETIME=TIME()
        CHANGED.BY=FG_TUSER:'@':SITE.NAME
        CRT @(0,10):@(-3):
        CRT @(0,11):STR('=',PWIDTH):
RESTART: !
        LOOP
            IF PATCH.FLD=TYPE.FLD THEN
                INPTYPE='U'
                CRT @(0,12):BG:'Type:':FG:; L=20; Z=TYPE; CRT @(15,12):; GOSUB INPT
                IF NOT(FG_ACT.CODE) THEN
                    TYPE=Z
                    LOCATE TYPE IN PATCH.TYPES<1,vm_start> SETTING POS ELSE TYPE=''
                    IF TYPE='NOPATCH' OR TYPE='DEBUG' THEN PATCH.FLD=COMP.FLD ELSE
                        IF TYPE#'' THEN PATCH.FLD+=1
                    END
                END ELSE
                    BEGIN CASE
                        CASE FG_ACT.CODE=FG_ABT.CODE
                            PATCH.FLD=LAST.FLD
                        CASE FG_ACT.CODE=FG_OPT.CODE
                            CALL EB_CHOICES(50,12,'','','',PATCH.TYPES,TYPE,1,'':AM:TYPE:AM:AM:AM:1,1,'L#10','Types')
                    END CASE
                END
            END
!
            IF PATCH.FLD=TIME.FLD THEN
                CRT @(0,13):BG:'Started*: ':FG:
                L=8; Z=PSTIME; CRT @(15,13):
                INPTYPE='T'
                GOSUB 1550
                BEGIN CASE
                    CASE FG_ACT.CODE=FG_BCK.CODE
                        PATCH.FLD-=1
                    CASE FG_ACT.CODE=FG_ABT.CODE
                        PATCH.FLD=LAST.FLD
                    CASE FG_ACT.CODE=FG_OPT.CODE
                        CRT MSG.CLR:
                        SAVE.PSTIME=PSTIME:VM:PETIME
                        PHCNT = DCOUNT(PREV.HIST<1>,VM)
                        PSTIME=PREV.HIST<2,PHCNT>
                        CALL EB_CHOICES(10,14,'','','',PREV.HIST,RESULT,1:SVM:1:SVM:1,AM:PSTIME,2:SVM:3:SVM:1,'L#8':CTRL.C:'MTS':SVM:'L#8':CTRL.C:'MTS':SVM:'L#55','History')
                        IF RESULT#'' THEN
                            PETIME=RESULT<1>
                            PSTIME=RESULT<2>
                            PDESC=RESULT<3>
                            IF INDEX(PETIME,':',1) THEN PETIME=ICONV(PETIME,'MTS')
                        END ELSE
                            PSTIME=SAVE.PSTIME<1,1>; PETIME=SAVE.PSTIME<1,2>
                        END
                    CASE 1
                        PSTIME=Z
                        PATCH.FLD+=1
                END CASE
            END
!
            IF PATCH.FLD=DURATION.FLD THEN
                CRT @(40,13):BG:'Finished: ':FG:
                L=8; Z=PETIME; CRT @(55,13):
                INPTYPE='T'
                GOSUB 1550
                BEGIN CASE
                    CASE FG_ACT.CODE=FG_BCK.CODE
                        PATCH.FLD-=1
                    CASE FG_ACT.CODE=FG_ABT.CODE
                        PATCH.FLD=LAST.FLD
                    CASE 1
                        PETIME=Z
                        PATCH.FLD+=1
                END CASE
            END
!
            IF PATCH.FLD=DESC.FLD THEN
                CRT @(0,14):BG:'Description*:':FG:
                L=400; Z=PDESC; CRT @(0,15):
                INPTYPE='AN':AM:AM:LEN(Z)+1
                GOSUB 1550
                FG_TYPEAHEAD.BUFF = ''
                BEGIN CASE
                    CASE FG_ACT.CODE=FG_BCK.CODE
                        PATCH.FLD-=1
                    CASE FG_ACT.CODE=FG_ABT.CODE
                        PATCH.FLD=LAST.FLD
                    CASE FG_ACT.CODE=FG_OPT.CODE
                        IF UPDHIST THEN
                            CRT MSG.CLR:'Building history...':
                            IF PDESC#'' THEN Z=PDESC:';' ELSE Z=''
                            PREV.HIST = HISTORY<1>
                            IF LEN(PREV.HIST) THEN
                                PREV.HIST<2>=ITNM:STR(VM:ITNM,COUNT(PREV.HIST,VM))
                            END
                            IDLEN = 15
                            DESCLEN = PWIDTH - IDLEN - 10
                            SELECT F.HISTFILE TO HISTLIST
                            LOOP
                                READNEXT HID FROM HISTLIST ELSE HID=AM
                            UNTIL HID=AM DO
                                IF NOT(INDEX(HID, '@', 1)) THEN
                                    IF HID#ITNM THEN
                                        PHISTORY = SRC_READHIST(1, FLNM, HID, '', '', '', '')
                                        IF LEN(PHISTORY) THEN
                                            PDESC = PHISTORY<1>
                                            HID = OCONV(HID, 'VDOTC#':IDLEN)
                                            PREV.HIST<1,-1>=PDESC
                                            PREV.HIST<2,-1>=HID:STR(VM:HID,COUNT(PDESC,VM))
                                        END
                                    END
                                END
                            REPEAT
                            CRT MSG.CLR:
                            PDESC=''
                            CALL EB_CHOICES(5,12,'','','',PREV.HIST,PDESC,1,AM:AM:AM:AM:1,1:SVM:2,'L#':DESCLEN:SVM:'L#':IDLEN,'History')
                            PDESC=Z:PDESC
                        END
                    CASE 1
                        PDESC=Z
                        LAST.DESC=Z
                        PATCH.FLD+=1
                END CASE
                CRT @(-3):
            END
!
            IF PATCH.FLD=APPL.FLD THEN
                INPTYPE='U'
                CRT @(0,20):BG:'Application:':FG:; L=10; Z=APPL; CRT @(15,20):; GOSUB INPT
                IF NOT(FG_ACT.CODE) THEN
                    APPL=Z
                    IF APPL#'' THEN PATCH.FLD+=1
                END ELSE
                    BEGIN CASE
                        CASE FG_ACT.CODE=FG_ABT.CODE
                            PATCH.FLD=LAST.FLD
                        CASE FG_ACT.CODE=FG_OPT.CODE
                            CALL EB_CHOICES(50,12,'','','EB.CONTROL','APPLICATIONS',APPL,1,'':AM:APPL:AM:AM:AM:1,1:SVM:2,'L#10':SVM:'L#30','Applications')
                        CASE 1; FG_ACT.CODE=FG_BCK.CODE
                            PATCH.FLD-=1
                    END CASE
                END
            END
!
            READV REL.NO FROM FG_EB.CONTROL,APPL:'.REL',1 ELSE REL.NO=''
            IF PATCH.FLD=REL.FLD THEN
                CRT @(0,21):BG:'Release:':FG:; L=7; Z=REL.NO; CRT @(15,21):; GOSUB INPT
                IF NOT(FG_ACT.CODE) THEN
                    REL.NO=Z
                    PATCH.FLD+=1
                END ELSE
                    IF FG_ACT.CODE=FG_ABT.CODE THEN PATCH.FLD=LAST.FLD ELSE PATCH.FLD-=1
                END
            END
!
            IF PATCH.FLD=PR.FLD THEN
                CRT @(0,22):BG:'PR:':FG:; L=60; Z=SPRNO; CRT @(15,22):; GOSUB INPT
                IF NOT(FG_ACT.CODE) THEN
                    SPRNO=Z
                END ELSE
                    IF FG_ACT.CODE=FG_ABT.CODE THEN PATCH.FLD=LAST.FLD ELSE PATCH.FLD-=1
                END
            END
        UNTIL PATCH.FLD>=PR.FLD DO REPEAT
        IF PATCH.FLD<LAST.FLD THEN
            IF PATCH.FLD=PR.FLD THEN
                FilePath = FULLFLNM
                IF UPDHIST THEN
                    IF NOT(SRC_UPDATEHIST(FLNM, ITNM, DATE(), PSTIME, PETIME, PDESC)) THEN
                        CRT MSG.CLR:'Warning: history could not be updated':; GOSUB INPT
                    END
                END
                IF OPEN.PATCH THEN
                    PATCH.ITEM=REC
                    MATWRITE PATCH ON F.PATCHFILE,K.PATCHFILE
                END
                result = SRC_CHECKOUT(TRUE, FLNM, ITNM)
            END
COMPILE:    !
            IF PATCH.FLD>=PR.FLD THEN
                MSG = '(N)o'
                make = @FALSE
                IF TYPE EQ 'MAKE' THEN
                    make = @TRUE
                END ELSE
                    IF GETCWD(currdir) THEN
                        l = MINIMUM(LEN(FLNM):@AM:LEN(currdir))
                        IF FLNM[1,l] EQ currdir[1, l] THEN
                            READ dummy FROM F.currdir,'Makefile' THEN
                                make = @TRUE
                            END
                        END
                    END
                END
                IF make THEN
                    MSG := '(M)ake'
                END
                IF TYPE='BASIC' OR TYPE='RECOMPILE' OR TYPE='DEBUG' OR TYPE='SCRN' OR TYPE='SQL' THEN
                    MSG<-1> ='(B)asic, (C)atalog, (A)ll, (P)hantom'
                    MSG = CHANGE(MSG, @AM, ', ')
                END
                IF LEN(MSG) THEN
                    IF (FG_TYPEAHEAD.BUFF) THEN
                        COMPILE.IT = FIELD(FG_TYPEAHEAD.BUFF, CR, 1)
                        FG_TYPEAHEAD.BUFF = ''
                    END ELSE
                        COMPILE.IT='Y'
                        INPTYPE='U'
                        CRT @(0,PDEPTH):BG:
!                        MSG='Compile (N)o/[(Y)es, Basic (O)nly, (C)atalog Only'
!                        IF TYPE='SQL' OR COMMENT='--' THEN MSG := ']' ELSE MSG := '/(B)ackground]'
                        IF FG_OSTYPE='AP' THEN
                            REL=SYSTEM(100)
                            IF INDEX(REL,';',1) THEN
                                REL=OCONV(REL,'G6;1')
                                REL=OCONV(REL,'G.2')
                            END ELSE REL=OCONV(REL,'G1.2')
                        END ELSE REL=0
                        IF REL>6.1 THEN
                            MSG := '{F}'
                            L=2
                        END ELSE L=1
!                        MSG := ')'
                        COL=LEN(MSG)+1
                        Z=COMPILE.IT
                        YNCHRS=COMPILE.IT
                        letters = MSG
                        MSG = ''
                        LOOP
                            POS = INDEX(letters,'(',1)
                        WHILE POS DO
                            letter = letters[POS+1,1]
                            MSG := letters[1,POS]:RVON:letter:RVOFF:
                            letters=letters[POS+2,999]
                            LOCATE letter IN YNCHRS<1> SETTING POS ELSE
                                INS letter BEFORE YNCHRS<1,POS>
                            END
                        REPEAT
                        MSG := letters
                        CRT BG:MSG:FG:CLEOL:
                        YNL=1; GOSUB GET.CHAR
                        IF NOT(FG_ACT.CODE) THEN
                            COMPILE.IT=Z
                            IF Z EQ 'M' THEN TYPE='MAKE'
                            CRT
                        END ELSE
                            IF FG_ACT.CODE=FG_ABT.CODE THEN
                                COMPILE.IT=''
                                PATCH.FLD=LAST.FLD
                            END ELSE
                                IF TYPE='DEBUG' THEN PATCH.FLD=TYPE.FLD ELSE PATCH.FLD=PR.FLD
                                GO RESTART
                            END
                        END
                    END
                END
            END
        END
!
        IF PATCH.FLD=LAST.FLD THEN
            ENCRYPTED=''
            CRT MSG.CLR:
            SCR.UD=1
            SKIP.PATCH=TRUE
        END
    END ELSE
        TYPE='BASIC'
        PATCH.FLD=COMP.FLD
        GO COMPILE
    END
    RETURN
INPT: !
    POS=1
    EDITED=FALSE
1550 !
    CALL EB_UT_WP(Z,INPTYPE,L,0,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',ESC)
    IF INPTYPE='U' THEN Z=OCONV(Z,'MCU')
    INPTYPE='AN'
    RETURN
GET.CHAR: !
    LOOP
        CALL EB_UT_INPUT_ZERO(Z,MAT EB_CHARS,FG_ACT.CODE,COL,PDEPTH,FG_INPUT.CODES,YNCHRS,YNL,FG_TIMEOUT:AM:FG_MONITOR.SECS)
    UNTIL LEN(Z) OR FG_ACT.CODE DO REPEAT
    RETURN
