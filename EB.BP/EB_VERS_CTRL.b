    SUBROUTINE EB_VERS_CTRL(Op, lockvar, silent)
!
! Check out item from source control
!
    INCLUDE EB.EQUS EB.COMMON
    INCLUDE JBC.h
    DEFFUN EB_CLOSE()
    DEFFUN SRC_CASE()
    DEFFUN SRC_CHECKOUT()
    DEFFUN SRC_COMMIT()
    DEFFUN SRC_ADD()
    DEFFUN SRC_SYNC()
    DEFFUN SRC_DELETE()
    DEFFUN SRC_REVERT()
    DEFFUN SRC_GETORIGPATH()
    DEFFUN SRC_GETHOMEPATH()
    DEFFUN SRC_OPENLOCKS()
    DEFFUN SRC_SRC_STATUS()
    DEFFUN SRC_EXEC()
    DEFFUN GETFLNM()
    DEFFUN GETFULLPATH()
    DEFFUN GET_CATALOG_FILE()
    DEFFUN GETYN()
    DEFFUN EBJSHOW()
!
    EQU spc TO ' '
    shell = @IM:'k'
    shellend = ' 2>&1'
    INCLUDE EB.INCLUDES SRCDBG
!
! FLNM is (typically) the "BP" file
!
    IF INDEX(FLNM,DIR_DELIM_CH,1) THEN
        FilePath=FLNM
        BP_FILE = GETFLNM(FLNM)
    END ELSE
        BP_FILE = FLNM
        FilePath = EBJSHOW(' -f ':FLNM)
        FilePath=OCONV(FilePath, 'MCP ')
        FilePath=TRIM(FilePath)
        FilePath=FIELD(FilePath,spc,2,99)
    END
    IF GETENV('pwd',currdir) ELSE
        IF GETENV('PWD',currdir) THEN NULL
    END
    IF FilePath[1,2]=('.':DIR_DELIM_CH)[1,LEN(FilePath)] THEN
        FilePath=currdir:FilePath[2,99]
    END
    FullPath=FilePath:DIR_DELIM_CH:ITNM
    BEGIN CASE
!
! Find out the status of this item
! i.e. is in the source repository?
!      is it currently being worked on by somebody?
!
        CASE Op=VersStat
            lockvar = FALSE
            IF INDEX(FilePath, 'JET.PASTE', 1) OR ITNM MATCHES "'%'1X0X'%'" THEN RETURN
!
! Get unix login's home dir
!
            INCLUDE EB.INCLUDES GET.HOME
            BP_DIR = SRC_GETHOMEPATH(BP_FILE)
            IF BP_DIR = FilePath THEN     ;! already in user's home
                RETURN
            END
!
! Find out if anyone else is working on it and warn user
!
            IF NOT(SRC_OPENLOCKS(F.VAR)) THEN STOP 201,'SRC.LOCKS'
            K.lockvar = GETFULLPATH(BP_FILE):DIR_DELIM_CH:ITNM
            IO = TRIM(SRC_SRC_STATUS(FilePath, ITNM))
            gitinfo = SRC_EXEC('info ':K.lockvar, TRUE)
            lockvar = LEN(gitinfo) = 0
            RETURN
            LocalCopy = 'Make local copy'
            READV lockvar FROM F.VAR, SRC_CASE(K.lockvar), 1 THEN
                CALL EB_CHOICES(10,3,'','',VM:LocalCopy,lockvar,ANS,1,'',1,'L#50','Warning - Currently being worked on by')
            END ELSE ANS = LocalCopy
            IF ANS = LocalCopy THEN
                IO = TRIM(SRC_SRC_STATUS(FilePath, ITNM))
                IF LEN(IO) AND NOT(INDEX(IO[1,2], 'M', 1)) THEN
                    lockvar = ''
                END ELSE
                    Z='Y'; L=1
                    EXECUTE shell:'ls -l ':FullPath:'|grep rw' CAPTURING lockvar
                END
                lockvar=NOT(LEN(lockvar))*24576
            END ELSE
                CRT MSG.CLR:"Confirm edit of ":ANS:"'s version of ":K.lockvar:" (Y/N) ":
                Z = TRUE; L = 1; INPTYPE='YN'; GOSUB INPT
                IF Z THEN
                    FLNM = SRC_GETHOMEPATH(BP_FILE)
                    IF ANS NE user THEN
                        EXECUTE shell:'echo ~':ANS CAPTURING userhome
                        IF userhome = '~':ANS THEN userhome = CHANGE(homepath, DIR_DELIM_CH:user, DIR_DELIM_CH:ANS)
                        FLNM = CHANGE(FLNM, homepath, userhome)
                        lockvar = FALSE     ;! no writes
                    END ELSE lockvar = -1
                END ELSE lockvar = TRUE       ;! locked no check out
            END
        CASE Op=VersEdit
!            IO = TRIM(SRC_SRC_STATUS(FilePath, ITNM))
!            IF FIELD(IO, ' ', 1) MATCHES "1N0N" THEN          ;! under source control
            EXECUTE shell:'git ls-files ':K.lockvar:shellend CAPTURING IO
            IF LEN(IO) THEN
                Z=TRUE; L=1
                IF NOT(silent) THEN
                    CRT MSG.CLR:
                    Z = INDEX('NY',GETYN('Check out from Source Control (Y/N) ? ','Y',1),1)-1
                END
                lockvar=TRUE
                IF NOT(Z) THEN RETURN
                IF SRC_CASE(FilePath) = SRC_CASE(currdir) THEN
                    Z = '.'
                END ELSE Z = FilePath
                errmsg = SRC_CHECKOUT(FALSE, Z, ITNM)
                IF LEN(errmsg) THEN
                    CALL EB_ERRMSG(errmsg, 1)
                    SCR.UD = 1
                    CALL EB_REFRESH
                    lockvar = FALSE     ;! -1 ;! force re-read
                END ELSE FLNM = Z
            END
            RETURN
        CASE Op=VersAdd
            Z=TRUE; L=1
            IF NOT(silent) THEN
                CRT MSG.CLR:'Add source to Source Control (Y/N) ? ':
                INPTYPE='YN'; L = 1; GOSUB INPT
            END
            lockvar=TRUE
            IF NOT(Z) THEN RETURN
            FullPath = FilePath:DIR_DELIM_CH:ITNM
            errmsg = SRC_ADD(FullPath)
            IF LEN(errmsg) THEN
                CALL EB_ERRMSG(errmsg, 1)
                CALL EB_REFRESH
                RETURN
            END
            CALL SPLITFILEPATH(FullPath, FilePath, ITNM)
            GOSUB OPEN_FILE
            lockvar=FALSE
        CASE Op=VersDelete
            Z=TRUE; L=1
            IF NOT(silent) THEN
                CRT MSG.CLR:'Delete source from Source Control (Y/N) ? ':
                INPTYPE='YN'; L=1; GOSUB INPT
            END
            lockvar=TRUE
            IF NOT(Z) THEN RETURN
            errmsg = SRC_DELETE(TRUE, FilePath, ITNM)
            IF FIELD(TRIM(errmsg), ' ', 1) NE 'D' THEN
                CALL EB_ERRMSG(errmsg, 1)
                CALL EB_REFRESH
                RETURN
            END
            lockvar=FALSE
        CASE Op=VersRevert
            Z=TRUE; L=1
            IF NOT(silent) THEN
                CRT MSG.CLR:'Revert check-out from Source Control (Y/N) ? ':
                INPTYPE='YN'; L=1; GOSUB INPT
            END
            savelockvar = lockvar
            lockvar=TRUE
            IF NOT(Z) THEN RETURN
            FullPath = FilePath:DIR_DELIM_CH:ITNM
            errmsg = SRC_REVERT(FullPath)
            IF NOT(LEN(errmsg)) THEN errmsg = 'No revert took place'
            IF FIELD(TRIM(errmsg), ' ', 1) NE 'Reverted' THEN
                CALL EB_ERRMSG(errmsg, 1)
                SCR.UD=1
                CALL EB_REFRESH
                RETURN
            END
            CALL SPLITFILEPATH(FullPath, FilePath, ITNM)
            GOSUB OPEN_FILE
            lockvar = savelockvar
        CASE Op=VersCommit
            FullPath = FilePath:DIR_DELIM_CH:ITNM
            INCLUDE EB.INCLUDES PWD
!            IF pwd = FilePath[1, LEN(pwd)] THEN
!                errmsg     = 'Warning: you are currently in the directory for ':ITNM:'.'
!                errmsg<-1> = 'Committing the last item in a checked out file will attempt'
!                errmsg<-1> = 'to remove the file which will incur an error'
!                CALL EB_ERRMSG(errmsg, 1)
!!
!                CRT MSG.CLR:'Continue (Y/N) ? ':
!                INPTYPE='YN'; L=1; GOSUB INPT
!                IF NOT(Z) THEN RETURN
!            END
!
! First close all the files so commit can clean up (if relevant)
!
            IF EB_CLOSE('', FilePath) THEN
                CLOSE FIL
                CLOSE SFIL
                CLOSE HFIL
            END ELSE
                errmsg = 'Warning: failed to find ':FilePath:' in EB file array'
                errmsg<-1> = @AM:FOLD(CHANGE(EB.FILE.LIST<1>, @VM, ', '), LEN(errmsg))
                CALL EB_ERRMSG(errmsg, 1)
                CRT MSG.CLR:
            END
            errmsg = SRC_COMMIT(FullPath)
            CALL SPLITFILEPATH(FullPath, FilePath, ITNM)
            IF LEN(errmsg) THEN
                SCR.UD=1
                CALL EB_ERRMSG(errmsg, 1)
            END
            GOSUB OPEN_FILE
            CALL EB_SETUPSWITCH(HFLNM, SFLNM)
            IF FIELD(FIELD(errmsg, ' ', 1):':', ':', 1) = 'Error' THEN
                IF SCR.UD THEN CALL EB_REFRESH
                RETURN
            END
        CASE Op=VersSync
            Z=TRUE; L=1
            IF NOT(silent) THEN
                CRT MSG.CLR:'Sync source from Source Control (Y/N) ? ':
                INPTYPE='YN'; L=1; GOSUB INPT
            END
            lockvar=TRUE
            IF NOT(Z) THEN RETURN
            errmsg = SRC_SYNC(FilePath:DIR_DELIM_CH:ITNM)
            IF LEN(errmsg) THEN
                CALL EB_ERRMSG(errmsg, 1)
                SCR.UD = 1
                CALL EB_REFRESH
                lockvar = -lockvar
            END
            lockvar=FALSE
    END CASE
    RETURN
OPEN_FILE:
    CALL EB_OPEN('', FilePath, FIL, TRUE, POS)
    FLNM = FilePath
    RETURN
INPT: !
    POS=1
    EDITED=FALSE
    CALL EB_UT_WP(Z,INPTYPE,L,1,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',ESC)
    INPTYPE='AN'
    RETURN
