    SUBROUTINE EB_VERSION(Re_Read_Flag)
!
    INCLUDE EB.EQUS EB.COMMON
    INCLUDE JBC.h
    DEFFUN SRC_SRC_STATUS()
    DEFFUN SRC_HISTORY()
    DEFFUN SRC_READ()
    DEFFUN SRC_GETHOMEPATH()
    DEFFUN SRC_CASE()
    DEFFUN SRC_OPENLOCKS()
    DEFFUN GETFLNM()
    DEFFUN GETFULLPATH()
    DEFFUN EBJSHOW()
!
    Re_Read_Flag = FALSE
    shell = @IM:'k'
    shellend = ' 2>&1'
    CRT MSG.CLR:
    FullPath = GETFULLPATH(FLNM)
    SourceStatus = SRC_SRC_STATUS(FullPath, ITNM)
    IF LEN(SourceStatus) THEN
        YNCHRS='D':VM:'H':VM:'P':VM:'R':VM:'S':VM:ESC
        IF NOT(SRC_OPENLOCKS(F.Locks)) THEN STOP 201,'SRC.LOCKS'
        Commit = TRUE
        IF FullPath NE SRC_GETHOMEPATH(FullPath) THEN
            K.Locks = SRC_CASE(FullPath:DIR_DELIM_CH:ITNM)
            READV users FROM F.Locks, K.Locks, 1 THEN Commit = FALSE
        END
        IF Commit THEN
            MSG = '(C)ommit, '
            YNCHRS<1, -1> = 'C'
        END ELSE MSG = ''
        MSG := '(D) to delete, (R)evert, (P)ull, (H)istory, (S)tash'
    END ELSE
        MSG = '(A)dd to source control ?'
        YNCHRS='A':VM:ESC
    END
    MSG := ' '
    CRT MSG.CLR:MSG:
    YNC=LEN(MSG); YNR=PDEPTH-1
    YNL=1; GOSUB GET.CHAR
    CRT MSG.AKN:
    Y=OCONV(Y,'MCU')
    IF Y NE '' THEN
        lockvar=TRUE
        SITNM=ITNM
        BEGIN CASE
            CASE Y EQ 'C'; Y=VersCommit
            CASE Y EQ 'A'; Y=VersAdd
            CASE Y EQ 'D'; Y=VersDelete
            CASE Y EQ 'H' OR Y EQ 'S'
                IF Y EQ 'S' THEN ITNM := @AM:'S'
                history = SRC_HISTORY(FullPath, ITNM, '')
                IF LEN(history) THEN
                    IF ORIG.REC NE REC THEN
                        Y = 'Warning: any changes will be lost. Continue? (Y)es, (N)o '
                        CRT MSG.CLR:Y
                        YNC=LEN(Y); YNR=(PDEPTH-1)
                        YNCHRS='Y':VM:'N':VM:ESC
                        YNL=1; GOSUB GET.CHAR
                        IF Y NE 'Y' THEN RETURN
                    END
                END ELSE
                    SCR.UD=1
                    errmsg = 'No previous history found'
                    GOSUB DISPLAY_ERROR
                END
                Fmts = 'L#10':SVM:'L#10':SVM:'D':SVM:'MT':SVM:'L#':PWIDTH-48
                ColHdrs = 'Rev':SVM:'User':SVM:'Date':SVM:'time':SVM:'Description'
                Attrs = 1:SVM:2:SVM:3:SVM:4:SVM:5
                REV = ''
                CALL EB_CHOICES(5,3,'',PDEPTH-5,'',history,REV,1,1,Attrs,Fmts,'Revision History':SVM:ColHdrs)
                IF REV THEN
                    REV = ',':REV
                    IF SRC_READ(FLNM, ITNM:REV, errmsg) THEN
                        IF REC = errmsg THEN
                            errmsg = 'Version the same as current record'
                            GOSUB DISPLAY_ERROR
                            SCR.UD=1
                        END
                    END ELSE
                        GOSUB DISPLAY_ERROR
                        SCR.UD=1
                    END
                    IF SCR.UD THEN
                        CALL EB_REFRESH
                        RETURN
                    END
                    diff = EBJSHOW('-c diff':shellend)
                    msg = '(C)ompare with current'
                    opts = 'C'
                    IF LEN(diff) THEN
                        msg<1, -1> = '(I)nsert ifdef differences'
                        opts<1, -1>='I'
                    END
                    msg<1, -1>='(R)esume editing current'
                    opts<1, -1>='R'
                    msg = CHANGE(msg, @VM, ', ')
                    CRT @(-1)
                    CRT MSG.CLR:msg:' ?':
                    YNC=LEN(msg) + 2; YNR=(PDEPTH-1)
                    YNCHRS=opts:VM:ESC
                    YNL=1; GOSUB GET.CHAR
                    CRT MSG.AKN:
                    Y=OCONV(Y,'MCU')
                    WRITE REC ON FIL,ITNM:'.tmp'
                    WRITE errmsg ON FIL,ITNM:REV
                    BEGIN CASE
                        CASE Y='R'; Y=FALSE
                        CASE Y='C'
                            ECHO ON
                            DATA '','',''
                            EXECUTE 'COMPARE_ITEM ':FLNM:DIR_DELIM_CH:ITNM:REV:' ':FLNM:DIR_DELIM_CH:ITNM:'.tmp (T'
                            READ REC FROM FIL,ITNM:'.tmp' THEN
                                SCR.UD=1
                                CALL EB_REFRESH
                            END
                        CASE Y='I'
                            cmd = 'diff -Drev_':REV[2,9]
                            path = CHANGE(FullPath, DIR_DELIM_CH, '/')
                            cmd := ' ':path:'/':ITNM:REV
                            cmd := ' ':path:'/':ITNM:'.tmp'
                            cmd := '>':path:'/':ITNM:'_diff'
                            EXECUTE shell:cmd:shellend CAPTURING IO
                            ITNM=SITNM
                            READ REC FROM FIL,ITNM:'_diff' THEN
                                DELETE FIL,ITNM:'_diff'
                                SCR.UD=1
                                CALL EB_REFRESH
                            END
                            Re_Read_Flag = FALSE
                            RETURN
                        CASE Y='N'
                            REC = errmsg
                            Y = FALSE
                            ITNM:= REV
                    END CASE
                    DELETE FIL,ITNM:'.tmp'
                    DELETE FIL,ITNM:REV
                    Y=VersSync
                    Y = FALSE
                END ELSE
                    IF LEN(history) THEN
                        SCR.UD = 1
                        CALL EB_REFRESH
                    END
                END
            CASE Y='R'; Y=VersRevert
            CASE Y='P'; Y=VersSync
            CASE 1; Y=FALSE
        END CASE
        IF Y THEN
            save_lockvar = lockvar
            CALL EB_VERS_CTRL(Y,lockvar, TRUE)
            IF Y=VersRevert OR Y=VersSync OR Y=VersCommit THEN
                ITNM=SITNM
                Re_Read_Flag = (lockvar = save_lockvar)
                RETURN
            END
        END
    END
    RETURN
!
DISPLAY_ERROR:
!
    CALL EB_ERRMSG(errmsg, TRUE)
    IF SCR.UD THEN CALL EB_REFRESH
    RETURN
!
GET.CHAR: !
!
    CALL EB_UT_INPUT_ZERO(Y,MAT EB_CHARS,FG_ACT.CODE,YNC,YNR,FG_INPUT.CODES,YNCHRS,YNL,FG_TIMEOUT:AM:FG_MONITOR.SECS)
    RETURN
