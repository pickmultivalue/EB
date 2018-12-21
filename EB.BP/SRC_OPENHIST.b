    FUNCTION SRC_OPENHIST(F.Hist, FileName)
!
! OPEN 'SRC.HISTORY' and pass back the file variable
!
! If FileName is blank then the global SRC.HISTORY is
! opened otherwise FileName,SRC.HISTORY is opened
! (i.e. the currently checked out file)
!
! 27 JAN 2010 Peter Falson (jBASE)
!
! Returns 1 if succussful, 0 if not
!
    INCLUDE JBC.h
    DEFFUN SRC_OPENHIST()
    DEFFUN EBGETHOME()

    IF LEN(FileName) THEN
        fname = FileName:','
    END ELSE fname = EBGETHOME()
!
    histfile = 'SRC.HISTORY'
    fname := histfile
!
    OPEN fname TO F.Hist ELSE
!
! If it can't be opened via JEDIFILEPATH or MD then
! last place to look is in jbcreleasedir/config
!
        rc = GETENV('JBCRELEASEDIR', jbcreleasedir)
        fname = jbcreleasedir:DIR_DELIM_CH:'config':DIR_DELIM_CH:histfile
        OPEN fname TO F.Hist ELSE
            IF LEN(FileName) THEN
                RETURN SRC_OPENHIST(F.Hist, '')
            END ELSE RETURN 0
        END
    END
    RETURN 1
