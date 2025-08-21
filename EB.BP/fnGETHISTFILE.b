    FUNCTION fnGETHISTFILE(FileName)
!
! get the 'SRC.HISTORY'
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
            fname = EBGETHOME():DIR_DELIM_CH:histfile
            OPEN fname ELSE fname = ''
        END
    END
    RETURN fname
