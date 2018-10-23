    FUNCTION SRC_OPENLOCKS(F.Locks)
!
! OPEN 'SRC.LOCKS' and pass back the file variable
!
! Returns 1 if succussful, 0 if not
!
    INCLUDE JBC.h
    DEFFUN EBGETHOME()
    fname = 'SRC.LOCKS'
!
    OPEN EBGETHOME():fname TO F.Locks ELSE
        rc = GETENV('JBCRELEASEDIR', jbcreleasedir)
        fname = jbcreleasedir:DIR_DELIM_CH:'config':DIR_DELIM_CH:fname
        OPEN fname TO F.Locks ELSE
            RETURN 0
        END
    END
!
    RETURN 1
