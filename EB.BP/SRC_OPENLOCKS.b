    FUNCTION SRC_OPENLOCKS(F.Locks)
    INCLUDE EB.EQUS EB.COMMON
!
! OPEN 'SRC.LOCKS' and pass back the file variable
!
! Returns 1 if succussful, 0 if not
!
    INCLUDE JBC.h
    DEFFUN EBGETHOME()
    fname = 'SRC.LOCKS'
!
    CALL EB_OPEN('',EBGETHOME():fname,F.Locks,0,POS)
    IF NOT(POS) THEN
        rc = GETENV('JBCRELEASEDIR', jbcreleasedir)
        fname = jbcreleasedir:DIR_DELIM_CH:'config':DIR_DELIM_CH:fname
        CALL EB_OPEN('',fname,F.Locks,0,POS)
    END
!
    RETURN POS
