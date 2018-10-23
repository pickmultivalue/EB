    FUNCTION SRCPUSH
!
! Utility to do git push (or other...later)
!
! 17 OCT 2018 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    shell = @IM:'k'
    shellend = ' 2>&1'
!
    INCLUDE EB.INCLUDES SRC_DEBUG
!
    DEFFUN GETSRCTYPE()
    scType = GETSRCTYPE()
    io = ''
    BEGIN CASE
        CASE scType = 'GIT'
            EXECUTE shell:'git push ':shellend CAPTURING io
    END CASE
    RETURN io
