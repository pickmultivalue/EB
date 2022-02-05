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
    INCLUDE EB.INCLUDES SRCDBG
!
    DEFFUN GETSRCTYPE()
    scType = GETSRCTYPE()
    io = ''
    BEGIN CASE
        CASE scType = 'GIT'
            OPEN '.' THEN
                K.push = 'git_push.':@PID
                FOR tries = 1 TO 3
                    EXECUTE shell:'git push >':K.push:' ':shellend
                    READ io FROM K.push ELSE io = 'Push error'
                    DELETE K.push
                    IF NOT(INDEX(io, 'ailed', 1)) THEN BREAK
                NEXT tries
            END
    END CASE
    RETURN io
