    FUNCTION GIT_DELETE(RemoveFlag, FilePath, ItemName)
!
! Function to remove a program from the source control repository
! If the current status of the item is Add then a revert is issued
! instead and the item is deleted
!
! Function returns null if successful.
! Anything else is an error message
!
! 03 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN GIT_EXEC()
    DEFFUN GIT_SRC_STATUS()
    EQU TRUE TO 1, FALSE TO 0
    INCLUDE EB.INCLUDES SRCDBG
!
    IO = GIT_SRC_STATUS(FilePath, ItemName)
    IF IO EQ '' OR FIELD(IO, ' ',1) = 'A' THEN
        cmd = 'revert'
    END ELSE
        cmd = 'delete'
        IF RemoveFlag THEN cmd := ' --force'
    END
    IO = GIT_EXEC(cmd:' ':FilePath:'/':ItemName, TRUE)
!
    IF cmd = 'revert' AND RemoveFlag THEN   ;! try to delete it now
        EXECUTE 'DELETE ':FilePath:' ':ItemName CAPTURING io
        IF LEN(io) THEN IO<-1> = io
    END
!
    RETURN IO
