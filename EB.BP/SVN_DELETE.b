    FUNCTION SVN_DELETE(RemoveFlag, FilePath, ItemName)
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
    DEFFUN SVN_EXEC()
    DEFFUN SVN_SRC_STATUS()
    EQU TRUE TO 1, FALSE TO 0
    INCLUDE EB.INCLUDES SRC_DEBUG
!
    IO = SVN_SRC_STATUS(FilePath, ItemName)
    IF FIELD(IO, ' ',1) = 'A' THEN
        cmd = 'revert'
    END ELSE
        cmd = 'delete'
        IF RemoveFlag THEN cmd := ' --force'
    END
    IO = SVN_EXEC(cmd:' ':FilePath:'/':ItemName, TRUE)
!
    IF cmd = 'revert' AND RemoveFlag THEN   ;! try to delete it now
        EXECUTE 'DELETE ':FilePath:' ':ItemName CAPTURING io
        IF LEN(io) THEN IO<-1> = io
    END
!
    RETURN IO
