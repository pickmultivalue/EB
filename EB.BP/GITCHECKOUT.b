! PROGRAM GITCHECKOUT
!
! Command line interface for GIT_CHECKOUT
! 25 JAN 2010 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    EQU TRUE TO 1, FALSE TO 0
    DEFFUN GIT_CHECKOUT()
    DEFFUN SRC_SENTENCE()
    INCLUDE EB.INCLUDES SRC_DEBUG
!
    FullPaths = SRC_SENTENCE(FALSE, FALSE)
!
    IF LEN(FullPaths) THEN
        loc = 0
        LOOP
            REMOVE FullPath FROM FullPaths AT loc SETTING delim
            CALL SPLITFILEPATH(FullPath, FilePath, ItemName)
            CRT 'Checking out ':FullPath:' ':GIT_CHECKOUT(FALSE, FilePath, ItemName)
        WHILE delim DO REPEAT
    END
