! PROGRAM GITDELETE
!
! Utility to revert one or more items to GIT
!
! 03 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN GIT_DELETE()
    DEFFUN SRC_SENTENCE()
    DEFFUN GETYN()
    EQU TRUE TO 1, FALSE TO 0
    shell = @IM:'k'
    shellend = ' 2>&1'
!
    INCLUDE EB.INCLUDES SRCDBG
    INCLUDE EB.INCLUDES GET.HOME
!
    FullPaths = SRC_SENTENCE(TRUE, FALSE)
!
    IF LEN(FullPaths) THEN
        CRT
        CRT 'The following paths will be deleted:'
        CRT
        loc = 0
        LOOP
            REMOVE FullPath FROM FullPaths AT loc SETTING delim
            CRT FullPath
        WHILE delim DO REPEAT
        CRT
        ANS = GETYN('Continue', 'N', 2)
        IF ANS = 'Y' THEN
            loc = 0
            LOOP
                REMOVE FullPath FROM FullPaths AT loc SETTING delim
                CALL SPLITFILEPATH(FullPath, FilePath, ItemName)
                CRT GIT_DELETE(TRUE, FilePath, ItemName)
            WHILE delim DO REPEAT
        END
    END
