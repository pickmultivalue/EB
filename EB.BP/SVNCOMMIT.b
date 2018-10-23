    PROGRAM SVNCOMMIT
!
! Utility to check in one or more items to SVN
!
! 03 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN SVN_COMMIT()
    DEFFUN SRC_SENTENCE()
    INCLUDE JBC.h
    EQU TRUE TO 1, FALSE TO 0
    shell = CHAR(255):'k'
    shellend = ' 2>&1'
    OPTIONS = UPCASE(SYSTEM(15))
!
    INCLUDE EB.INCLUDES SRC_DEBUG
    INCLUDE EB.INCLUDES GET.HOME
!
    FullPaths = SRC_SENTENCE(TRUE, FALSE)
!
    IF LEN(FullPaths) THEN
        IF INDEX(OPTIONS,'L',1) THEN        ;! list only
            CRT
            CRT 'The following are ready to be checked in:'
            CRT
            loc = 0
            LOOP
                REMOVE FullPath FROM FullPaths AT loc SETTING delim
                CRT FullPath
            WHILE delim DO REPEAT
        END ELSE
            CRT SVN_COMMIT(FullPaths)
        END
    END ELSE
        CRT
        CRT 'Additionally the (L option can be used to display'
        CRT 'the items waiting to be commited'
        CRT
    END
