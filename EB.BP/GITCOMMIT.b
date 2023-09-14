    PROGRAM GITCOMMIT
    INCLUDE EB.EQUS EB.COMMON
!
! Utility to check in one or more items to GIT
!
! 03 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN GIT_COMMIT()
    DEFFUN SRC_SENTENCE()
    DEFFUN GETYN()
    DEFFUN SRCPUSH()
    INCLUDE JBC.h
    EQU TRUE TO 1, FALSE TO 0
    shell = @IM:'k'
    shellend = ' 2>&1'
    OPTIONS = UPCASE(SYSTEM(15))
    INCLUDE EB.EQUS EB.EQUS
    MAT EXTRAS=''
!
    INCLUDE EB.INCLUDES SRCDBG
    INCLUDE EB.INCLUDES GET.HOME
!
    FullPaths = SRC_SENTENCE(TRUE, TRUE)
    IF LEN(FullPaths) = 0 THEN
        EXECUTE shell:'git status':shellend CAPTURING io
        nbr = DCOUNT(io, @AM)
        FullPaths = ''
        FOR a = 1 TO nbr
            line = io<a>
            status = FIELD(line, ':', 1)
            IF INDEX(status, 'modified', 1) OR INDEX(status, 'new file', 1) OR INDEX(status, 'renamed', 1) THEN
                line = TRIM(line[COL2(), 99])
                FullPath = FIELD(line, ' ', 2)
                IF INDEX(status,'renamed',1) THEN
                    FullPath<1, -1> = FIELD(line, ' ', 4)
                END
                LOCATE FullPath IN FullPaths SETTING fpos ELSE
                    FullPaths<-1> = FullPath
                END
            END
        NEXT a
    END
!
    IF LEN(FullPaths) THEN
        IF INDEX(OPTIONS,'L',1) THEN        ;! list only
            CRT
            CRT 'The following are ready to be checked in:'
            CRT
            loc = 0
            lastDelim = 0
            LOOP
                REMOVE FullPath FROM FullPaths AT loc SETTING delim
                IF lastDelim = 3 THEN CRT ' - renamed to ':
                CRT FullPath
                lastDelim = delim
            WHILE delim DO REPEAT
        END ELSE
            result = GIT_COMMIT(FullPaths)
            IF LEN(result) THEN
                CRT result
                DEBUG
            END ELSE
                IF GETYN('Push to respository', '', 1) EQ 'Y' THEN
                    CRT SRCPUSH()
                END
            END
        END
    END ELSE
        CRT
        CRT 'Additionally the (L option can be used to display'
        CRT 'the items waiting to be commited'
        CRT
    END
