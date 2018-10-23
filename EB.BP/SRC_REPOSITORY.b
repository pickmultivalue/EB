    FUNCTION SRC_REPOSITORY(FilePath)
! * COPIED from SVN program
!
! Return the git repository
!
    DEFFUN GETFULLPATH()
    DEFFUN SRC_EXEC()
    EQU TRUE TO 1, FALSE TO 0
!
    INCLUDE EB.INCLUDES SRC_DEBUG
    IO = SRC_EXEC('info ':GETFULLPATH(FilePath), TRUE)
!
! Using the URL and Repository Root we can derive the current
! path
!
    git = IO<1,1,1>
    loc = 0
    LOOP
        REMOVE line FROM IO AT loc SETTING delim
        IF FIELD(line, ':', 1) = 'Repository Root' THEN
            git = line[COL2()+2, 99]
            delim = 0
        END
    WHILE delim DO REPEAT
    RETURN git
