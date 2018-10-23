    FUNCTION SRC_GETORIGPATH(FilePath)
!
! Return the original path of a given file
!
! 03 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN GETSRCTYPE()
    DEFFUN GIT_GETORIGPATH()
    DEFFUN SVN_GETORIGPATH()
    scType = GETSRCTYPE()
    BEGIN CASE
        CASE scType = 'GIT'
            RETURN GIT_GETORIGPATH(FilePath)
        CASE scType = 'SVN'
            RETURN SVN_GETORIGPATH(FilePath)
    END CASE
