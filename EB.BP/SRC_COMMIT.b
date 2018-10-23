    FUNCTION SRC_COMMIT(FullPaths)
!
! Utility to check in one or more items to SRC
!
! 03 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN GETSRCTYPE()
    DEFFUN GIT_COMMIT()
    DEFFUN SVN_COMMIT()
    scType = GETSRCTYPE()
    BEGIN CASE
        CASE scType = 'GIT'
            RETURN GIT_COMMIT(FullPaths)
        CASE scType = 'SVN'
            RETURN SVN_COMMIT(FullPaths)
    END CASE
