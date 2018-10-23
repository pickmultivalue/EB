    FUNCTION SRC_REVERT(FullPaths)
!
! Function to add a program to the source control repository
!
! Function returns null if successful.
! Anything else is an error message
!
! 01 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN GETSRCTYPE()
    DEFFUN GIT_REVERT()
    DEFFUN SVN_REVERT()
    scType = GETSRCTYPE()
    BEGIN CASE
        CASE scType = 'GIT'
            RETURN GIT_REVERT(FullPaths)
        CASE scType = 'SVN'
            RETURN SVN_REVERT(FullPaths)
    END CASE
