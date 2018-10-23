    FUNCTION SRC_ADD(FullPaths)
!
! Function to add a program to the source control repository
!
! Function returns null if successful.
! Anything else is an error message
!
! 01 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN GIT_ADD()
    DEFFUN SVN_ADD()
    DEFFUN GETSRCTYPE()
    scType = GETSRCTYPE()
    BEGIN CASE
        CASE scType = 'GIT'
            RETURN GIT_ADD(FullPaths)
        CASE scType = 'SVN'
            RETURN SVN_ADD(FullPaths)
    END CASE
