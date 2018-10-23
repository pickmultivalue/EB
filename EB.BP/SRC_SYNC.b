    FUNCTION SRC_SYNC(FullPaths)
!
! Function to get either the latest copy or a revision of a program
! to the user's working directory.
!
! Function returns null if successful.
! Anything else is an error message
!
! 02 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN GETSRCTYPE()
    DEFFUN GIT_SYNC()
    DEFFUN SVN_SYNC()
    scType = GETSRCTYPE()
    BEGIN CASE
        CASE scType = 'GIT'
            RETURN GIT_SYNC(FullPaths)
        CASE scType = 'SVN'
            RETURN SVN_SYNC(FullPaths)
    END CASE
