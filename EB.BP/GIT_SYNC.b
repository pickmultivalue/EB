    FUNCTION GIT_SYNC(FullPaths)
! * COPIED from SVN program
!
! Function to get either the latest copy or a revision of a program
! to the user's working directory.
!
! Function returns null if successful.
! Anything else is an error message
!
! 02 DEC 2009 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    DEFFUN GETFLNM()
    DEFFUN GIT_EXEC()
    DEFFUN GIT_REPOSITORY()
    DEFFUN GIT_GET_REPOSITORY()
    DEFFUN GIT_GETHOMEPATH()
    EQU TRUE TO 1, FALSE TO 0
!
    RETURN ;! disable for now
