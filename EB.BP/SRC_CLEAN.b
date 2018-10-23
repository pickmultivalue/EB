    FUNCTION SRC_CLEAN(HomeDir, F.Source, ItemName)
!
! * COPIED from SVN program
! Program designed to clean up a user's home/dir
! directory after committing or removing
! item from a previous git check out
! F.Source is assumed to be opened to the /home/<user>/dir
!
! Returns the original source path
!
! 02 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN GETSRCTYPE()
    DEFFUN GIT_CLEAN()
    DEFFUN SVN_CLEAN()
    scType = GETSRCTYPE()
    BEGIN CASE
        CASE scType = 'GIT'
            RETURN GIT_CLEAN(HomeDir, F.Source, ItemName)
        CASE scType = 'SVN'
            RETURN SVN_CLEAN(HomeDir, F.Source, ItemName)
    END CASE
