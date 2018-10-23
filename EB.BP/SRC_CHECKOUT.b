    FUNCTION SRC_CHECKOUT(Ask, FilePath, ItemName)
!
! Function to make a copy of a program to a user's home directory.
! Originally copied from SVN_CHECKOUT so may need work.
! The copy then becomes the user's working copy of the program
! When the user check's it in it moves it to the original location
! and issues an git commit to update the git repository
!
! Function returns null if successful.
! Anything else is an error message
!
! 30 NOV 2009 Peter Falson (jBASE)
!
    DEFFUN GETSRCTYPE()
    DEFFUN GIT_CHECKOUT()
    DEFFUN SVN_CHECKOUT()
    scType = GETSRCTYPE()
    BEGIN CASE
        CASE scType = 'GIT'
            RETURN GIT_CHECKOUT(Ask, FilePath, ItemName)
        CASE scType = 'SVN'
            RETURN SVN_CHECKOUT(Ask, FilePath, ItemName)
    END CASE
