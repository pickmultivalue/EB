    FUNCTION SRC_DELETE(RemoveFlag, FilePath, ItemName)
!
! Function to remove a program from the source control repository
! If the current status of the item is Add then a revert is issued
! instead and the item is deleted
!
! Function returns null if successful.
! Anything else is an error message
!
! 03 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN GETSRCTYPE()
    DEFFUN GIT_DELETE()
    DEFFUN SVN_DELETE()
    scType = GETSRCTYPE()
    BEGIN CASE
        CASE scType = 'GIT'
            RETURN GIT_DELETE(RemoveFlag, FilePath, ItemName)
        CASE scType = 'SVN'
            RETURN SVN_DELETE(RemoveFlag, FilePath, ItemName)
    END CASE
    RETURN -1
