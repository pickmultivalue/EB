    FUNCTION SRC_SRC_STATUS(FileName, ItemName)
    INCLUDE JBC.h
!
! Return the git status of given file/item
!
    DEFFUN GETSRCTYPE()
    DEFFUN GIT_SRC_STATUS()
    DEFFUN SVN_SRC_STATUS()
    scType = GETSRCTYPE()
    BEGIN CASE
        CASE scType = 'GIT'
            RETURN GIT_SRC_STATUS(FileName, ItemName)
        CASE scType = 'SVN'
            RETURN SVN_SRC_STATUS(FileName, ItemName)
    END CASE
