    FUNCTION SVN_SRC_STATUS(FileName, ItemName)
!
! Return the subversion status of given file/item
!
    DEFFUN SVN_EXEC()
    INCLUDE EB.INCLUDES SRC_DEBUG
    RETURN SVN_EXEC('status -v ':CONVERT(FileName,'\', '/'):'/':ItemName, 1)
