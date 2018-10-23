    FUNCTION GIT_SRC_STATUS(FileName, ItemName)
    INCLUDE JBC.h
!
! Return the git status of given file/item
!
    DEFFUN GIT_EXEC()
    INCLUDE EB.INCLUDES SRC_DEBUG
    root = CHANGE(FileName, DIR_DELIM_CH, @AM)
    dc = DCOUNT(root, @AM)
    fName = root<dc>
    K.lockvar = ItemName
    root = CHANGE(root, @AM, DIR_DELIM_CH)
    shell = @IM:'k'
    shellend = ' 2>&1'
    EXECUTE shell:'cd ':root:' && git ls-files ':K.lockvar:shellend CAPTURING IO
    IF LEN(IO) THEN
        EXECUTE shell:'cd ':root:' && git status ':K.lockvar:shellend CAPTURING IO
    END
    RETURN IO
    RETURN GIT_EXEC('status -v ':CHANGE(FileName,'\', '/'):'/':ItemName, 1)
