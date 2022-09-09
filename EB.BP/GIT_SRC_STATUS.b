    FUNCTION GIT_SRC_STATUS(FileName, ItemName)
    COMMON /GIT_SRC_STATUS/ lockvars, results
    INCLUDE JBC.h
!
! Return the git status of given file/item
!
    DEFFUN GIT_EXEC()
    K.lockvar = FileName:' ':ItemName
    LOCATE K.lockvar IN lockvars SETTING pos THEN
        RETURN results<pos>
    END
    INS K.lockvar BEFORE lockvars<pos>
    INCLUDE EB.INCLUDES SRCDBG
    root = CHANGE(FileName, DIR_DELIM_CH, @AM)
    dc = DCOUNT(root, @AM)
    fName = root<dc>
    K.lockvar = ItemName
    root = CHANGE(root, @AM, DIR_DELIM_CH)
    shell = @IM:'k'
    shellend = ' 2>&1'
!    EXECUTE shell:'cd ':root:' && git ls-files ':K.lockvar:shellend CAPTURING IO
!    IF LEN(IO) THEN
    EXECUTE shell:'cd ':root:' && git status ':K.lockvar:shellend CAPTURING IO
    IF INDEX(IO,'untracked',1) THEN
        IO = ''
    END
!    END
    INS IO BEFORE results<pos>
    RETURN IO
    RETURN GIT_EXEC('status -v ':CHANGE(FileName,'\', '/'):'/':ItemName, 1)
