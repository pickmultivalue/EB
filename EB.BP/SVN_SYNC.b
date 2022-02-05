    FUNCTION SVN_SYNC(FullPaths)
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
    DEFFUN SVN_EXEC()
    DEFFUN SVN_REPOSITORY()
    DEFFUN SVN_GET_REPOSITORY()
    DEFFUN SVN_GETHOMEPATH()
    EQU TRUE TO 1, FALSE TO 0
!
    INCLUDE EB.INCLUDES SRCDBG
    subversion = SVN_REPOSITORY(FullPaths<1>)
!
! First get user's home dir
!
    shell = CHAR(255):'k'
    shellend = ' 2>&1'
    INCLUDE EB.INCLUDES GET.HOME
!
! Now make the target directory
!
    nbr_items = DCOUNT(FullPaths, @AM)
    FOR i = 1 TO nbr_items
        FullPath = FullPaths<i>
        CALL SPLITFILEPATH(FullPath, FilePath, ItemName)
        homedir = SVN_GETHOMEPATH(FilePath)
!
! Now grab a copy of the program
!
        FileName = GETFLNM(FilePath)
        K.Locks = FileName:DIR_DELIM_CH:ItemName
!
! Make sure we at least have the working directory
!
        Repository = SVN_GET_REPOSITORY(FileName)
        repo = subversion:'/':Repository:'/':FileName
        IO = SVN_EXEC('checkout --depth=empty ':repo:' ':homedir, TRUE)
        IF NOT(INDEX(IO, 'revision', 1)) THEN
        RETURN IO
        END
!
        revision = FIELD(ItemName, ',', 2)
        IF revision THEN
        revision = '-r ':revision:' '
        ItemName = ItemName[1, COL1() - 1]
        END
        target = homedir:'/':ItemName
        svn_args = 'update ':revision:target
        cmd = 'svn ':svn_args
        IO = SVN_EXEC(svn_args, TRUE)
        IF NOT(INDEX(IO, 'revision', 1)) THEN
        IF NOT(LEN(IO)) THEN
        IO = 'Error syncing:'
        IO<3> = cmd
        END
        RETURN IO
        END
    NEXT i
!
    RETURN ''
