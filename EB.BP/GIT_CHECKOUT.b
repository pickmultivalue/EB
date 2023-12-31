    FUNCTION GIT_CHECKOUT(Ask, FilePath, ItemName)
    INCLUDE EB.EQUS EB.COMMON
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
    INCLUDE JBC.h
    DEFFUN GETYN()
    DEFFUN GETFLNM()
    DEFFUN GETFULLPATH()
    DEFFUN SRC_CASE()
    DEFFUN GIT_EXEC()
    DEFFUN GIT_REPOSITORY()
    DEFFUN GIT_GET_REPOSITORY()
    DEFFUN GIT_GETHOMEPATH()
    DEFFUN GIT_GETORIGPATH()
    DEFFUN SRC_OPENLOCKS()
    DEFFUN GIT_SRC_STATUS()
    DEFFUN GIT_CHECKOUT()
!
    IF NOT(SRC_OPENLOCKS(F.Locks)) THEN
        RETURN 'SRC.LOCKS is not a file name'
    END
    EQU TRUE TO 1, FALSE TO 0
    shell = @IM:'k'
    shellend = ' 2>&1'
    PROMPT ''
!
! First get user's home dir
!
    INCLUDE EB.INCLUDES GET.HOME
    INCLUDE EB.INCLUDES SRCDBG
!
! Check if a specific revision is required
!
    revision = FIELD(ItemName, ',', 2)
    IF revision THEN
        revision = ' -r ':revision
        tItemName = ItemName[1, COL1() - 1]
    END ELSE tItemName = ItemName
!
    IF Ask THEN
        GOSUB Ask_Checkout
        IF LEN(result) THEN RETURN result
    END
!
! Now make the target directory
!
    FileName = GETFLNM(FilePath)
    fullpath = FilePath:DIR_DELIM_CH:ItemName
    result = GIT_EXEC('add ': fullpath, TRUE)
    RETURN result
!
! First check that directory is under source control
!
    OrigPath = GIT_GETORIGPATH(FileName)
    Status_IO = GIT_EXEC('status -v --depth=empty ':OrigPath, TRUE)
    IF LEN(Status_IO) = 0 OR Status_IO[1,1] = 'I' THEN          ;! need to add this directory
        homedir = GIT_GETHOMEPATH('.'):DIR_DELIM_CH:FileName
        CALL EB_OPEN('',homedir,F.homedir,0,ok)
        IF ok THEN
            EXECUTE shell:'mkdir ':homedir:shellend CAPTURING IO
        END
        OrigPath = CHANGE(homedir, DIR_DELIM_CH, '/')
        Status_IO = GIT_EXEC('add --depth=empty ':OrigPath, TRUE)
        IF Status_IO[1,1] # 'A' THEN        ;! bailing out at this point
            Result = OrigPath:' git status error:'
            Result<3> = Status_IO
            RETURN Result
        END
        FilePath = homedir
    END
    homedir = GIT_GETHOMEPATH(FilePath)
!
! Now check the git status of mName/tItemNameItemName
!
    OrigPath := '/':tItemName
    Status_IO = GIT_EXEC('status -v ':OrigPath, TRUE)
    IF TRIM(Status_IO[1,1]) # '' THEN
        Result = tItemName:' error'
        Result<3> = 'git status output:'
        Result<5> = Status_IO
        RETURN Result
    END
!
! Special case for Windows
! - make sure the case of the itemname is correct
!
    IF LEN(Status_IO) THEN
        tItemName = FIELD(Status_IO, DIR_DELIM_CH, DCOUNT(Status_IO, DIR_DELIM_CH))
    END
!
! Set the SRC.LOCKS key
!
    K.Locks = SRC_CASE(GETFULLPATH(FileName):DIR_DELIM_CH:tItemName)
!
    CALL EB_OPEN('',homedir,F.homedir,0,ok)
    IF NOT(ok) THEN
!
! Get the Branch/Repository info
!
        Branch = GIT_REPOSITORY(FileName)
        Repository = GIT_GET_REPOSITORY(FileName)
!
! First checkout the root directory
!
        repo = Branch:'/':Repository
        CALL SPLITFILEPATH(homedir, homepath, homeitem)
        IO = GIT_EXEC('checkout --depth=empty ':repo:' ':homepath, TRUE)
        IF NOT(INDEX(IO, 'revision', 1)) THEN
            RETURN IO
        END
!
! Now checkout the filename
!
        repo := '/':FileName
        IO = GIT_EXEC('checkout --depth=empty ':repo:' ':homedir, TRUE)
        IF NOT(INDEX(IO, 'revision', 1)) THEN
            RETURN IO
        END
!
! Make sure the above worked...we should be able to open homedir
!
        CALL EB_OPEN('',homedir,F.homedir,0,ok)
        IF NOT(ok) THEN
            IO = homedir:' checked out from ':repo:' but cannot be opened'
            RETURN IO
        END
    END
!
! Create a history file for EB
!
    EXECUTE 'CREATE-FILE DATA ':homedir:',SRC.HISTORY TYPE=UD' CAPTURING IO
!
! Create OBJECT file for BASIC
!
    CALL EB_OPEN('',FileName:',OBJECT',F.homedir,0,ok)
    IF NOT(ok) THEN
        EXECUTE 'CREATE-FILE DATA ':homedir:',OBJECT TYPE=UD' CAPTURING IO
    END
!
! Now check out the actual item
!
    target = homedir:DIR_DELIM_CH:tItemName
    IF LEN(Status_IO) THEN
        command = 'update':revision
    END ELSE
        EXECUTE shell:'touch ':target:shellend CAPTURING IO
        command = 'add'
    END
    IO = GIT_EXEC(command:' ':target, TRUE)
!
    IF LEN(Status_IO) THEN
        IF INDEX(IO, 'revision', 1) THEN IO = ''
    END ELSE
        IF IO[1,1] = 'A' THEN IO = ''
    END
    IF LEN(IO) THEN RETURN IO
!
    IF NOT(revision) THEN
!
! Update SRC.LOCKS to keep track of who's working on what
!
        READVU users FROM F.Locks, K.Locks, 1 ELSE users = ''
        LOCATE user IN users SETTING pos ELSE users<1, -1> = user
        WRITEV users ON F.Locks, K.Locks, 1
!
! Create/update an xref of the original and checked out
! version of the item (i.e. original path and homepath)
!
        READU locks FROM F.Locks, user ELSE locks = ''
        LOCATE K.Locks IN locks<1> SETTING pos ELSE
            locks<1, pos> = K.Locks
            locks<2, pos> = target
        END
        WRITE locks ON F.Locks, user
    END
!
! Update FilePath to reflect new location
!
    FilePath = homedir
!
    RETURN ''
!
Ask_Checkout:
!
    IO = TRIM(GIT_SRC_STATUS(FilePath, tItemName))
    IF LEN(IO) = 0 THEN
!
! There's no git status on the item
! First see if the item can be read from the
! original file.
!
        Exists = FALSE
        CALL EB_OPEN('',FilePath,F.homedir,0,ok)
        IF ok THEN
            READV rec FROM F.homedir,tItemName,1 THEN Exists = TRUE
        END
!
! Now see if it's a new item
!
        homepath = GIT_GETHOMEPATH(FilePath)
        IO = TRIM(GIT_SRC_STATUS(homepath, tItemName))
        IF LEN(IO) THEN
            CRT @(-3)
            CRT tItemName:' is under source control in ':homepath
            CRT
            IF Exists THEN
                ANS = GETYN('Do you want to edit that version', 'Y', 2)
                Exists = (ANS = 'N')
            END ELSE
                CRT 'Switching filename...':
                RQM; RQM
                CRT
            END
            IF NOT(Exists) THEN ;! just return the homepath?
                FilePath = homepath
                IO = ''
            END
        END
    END
    result = ''
    IF LEN(IO) THEN ;!FIELD(IO, ' ', 1) MATCHES "1N0N" THEN          ;! under source control
        CRT @(-3)
        CRT FilePath:' ':tItemName:' is under source control.'
        CRT
        result = GIT_CHECKOUT(FALSE, FilePath, tItemName)
    END
!
    RETURN result
