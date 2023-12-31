    FUNCTION SVN_CHECKOUT(Ask, FilePath, ItemName)
!
! Function to make a copy of a program to a user's home directory using
! svn export.
! The copy then becomes the user's working copy of the program
! When the user check's it in it moves it to the original location
! and issues an svn commit to update the svn repository
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
    DEFFUN SVN_EXEC()
    DEFFUN SVN_REPOSITORY()
    DEFFUN SVN_GET_REPOSITORY()
    DEFFUN SVN_GETHOMEPATH()
    DEFFUN SVN_GETORIGPATH()
    DEFFUN SRC_OPENLOCKS()
    DEFFUN SVN_SRC_STATUS()
    DEFFUN SVN_CHECKOUT()
!
    IF NOT(SRC_OPENLOCKS(F.Locks)) THEN
        RETURN 'SRC.LOCKS is not a file name'
    END
    EQU TRUE TO 1, FALSE TO 0
    shell = CHAR(255):'k'
    shellend = ' 2>&1'
    PROMPT ''
!
! First get user's home dir
!
    INCLUDE EB.INCLUDES GET.HOME
    INCLUDE EB.INCLUDES SRCDBG
!
    IF Ask THEN
        GOSUB Ask_Checkout
        RETURN result
    END
!
! Check if a specific revision is required
!
    revision = FIELD(ItemName, ',', 2)
    IF revision THEN
        revision = ' -r ':revision
        tItemName = ItemName[1, COL1() - 1]
    END ELSE tItemName = ItemName
!
! Now make the target directory
!
    FileName = GETFLNM(FilePath)
!
! First check that directory is under source control
!
    OrigPath = SVN_GETORIGPATH(FileName)
    Status_IO = SVN_EXEC('status -v --depth=empty ':OrigPath, TRUE)
    IF LEN(Status_IO) = 0 OR Status_IO[1,1] = 'I' THEN          ;! need to add this directory
        homedir = SVN_GETHOMEPATH('.'):DIR_DELIM_CH:FileName
        OPEN homedir ELSE
            EXECUTE shell:'mkdir ':homedir:shellend CAPTURING IO
        END
        OrigPath = CONVERT(homedir, DIR_DELIM_CH, '/')
        Status_IO = SVN_EXEC('add --depth=empty ':OrigPath, TRUE)
        IF Status_IO[1,1] NE 'A' THEN        ;! bailing out at this point
            Result = OrigPath:' svn status error:'
            Result<3> = Status_IO
            RETURN Result
        END
        FilePath = homedir
    END
    homedir = SVN_GETHOMEPATH(FilePath)
!
! Now check the svn status of mName/tItemNameItemName
!
    OrigPath := '/':tItemName
    Status_IO = SVN_EXEC('status -v ':OrigPath, TRUE)
    IF TRIM(Status_IO[1,1]) NE '' THEN
        Result = tItemName:' error'
        Result<3> = 'svn status output:'
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
    OPEN homedir ELSE
!
! Get the Subversion/Repository info
!
        Subversion = SVN_REPOSITORY(FileName)
        Repository = SVN_GET_REPOSITORY(FileName)
!
! First checkout the root directory
!
        repo = Subversion:'/':Repository
        CALL SPLITFILEPATH(homedir, homepath, homeitem)
        IO = SVN_EXEC('checkout --depth=empty ':repo:' ':homepath, TRUE)
        IF NOT(INDEX(IO, 'revision', 1)) THEN
            RETURN IO
        END
!
! Now checkout the filename
!
        repo := '/':FileName
        IO = SVN_EXEC('checkout --depth=empty ':repo:' ':homedir, TRUE)
        IF NOT(INDEX(IO, 'revision', 1)) THEN
            RETURN IO
        END
!
! Make sure the above worked...we should be able to open homedir
!
        OPEN homedir ELSE
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
    OPEN FileName:',OBJECT' THEN
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
    IO = SVN_EXEC(command:' ':target, TRUE)
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
    IO = TRIM(SVN_SRC_STATUS(FilePath, tItemName))
    IF LEN(IO) = 0 THEN
!
! There's no svn status on the item
! First see if the item can be read from the
! original file.
!
        Exists = FALSE
        OPEN FilePath THEN
            READV rec FROM tItemName,1 THEN Exists = TRUE
        END
!
! Now see if it's a new item
!
        homepath = SVN_GETHOMEPATH(FilePath)
        IO = TRIM(SVN_SRC_STATUS(homepath, tItemName))
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
    IF FIELD(IO, ' ', 1) MATCHES "1N0N" THEN          ;! under source control
        CRT @(-3)
        CRT FilePath:' ':tItemName:' is under source control.'
        CRT
        ANS = GETYN('Do you want to check it out and edit the checked out copy', 'Y', 2)
        IF ANS = 'Y' THEN
            result = SVN_CHECKOUT(FALSE, FilePath, tItemName)
        END
    END
!
    RETURN result
