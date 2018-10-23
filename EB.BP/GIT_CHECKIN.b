    FUNCTION GIT_CHECKIN(rootpath, FullPaths, Message)
!
! Function to check in the user's copy of a program.
!
! Message is AM delimited description of changes
!
! 01 DEC 2009 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    DEFFUN GETFLNM()
    DEFFUN GETFULLPATH()
    DEFFUN GIT_EXEC()
    DEFFUN SRC_CASE()
    DEFFUN GIT_CLEAN()
    DEFFUN GIT_GETROOT()
    DEFFUN GIT_GETHOMEPATH()
    DEFFUN GIT_GETORIGPATH()
    DEFFUN SRC_OPENLOCKS()
    DEFFUN GIT_REMOVEHIST()
    EQU TRUE TO 1, FALSE TO 0
!
    end_of_screen = @(0,SYSTEM(3)):@(-3)
    shell = @IM:'k'
    shellend = ' 2>&1'
!
! First get user's home dir
!
    INCLUDE EB.INCLUDES GET.HOME
!
! Initialise the commit command
!
    rc = SRC_OPENLOCKS(F.Locks)
    commit_path = ''
    INCLUDE EB.INCLUDES SRC_DEBUG
!
    nbr_items = DCOUNT(FullPaths, @AM)
    FOR i = 1 TO nbr_items
        FullPath = FullPaths<i>
        IF FullPath[1, LEN(rootpath)] # rootpath THEN
            FullPath = rootpath:DIR_DELIM_CH:FullPath
        END
        commit_path := ' ':CHANGE(FullPath, '*', '\*')
!        IF i = nbr_items THEN
!            CALL SPLITFILEPATH(FullPath, FilePath, ItemName)
!            rootdir = GIT_GETROOT(FilePath)
!            OPEN rootdir TO F.Source ELSE
!                RETURN rootdir:' could not be opened'
!            END
!        END
    NEXT i
!
! Commit item to repository
!
    commit_msg = 'git_commit_msg'
    OPEN '.' TO F.local ELSE NULL
    commit_message = ''
    loc = 0
    LOOP
        REMOVE line FROM Message AT loc SETTING delim
        IF INDEX(line, '--This line, and those below, will be ignored--', 1) THEN
            BREAK
        END
        commit_message<-1> = line
    WHILE delim DO REPEAT
    WRITE commit_message ON F.local, commit_msg
    IO = GIT_EXEC('commit -F ':commit_msg:' ':commit_path, TRUE)
!
    IF INDEX(IO, 'working copy', 1) THEN
        CRT end_of_screen:'Warning: commit of ':commit_path:' failed'
        CRT
        CRT IO
        CRT
        CRT 'Backing up items in ':commit_path:' xxxx.bck'
        CRT
!
        LastFileName = ''
        FOR i = 1 TO nbr_items
            FullPath = FullPaths<i>
            commit_path := ' ':FullPath
            CALL SPLITFILEPATH(FullPath, FilePath, ItemName)
            FileName = GETFLNM(FilePath)
!
            IF LastFileName # FileName THEN
                LastFileName = FileName
                rootdir = GIT_GETROOT(FilePath)
                OPEN rootdir TO F.Source ELSE
                    RETURN rootdir:' could not be opened'
                END
            END
            READ record FROM F.Source, ItemName ELSE
                RETURN ItemName:' could not be read from ':rootdir
            END
            WRITE record ON F.Source, ItemName:'.bck' ON ERROR
                RETURN ItemName:'.bck could not be written to ':rootdir
            END
        NEXT i
!
        CRT
        CRT 'You may wish to compare ':commit_path:'.bck with ':commit_path
!
        Result = 'Please review ':commit_path
        CRT
        CRT Result
        RETURN Result
    END
!
    DELETE F.local, commit_msg
    IF INDEX(IO, 'fatal', 1) THEN
        CONVERT @LF:@CR TO @AM IN IO
        Result = 'Error during commit'
        Result<3> = 'commit output:'
        Result<5> = IO
        RETURN Result
    END
!
! Was this a new check in or a modification
!
    READ locks FROM F.Locks, user ELSE locks = ''
    litems = SRC_CASE(locks<2>)
    sync_path = ''
    Decatalog_List = ''
    SaveFullPaths = FullPaths
    FOR i = 1 TO nbr_items
        FullPath = FullPaths<i>
        CALL SPLITFILEPATH(FullPath, FilePath, ItemName)
        IF FilePath = GIT_GETHOMEPATH(FilePath) THEN
            LOCATE SRC_CASE(FullPath) IN litems<1> SETTING pos THEN
                OFullPath = locks<1, pos>
                CALL SPLITFILEPATH(OFullPath, OrigPath, ItemName)
            END ELSE
                OrigPath = GIT_GETORIGPATH(FilePath)
            END
            LOCATE FilePath IN Decatalog_List<1> SETTING fpos ELSE
                INS FilePath BEFORE Decatalog_List<1, fpos>
                INS OrigPath BEFORE Decatalog_List<2, fpos>
                INS '' BEFORE Decatalog_List<3, fpos>
            END
            LOCATE ItemName IN Decatalog_List<3, fpos> SETTING ipos ELSE
                INS ItemName BEFORE Decatalog_List<3, fpos, ipos>
            END
            FullPath = OrigPath:DIR_DELIM_CH:ItemName
            sync_path := ' ':FullPath
            FullPaths<i> = FullPath         ;! so calling program can pick up new location if desired
        END
    NEXT i
!
    RETURN ''
