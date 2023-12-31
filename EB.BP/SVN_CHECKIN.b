    FUNCTION SVN_CHECKIN(FullPaths, Message)
!
! Function to check in the user's copy of a program.
! This copy is first copied on top of the original source,
! an "svn commit" is then performed. If successful then
! the current user copy is decataloged and deleted and the
! lock is removed from SRC.LOCKS
!
! Message is AM delimited description of changes
!
! 01 DEC 2009 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    DEFFUN GETFLNM()
    DEFFUN GETFULLPATH()
    DEFFUN SVN_EXEC()
    DEFFUN SRC_CASE()
    DEFFUN SVN_CLEAN()
    DEFFUN SVN_GETROOT()
    DEFFUN SVN_GETHOMEPATH()
    DEFFUN SVN_GETORIGPATH()
    DEFFUN SVN_OPENHIST()
    DEFFUN SRC_OPENLOCKS()
    DEFFUN SVN_REMOVEHIST()
    EQU TRUE TO 1, FALSE TO 0
!
    end_of_screen = @(0,SYSTEM(3)):@(-3)
    shell = CHAR(255):'k'
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
    INCLUDE EB.INCLUDES SRCDBG
!
    nbr_items = DCOUNT(FullPaths, @AM)
    FOR i = 1 TO nbr_items
        FullPath = FullPaths<i>
        commit_path := ' ':FullPath
        IF i = nbr_items THEN
            CALL SPLITFILEPATH(FullPath, FilePath, ItemName)
            rootdir = SVN_GETROOT(FilePath)
            OPEN rootdir TO F.Source ELSE
                RETURN rootdir:' could not be opened'
            END
        END
    NEXT i
!
! Commit item to repository
!
    commit_msg = 'svn_commit_msg'
    WRITE Message ON F.Source, commit_msg
    IO = SVN_EXEC('commit -F ':rootdir:DIR_DELIM_CH:commit_msg:' ':commit_path, TRUE)
!
! If commit fails due to "not working copy"
! then do an svn update - which should merge in
! other changes. However, to be safe we'll make a backup
! and return a non-null result to allow the user to re-check
! the record
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
                rootdir = SVN_GETROOT(FilePath)
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
        CRT 'Performing svn update to merge in changes'
        SVN_EXEC('update ':commit_path, FALSE)
        EXECUTE shell:'chmod -w ':commit_path:shellend
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
    DELETE F.Source, commit_msg
    IF NOT(INDEX(IO, 'Committed revision', 1)) THEN
        Result = 'Error during commit of ':ItemName
        Result<3> = 'svn commit output:'
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
        IF FilePath = SVN_GETHOMEPATH(FilePath) THEN
            LOCATE SRC_CASE(FullPath) IN litems<1> SETTING pos THEN
                OFullPath = locks<1, pos>
                CALL SPLITFILEPATH(OFullPath, OrigPath, ItemName)
            END ELSE
                OrigPath = SVN_GETORIGPATH(FilePath)
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
    IF LEN(sync_path) THEN
!
! Now resync the original copy
!
        IO = SVN_EXEC('update ':sync_path, TRUE)
        IF NOT(INDEX(IO, 'revision', 1)) THEN
            Result = 'Error resync of ':ItemName
            Result<3> = 'svn update output:'
            Result<5> = IO
            FullPaths = SaveFullPaths
            RETURN Result
        END
        EXECUTE shell:'chmod -w ':sync_path:shellend CAPTURING ignore
!
! Commit successful now to remove local copy
!
        nbr_files = DCOUNT(Decatalog_List<1>, @VM)
        FOR f = 1 TO nbr_files
            HomePath = Decatalog_List<1, f>
            OrigPath = Decatalog_List<2, f>
            ItemList    = Decatalog_List<3, f>
            OPEN HomePath TO F.Source ELSE
                RETURN HomePath:' could not be opened when cleaning up check in for ':ItemList
            END
            nbr_items = DCOUNT(ItemList, @SVM)
            cat_list = ''
            FileName = GETFLNM(OrigPath)
            Remove_Hist = SVN_OPENHIST(F.Hist, HomePath)
            FOR i = 1 TO nbr_items
                ItemName = ItemList<1, 1, i>
                IF Remove_Hist THEN
                    IF NOT(SVN_REMOVEHIST(F.Hist, ItemName)) THEN
                        CRT; CRT 'Error removing SVN.HISTORY for ':ItemName
                        INPUT CONT,0
                    END
                END
                tItemName = ItemName
                IF OCONV(tItemName 'R#2', 'MCL') = '.b' THEN
                    tItemName = tItemName[1, -3]:'.o*'
                END ELSE
                    tItemName = '\$':tItemName
                END
                hp = HomePath
                obj = ',OBJECT'
                OPEN hp:obj THEN
                    hp := obj
                END
                hp = GETFULLPATH(hp)
                hp = CONVERT(hp, DIR_DELIM_CH, '/')
                EXECUTE shell:'ls -s ':hp:'/':tItemName:shellend CAPTURING IO
                IO = TRIM(IO)
                IF INDEX(IO, 'No such ', 1) THEN
                    recompile = FALSE
                END ELSE
                    recompile = FIELD(IO, ' ',1)
                    IF NOT(NUM(recompile)) THEN recompile = FALSE
                END
                IF recompile THEN     ;! remove object code
                    EXECUTE shell:'rm ':hp:'/':tItemName:shellend
                    EXECUTE 'DECATALOG ':HomePath:' ':ItemName CAPTURING IO
!
! Do we care if it failed?
!
                    IF NOT(INDEX(IO, 'decataloged successfully', 1)) THEN
                        CRT end_of_screen:'Warning: attempted decatalog of ':HomePath:' ':ItemName
                        CRT
                        CRT CHANGE(IO, @AM, CHAR(13):CHAR(10))
                        CRT
                        CRT 'Press <enter> to continue: ':
                        INPUT X,1_
                        catalog = FALSE
                    END ELSE
                        catalog = TRUE
                    END
                    FnId = FileName:' ':ItemName
                    IF catalog THEN
                        IF recompile THEN
                            CRT end_of_screen:'Recompiling ':FnId:'...'
                            EXECUTE 'BASIC ':FnId CAPTURING IO
                            IF NOT(INDEX(IO, 'successfully', 1)) THEN
                                CRT
                                CRT CHANGE(IO, @AM, CHAR(13):CHAR(10))
                                CRT
                                CRT 'Press <enter> to continue: ':
                                INPUT X,1_
                                catalog = FALSE
                            END
                        END
                    END
                    IF catalog THEN cat_list := ' ':ItemName
                END
!
! Delete the user's copy and .bck
!
                flnm = SVN_CLEAN(HomePath, F.Source, ItemName)
            NEXT i
            IF LEN(cat_list) THEN
                FnId = FileName:cat_list
                CRT end_of_screen:'Cataloging ':FnId:'...'
                EXECUTE 'CATALOG ':FnId
            END
        NEXT f
    END ELSE
        EXECUTE shell:'chmod -w ':commit_path:shellend CAPTURING ignore
    END
!
    RETURN ''
