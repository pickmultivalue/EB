    FUNCTION GIT_CLEAN(HomeDir, F.Source, ItemName)
!
! * COPIED from SVN program
! Program designed to clean up a user's home/dir
! directory after committing or removing
! item from a previous git check out
! F.Source is assumed to be opened to the /home/<user>/dir
!
! Returns the original source path
!
! 02 DEC 2009 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    DEFFUN GIT_EXEC()
    DEFFUN SRC_CASE()
    DEFFUN SRC_OPENLOCKS()
    DEFFUN GETFLNM()
    DEFFUN GETFULLPATH()
    EQU TRUE TO 1, FALSE TO 0
!
    HandleLocks = FALSE
    INCLUDE EB.INCLUDES SRCDBG
!
! First get user's home dir
!
    shell = @IM:'k'
    shellend = ' 2>&1'
    INCLUDE EB.INCLUDES GET.HOME
!
    IF LEN(ItemName) THEN
        IF SRC_OPENLOCKS(F.Locks) THEN
            HandleLocks = TRUE
        END
    END
    FullPath = HomeDir:DIR_DELIM_CH:ItemName
    FileName = GETFLNM(HomeDir)
    K.Locks = GETFULLPATH(FileName):DIR_DELIM_CH:ItemName
    IF HandleLocks THEN
        lock_key = SRC_CASE(K.Locks)
!
! Remove the GIT.LOCKS record
!
        READU locks FROM F.Locks, user THEN
!
! Just in case platform isn't case sensitive
!
            litem = SRC_CASE(FullPath)
            litems = SRC_CASE(locks<2>)
!
            LOCATE litem IN litems<1> SETTING pos THEN
                K.Locks = locks<1, pos>   ;! this is more reliable
                DEL locks<1, pos>
                DEL locks<2, pos>
            END
            IF LEN(locks<1>) THEN
                WRITE locks ON F.Locks, user
            END ELSE
                DELETE F.Locks, user
            END
        END ELSE RELEASE F.Locks, lock_key
!
! Remove the user from the lock item
! This would normally be just one entry in which
! case the item is deleted
!
        READVU users FROM F.Locks, lock_key, 1 THEN
            LOCATE user IN users<1> SETTING POS THEN
                DEL users<1,POS>
            END
            IF LEN(users) THEN
                WRITEV users ON F.Locks, lock_key, 1
            END ELSE
                DELETE F.Locks, lock_key
            END
        END ELSE RELEASE F.Locks, lock_key
    END
!
! Do an git status on the home file path.
! If it returns nothing then that means that
! there are no items in modified state and
! we can remove the directory
!
    IO = GIT_EXEC('status ':HomeDir, 1)
!
! Remove any ? status items
    dc = DCOUNT(IO, @AM)
    FOR i =dc TO 1 STEP -1
        io = CHANGE(TRIM(IO<i>), ' ', @AM)
        path = io<DCOUNT(io, @AM)>
        IF SRC_CASE(path) = SRC_CASE(FullPath) AND io<1> = '?' THEN
            DEL IO<i>
            DELETE F.Source, ItemName
        END
    NEXT i
    IF LEN(IO) = 0 THEN
        CLOSE F.Source
        EXECUTE 'CREATE-FILE DICT ':HomeDir:' 1' CAPTURING IO
        OPEN 'DICT',HomeDir TO F.Source THEN
            CLEARFILE F.Source
            CLOSE F.Source
        END
        CRT 'Removing ':HomeDir
        EXECUTE shell:'rm -rf ':HomeDir:DIR_DELIM_CH:'*':shellend CAPTURING io
        IF LEN(io) THEN
            CRT 'Attempted removal of ':HomeDir:' contents caused the following error:'
            CRT; CRT io
        END
        EXECUTE 'DELETE-FILE ':HomeDir CAPTURING IO
        IF LEN(IO) THEN
            EXECUTE shell:'rm -rf ':HomeDir:shellend CAPTURING io
            IF LEN(io) THEN
                CRT
                CRT 'Error cleaning up ':HomeDir
                CRT
                CRT CHANGE(IO:@AM:io, @AM, CHAR(13):CHAR(10))
                CRT
                CRT 'Press any key to continue: '
                INPUT X,0
            END ELSE
                EXECUTE 'DELETE-FILE DICT ':HomeDir CAPTURING IO
                IF LEN(IO) THEN
                    CRT
                    CRT 'Error removing DICT ':HomeDir
                    CRT
                    CRT CHANGE(IO, @AM, CHAR(13):CHAR(10))
                    CRT
                    CRT 'Press any key to continue: '
                    INPUT X,0
                END
            END
        END
    END
!
! At this point K.Locks may have changed from the
! user's homepath to the original path
! SPLITFILEPATH will update the FileName to
! return back to the calling program
!
    CALL SPLITFILEPATH(K.Locks, FileName, ItemName)
!
    RETURN FileName
