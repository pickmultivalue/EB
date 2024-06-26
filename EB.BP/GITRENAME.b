! PROGRAM GITRENAME
!
! git wrapper to rename a program
!
! 26 JAN 2010 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    DEFFUN GIT_SRC_STATUS()
    DEFFUN GIT_EXEC()
    DEFFUN GETFULLPATH()
    FNAME = SENTENCE(1)
    ITEMNAME = SENTENCE(2)
    NEWNAME = SENTENCE(3)
    IF LEN(NEWNAME) = 0 THEN
        CRT 'Syntax: ':SYSTEM(40):' filename itemname newname'
        STOP
    END
    OPEN FNAME TO F.Source THEN
        READ item FROM F.Source, ITEMNAME THEN
            READ temp FROM F.Source, NEWNAME THEN
                CRT 'Error: ':NEWNAME:' exists'
                STOP
            END
        END ELSE
            CRT 'Error: missing ':ITEMNAME
            STOP
        END
    END
    FullPath = GETFULLPATH(FNAME)
    FilePath = FullPath:DIR_DELIM_CH:ITEMNAME
    NewPath = FullPath:DIR_DELIM_CH:NEWNAME
!
    IO = GIT_SRC_STATUS(FullPath, ITEMNAME)
    IF INDEX(IO, 'Warning', 1) THEN
        CRT 'Error (see below)'
        CRT
        CRT IO
        STOP
    END
!
! Check the first part of IO to see if it's an Add
! in which case we rename the file, revert the add
! and make a new add
!
    IF FIELD(IO, ' ',1) = 'A' THEN
        WRITE item ON F.Source, NEWNAME
        CRT GIT_EXEC('revert ':FilePath, 1)
        CRT GIT_EXEC('add ':NewPath, 1)
        DELETE F.Source, ITEMNAME
    END ELSE
        GIT_EXEC('rename ':FilePath:' ':NewPath, 0)
    END
