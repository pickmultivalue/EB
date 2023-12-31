    FUNCTION SVN_READ(FilePath, tItemName, Record)
!
! Function that works like a READ but designed to
! work from the source repository.
! Primary purpose is to read a specific revision for
! subsequent use in a compare routine.
!
! Function returns false if unsuccessful
! (i.e. the requested item/revision is not there)
! in which case any Record is returned in the Record arg
!
! 02 DEC 2009 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    DEFFUN SVN_SRC_STATUS()
    DEFFUN SVN_CHECKOUT()
    DEFFUN SVN_REVERT()
    DEFFUN SVN_GETHOMEPATH()
    EQU TRUE TO 1, FALSE TO 0
    shell = CHAR(255):'k'
    shellend = ' 2>&1'
    INCLUDE EB.INCLUDES SRCDBG
!
! First get user's home dir
!
    INCLUDE EB.INCLUDES GET.HOME
!
    homedir = SVN_GETHOMEPATH(FilePath)
    OPEN homedir TO F.Target ELSE
        Record = 'Error: cannot open ':homedir
        GOTO Read_Error
    END
!
! Parse out the revision
!
    ItemName = tItemName
    revision = FIELD(ItemName, ',', 2)
    IF revision THEN
        ItemName = ItemName[1, COL1()-1]
    END
!
! Backup current record (if any)
!
    READ Backup FROM F.Target, ItemName THEN
        Restore = TRUE
        DELETE F.Target, ItemName
    END ELSE
        Restore = FALSE
    END
!
! Check to see what revision we have at the moment
!
    Status_IO = TRIM(SVN_SRC_STATUS(homedir, ItemName)[2, -1])
    IF LEN(Status_IO) THEN
        current_rev = FIELD(Status_IO, ' ',2)
    END ELSE
        current_rev = ''
    END
!
    Record = SVN_CHECKOUT(FALSE, FilePath, ItemName)
    IF LEN(Record) THEN
        ItemName = tItemName          ;! errmsg purposes
        Record = 'Error: from svn checkout ':@AM:@AM:Record
        GOTO Read_Error
    END
!
! Now get the requested item
!
    READ Record FROM F.Target, ItemName THEN
        ok = TRUE
    END ELSE
        ok = FALSE
    END
!
! Restore the item on disk
!
    IF Restore THEN
!
! First re-issue a correct svn update command
! otherwise subsequent updates will cause an
! error with subversion
!
        IF revision # current_rev THEN
            IF NOT(LEN(current_rev)) THEN DEBUG
            ItemName := ',':current_rev
            Record = SVN_CHECKOUT(FALSE, FilePath, ItemName)
            ok = INDEX(Record, 'revision', 1)
        END
!
        WRITE Backup ON F.Target, ItemName
    END ELSE
        IO = SVN_REVERT(homedir:DIR_DELIM_CH:ItemName)
    END
!
    IF NOT(ok) THEN
        Record = 'Error: failed to read ':ItemName:' after'
        Record<3> = 'SVN_CHECKOUT of ':ItemName
        GOTO Read_Error
    END
!
    RETURN TRUE
!
Read_Error:
!
    RETURN FALSE
