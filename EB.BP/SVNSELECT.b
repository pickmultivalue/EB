    PROGRAM SVNSELECT
!
! Generate a select list of SRC.LOCKS for a given file
! for the current user
!
! Syntax: SVNSELECT {filename} {(C or (F  for commit path}
!
! if C or F option not provided then a list of item names
! only will be return.
!
! If no filename specified then all items are returned
!
    INCLUDE JBC.h
    DEFFUN SVN_GETHOMEPATH()
    DEFFUN SRC_OPENLOCKS()
!
    IF NOT(SRC_OPENLOCKS(F.Locks)) THEN STOP 201, 'SRC.LOCKS'
!
    shell = CHAR(255):'k'
    shellend = ' 2>&1'
    INCLUDE EB.INCLUDES GET.HOME
    INCLUDE EB.INCLUDES SRCDBG
    FileName = SENTENCE(1)
    IF FileName[1,1] = '(' THEN FileName = ''
    Opts = UPCASE(FIELD(@SENTENCE, '(', 2))
    GetFullPath = (INDEX(Opts, 'C', 1) OR INDEX(Opts, 'F', 1))
    AllUsers = INDEX(Opts, 'A', 1)
!
    ItemList = ''
!
    IF AllUsers THEN
    SelectList = ''
    SELECT F.Locks
    LOOP WHILE READNEXT FullPath DO
        SelectList<1, -1> = FullPath
        REPEAT
    END ELSE
    READV SelectList FROM F.Locks, user, 1 ELSE SelectList = ''
    END
!
    IF LEN(SelectList) THEN
    loc = 0
    LOOP
    REMOVE FullPath FROM SelectList AT loc SETTING delim
    IF INDEX(FullPath, DIR_DELIM_CH, 1) THEN
    CALL SPLITFILEPATH(FullPath, FilePath, ItemName)
    CALL SPLITFILEPATH(FilePath, Path, Fname)
    IF Fname = FileName OR LEN(FileName) = 0 THEN
    IF (GetFullPath) THEN
    ItemName = SVN_GETHOMEPATH(Fname):DIR_DELIM_CH:ItemName
END ELSE
IF LEN(FileName) = 0 THEN
ItemName = FullPath
END
END
END ELSE ItemName = FullPath
IF LEN(TRIM(ItemName)) THEN
LOCATE ItemName IN ItemList BY 'AL' SETTING pos ELSE
INS ItemName BEFORE ItemList<pos>
END
END
END
WHILE delim DO REPEAT
!
K.List = user:'_svn_':FileName
IF LEN(FileName) = 0 THEN FileName = 'all'
WRITELIST ItemList ON K.List
CHAIN 'GET-LIST ':K.List
END ELSE
CRT 'No items to process'
END
