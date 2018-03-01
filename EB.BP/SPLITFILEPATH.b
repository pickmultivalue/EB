    SUBROUTINE SPLITFILEPATH(FullPath, FilePath, ItemName)
!
    INCLUDE JBC.h
    FullPath = CHANGE(FullPath, '/', DIR_DELIM_CH)
    dc = DCOUNT(FullPath, DIR_DELIM_CH)
    IF dc = 1 THEN
        ItemName = FullPath
        FilePath = '.'
    END ELSE
        ItemName = FIELD(FullPath, DIR_DELIM_CH, dc)
        FilePath = FullPath[1, COL1()-1]
    END
    RETURN
