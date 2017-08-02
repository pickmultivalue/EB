    SUBROUTINE SPLITFILEPATH(FullPath, FilePath, ItemName)
* @(#) SPLITFILEPATH.b Ported to jBASE 07:23:52  18 FEB 2010
!
    INCLUDE JBC.h
    dc = DCOUNT(FullPath, DIR_DELIM_CH)
    IF dc = 1 THEN
    ItemName = FullPath
    FilePath = '.'
END ELSE
ItemName = FIELD(FullPath, DIR_DELIM_CH, dc)
FilePath = FullPath[1, COL1()-1]
END
RETURN
