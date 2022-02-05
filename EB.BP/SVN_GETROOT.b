    FUNCTION SVN_GETROOT(FilePath)
!
! Return the root path tree above path recognised by SVN
!
    DEFFUN GETFULLPATH()
    DEFFUN SVN_GETPWD()
    DEFFUN SRC_CASE()
    INCLUDE JBC.h
    EQU TRUE TO 1
!
    INCLUDE EB.INCLUDES SRCDBG
    ChildPath = SVN_GETPWD(FilePath, DIR_DELIM_CH, TRUE)
    FullPath = GETFULLPATH(FilePath)
!
! Reverse travers both tFilePath and tRepo
! to match up (and remove) the trailing dirs
!
    FullPath = CONVERT(FullPath, DIR_DELIM_CH, @AM)
    ChildPath = CONVERT(ChildPath, DIR_DELIM_CH, @AM)
!
! Just in case (Windows) make case insensitive versions
!
    c_ChildPath = SRC_CASE(ChildPath)
    c_FullPath = SRC_CASE(FullPath)
    ChildPathCNT = DCOUNT(c_ChildPath, @AM)
    FullPathCNT = DCOUNT(c_FullPath, @AM)
    LOOP WHILE SRC_CASE(c_ChildPath<ChildPathCNT>) = SRC_CASE(c_FullPath<FullPathCNT>) AND FullPathCNT > 1 DO
        ChildPathCNT--
        FullPathCNT--
    REPEAT
!
    RETURN CONVERT(FIELD(FullPath, @AM, 1, FullPathCNT), @AM, DIR_DELIM_CH)
