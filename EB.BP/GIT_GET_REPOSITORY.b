    FUNCTION GIT_GET_REPOSITORY(FilePath)
!
! Function to return the repository for a given path
! (basically the parent directory of a file
! 01 DEC 2009 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    DEFFUN GETFLNM()
    DEFFUN GETFULLPATH()
    DEFFUN GIT_GETPWD()
    DEFFUN SRC_CASE()
    EQU FALSE TO 0
!
    INCLUDE EB.INCLUDES SRCDBG
    FileName = GETFLNM(FilePath)
    IO = GETFULLPATH(FilePath)
!
! Check Repo with git (path)
! and "massage" accordingly
!
    Repo= GIT_GETPWD(IO, DIR_DELIM_CH, FALSE)
    tRepo = CHANGE(Repo, DIR_DELIM_CH, @AM)
    tFilePath = CHANGE(FilePath, DIR_DELIM_CH, @AM)
!
! Reverse travers both tFilePath and tRepo
! to match up (and remove) the trailing dirs
!
    RepoCNT = DCOUNT(tRepo, @AM)
    FilePathCNT = DCOUNT(tFilePath, @AM)
    LOOP WHILE SRC_CASE(tRepo<RepoCNT>) = SRC_CASE(tFilePath<FilePathCNT>) AND RepoCNT > 1 DO
        RepoCNT--
        FilePathCNT--
    REPEAT
    Repo = CHANGE(FIELD(tRepo, @AM, 1, RepoCNT), @AM, '/')
    RETURN Repo
