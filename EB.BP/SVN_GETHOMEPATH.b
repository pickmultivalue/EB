    FUNCTION SVN_GETHOMEPATH(FileName)
!
! Return the user's homepath version of a given file
!
! 03 DEC 2009 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    DEFFUN GETFULLPATH()
    DEFFUN SVN_GETPWD()
    shell = CHAR(255):'k'
    shellend = ' 2>&1'
    EQU TRUE TO 1
!
! First get user's home dir
!
    INCLUDE EB.INCLUDES GET.HOME
    INCLUDE EB.INCLUDES SRC_DEBUG
!
! Now get the fully qualified path of the file
!
    FilePath = GETFULLPATH(FileName)
!
! Now get the portion of the FilePath
! that is specific to the repository
!
    s = DIR_DELIM_CH
    curr_dir = SVN_GETPWD(FilePath, s, TRUE)
    IF LEN(curr_dir) = 0 THEN ;! not a versioned file
        RETURN FilePath
        END
        curr_dir = TRIM(curr_dir, s, 'T')
!
        RETURN homepath:s:curr_dir