    FUNCTION SRC_GETHOMEPATH(FileName)
    COMMON /SRC_GETHOMEPATH/ filenames, homepaths
!
! Return the user's homepath version of a given file
!
! 03 DEC 2009 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    DEFFUN GETFULLPATH()
    DEFFUN SRC_GETPWD()
    shell = @IM:'k'
    shellend = ' 2>&1'
    EQU TRUE TO 1
    LOCATE FileName IN filenames SETTING pos THEN
        result = homepaths<pos>
    END ELSE
!
! First get user's home dir
!
        INCLUDE EB.INCLUDES GET.HOME
        INCLUDE EB.INCLUDES SRCDBG
!
! Now get the fully qualified path of the file
!
        FilePath = GETFULLPATH(FileName)
!
! Now get the portion of the FilePath
! that is specific to the repository
!
        s = DIR_DELIM_CH
        curr_dir = SRC_GETPWD(FilePath, s, TRUE)
        IF LEN(curr_dir) = 0 THEN ;! not a versioned file
            result = FilePath
        END ELSE
            result = CHANGE(curr_dir, '/', s)
!        curr_dir = TRIM(curr_dir, s, 'T')
!
!        result = homepath:s:curr_dir
        END
        INS FileName BEFORE filenames<pos>
        INS result BEFORE homepaths<pos>
    END

    RETURN result
