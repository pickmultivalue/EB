    FUNCTION EB_GIT(cmd, filepath)
!
! Wrapper to git
! cmd      - is the command (including any args)
! filepath - qualified filepath
!
    shell = @IM:'k'
    shellend = ' 2>&1'
!
! First get the full qualified filepath/dir
! then split into git root and the rest
!
    RETURN result
