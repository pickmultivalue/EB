    FUNCTION SRC_GETPWD(FilePath, Delim, Remove_trunk)
! * COPIED from SVN program
!
! Return the portion of the given filepath
! which is specific to the repository
! (i.e. the portion of the filepath
! which follows the git info root)
!
    DEFFUN GETSRCTYPE()
    DEFFUN GIT_GETPWD()
    DEFFUN SVN_GETPWD()
    scType = GETSRCTYPE()
    BEGIN CASE
        CASE scType = 'GIT'
            RETURN GIT_GETPWD(FilePath, Delim, Remove_trunk)
        CASE scType = 'SVN'
            RETURN SVN_GETPWD(FilePath, Delim, Remove_trunk)
    END CASE
