    FUNCTION SRC_GET_REPOSITORY(FilePath)
!
! Function to return the repository for a given path
! (basically the parent directory of a file
! 01 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN GETSRCTYPE()
    DEFFUN GIT_GET_REPOSITORY()
    DEFFUN SVN_GET_REPOSITORY()
    scType = GETSRCTYPE()
    BEGIN CASE
        CASE scType = 'GIT'
            RETURN GIT_GET_REPOSITORY(FilePath)
        CASE scType = 'SVN'
            RETURN SVN_GET_REPOSITORY(FilePath)
    END CASE
