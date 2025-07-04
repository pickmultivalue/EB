    FUNCTION SRC_HISTORY(FilePath, ItemName, Filters)
!
! Function to get the history/revsion of changes of a program from subversion
!
! Filters is an optional dynamic array
!
! attr 1 multi-valued strings containing any of the following:
!
! - Date       {used for exact match}
! - StartDate  {used for range}
! - EndDate
! - Time       {used for exact match}
! - StartTime  {used for range}
! - EndTime
! - User       {login name (* can be used at start/end)}
! - Word       {sub-valued list of words to search (case insensitive)}
!
! attr 2 multi-valued corresponding value
!
    DEFFUN GETSRCTYPE()
    DEFFUN GIT_HISTORY()
    DEFFUN SVN_HISTORY()
    rc = GETCWD(currdir)
    rc = CHDIR(FilePath)
    scType = GETSRCTYPE()
    result = ''
    BEGIN CASE
        CASE scType = 'GIT'
            result = GIT_HISTORY(FilePath, ItemName, Filters)
        CASE scType = 'SVN'
            result = SVN_HISTORY(FilePath, ItemName, Filters)
    END CASE
    rc = CHDIR(currdir)
    RETURN result
