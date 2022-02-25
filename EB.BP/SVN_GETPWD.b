    FUNCTION SVN_GETPWD(FilePath, Delim, Remove_trunk)
!
! Return the portion of the given filepath
! which is specific to the repository
! (i.e. the portion of the filepath
! which follows the svn info root)
!
    DEFFUN SVN_EXEC()
    EQU TRUE TO 1, FALSE TO 0
!
! Return the subversion repository
!
    INCLUDE EB.INCLUDES SRCDBG
    IO = SVN_EXEC('info ':FilePath, TRUE)
!
    IF INDEX(IO, 'Not a version', 1) THEN
        CALL SPLITFILEPATH(FilePath, Parent, FileName)
        IO = SVN_EXEC('info ':Parent, TRUE)
        FileName = Delim:FileName
    END ELSE FileName = ''
!
    url = ''
    repo = ''
    loc = 0
    IO = CONVERT(IO, CHAR(13):CHAR(10), @AM:@AM)
    LOOP
        REMOVE line FROM IO AT loc SETTING delim
        lbl = FIELD(line, ':', 1)
        therest = line[COL2()+2, 999]
        BEGIN CASE
            CASE lbl = 'URL' ; url = therest
            CASE lbl = 'Repository Root' ; repo = therest
        END CASE
    WHILE delim AND (LEN(url) = 0 OR LEN(repo) = 0) DO REPEAT
!
    therest = url[LEN(repo)+2, 999]
!
    IF Remove_trunk THEN
        therest = CHANGE(therest, '/trunk', '')
    END
!
    IF LEN(Delim) AND Delim NE '/' THEN
        therest = CONVERT(therest, '/', Delim)
    END
    RETURN therest:FileName
