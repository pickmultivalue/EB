    FUNCTION GIT_GETPWD(FilePath, Delim, Remove_trunk)
! * COPIED from SVN program
!
! Return the portion of the given filepath
! which is specific to the repository
! (i.e. the portion of the filepath
! which follows the git info root)
!
    INCLUDE JBC.h
    DEFFUN GIT_EXEC()
    EQU TRUE TO 1, FALSE TO 0
!
! Return the git repository
!
    INCLUDE EB.INCLUDES SRCDBG
    IO = GIT_EXEC('info ':FilePath, TRUE)
!
    IF LEN(IO) = 0 OR INDEX(IO, 'Not a ', 1) OR INDEX(IO, 'ambiguous', 1) THEN
        CALL SPLITFILEPATH(FilePath, Parent, FileName)
        IO = GIT_EXEC('info ':Parent, TRUE)
        IF LEN(IO) = 0 OR INDEX(IO, 'Not a ', 1) OR INDEX(IO, 'ambiguous', 1) THEN
            IO = ''
        END
        FileName = Delim:FileName
    END ELSE FileName = ''
!
    IF 0 THEN
        url = ''
        repo = ''
        loc = 0
        CONVERT CHAR(13):CHAR(10) TO @AM:@AM IN IO
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
    END ELSE ;! git
        root = IO<1>
!        path = IO<2>
!        IF LEN(path) THEN
!            root = CHANGE(IO, @AM, DIR_DELIM_CH)
!        END
        RETURN root
    END
!
    IF Remove_trunk THEN
        therest = CHANGE(therest, '/trunk', '')
    END
!
    IF LEN(Delim) AND Delim # '/' THEN
        therest = CHANGE(therest, '/', Delim)
    END
    RETURN therest:FileName
