    FUNCTION SRC_CHECKIN(rootpath, FullPaths, Message)
!
! Function to check in the user's copy of a program.
!
! Message is AM delimited description of changes
!
! 01 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN GETSRCTYPE()
    DEFFUN GIT_CHECKIN()
    DEFFUN SVN_CHECKIN()
    scType = GETSRCTYPE()
    BEGIN CASE
        CASE scType = 'GIT'
            RETURN GIT_CHECKIN(rootpath, FullPaths, Message)
        CASE scType = 'SVN'
            RETURN SVN_CHECKIN(rootpath, FullPaths, Message)
    END CASE
