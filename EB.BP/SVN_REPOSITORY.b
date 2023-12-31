    FUNCTION SVN_REPOSITORY(FilePath)
!
! Return the subversion repository
!
    DEFFUN GETFULLPATH()
    DEFFUN SVN_EXEC()
    EQU TRUE TO 1, FALSE TO 0
!
    INCLUDE EB.INCLUDES SRCDBG
    IO = SVN_EXEC('info ':GETFULLPATH(FilePath), TRUE)
!
! Example of svn info
!
! URL: http://192.168.2.111:8080/svn/repository/Utils/trunk/EB
! Repository Root: http://192.168.2.111:8080/svn/repository
! Repository UUID: b5bbdc41-e4c0-d942-b859-9dd2c8910d5b
! Revision: 571
! Node Kind: directory
! Schedule: normal
! Last Changed Author: util
! Last Changed Rev: 571
! Last Changed Date: 2010-01-13 17:48:24 -0800 (Wed, 13 Jan 2010)
!
! sh UTIL C:\SVN\Utils\EB
!
! Using the URL and Repository Root we can derive the current
! path
!
    subversion = ''
    loc = 0
    LOOP
    REMOVE line FROM IO AT loc SETTING delim
    IF FIELD(line, ':', 1) = 'Repository Root' THEN
    subversion = line[COL2()+2, 99]
    delim = 0
    END
WHILE delim DO REPEAT
    RETURN subversion
