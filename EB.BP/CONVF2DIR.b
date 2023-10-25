! PROGRAM CONVF2DIR
    INCLUDE JBC.h
!
! Convert a hashed file to a directory
!
    fileName = SENTENCE(1)
    IF LEN(fileName) EQ 0 THEN
        CRT 'Syntax: CONVF2DIR <filename>'
        STOP
    END
    OPEN fileName TO f.orig ELSE STOP 201, fileName
    result = ''
    rc = IOCTL(f.orig, JIOCTL_COMMAND_FILESTATUS, result)
    IF result<1> EQ 'UD' THEN
        CRT fileName:' already a directory'
        STOP
    END
    rc = IOCTL(f.orig, JBC_COMMAND_GETFILENAME, fileName)
    dirName = 'dir_':OCONV(fileName, 'MCA')
    dirName = CHANGE(dirName, DIR_DELIM_CH:'.':DIR_DELIM_CH, DIR_DELIM_CH)
    ksh = @IM:'k'
    EXECUTE ksh:'mkdir ':dirName CAPTURING io
    IF LEN(io) THEN
        CRT io
        STOP
    END
    OPEN dirName TO f.new ELSE STOP 201, dirName
    SELECT f.orig
    LOOP WHILE READNEXT id DO
        READ rec FROM f.orig, id THEN
            WRITE rec ON f.new, id
        END
    REPEAT
    CLOSE f.orig
    CLOSE f.new
    EXECUTE ksh:RM_CMD:' ':fileName CAPTURING io
    IF LEN(io) THEN
        CRT 'Fatal error removing original file'
        CRT io
        STOP
    END
    EXECUTE ksh:MOVE_CMD:' ':dirName:' ':fileName
