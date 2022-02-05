    FUNCTION SVN_GETORIGPATH(FilePath)
!
! Return the original path of a given file
!
! 03 DEC 2009 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    DEFFUN GETFLNM()
!
    IF FilePath = '.' THEN
        IF GETENV('pwd',currdir) ELSE
            IF GETENV('PWD',currdir) THEN NULL
        END
        IO = '. ':currdir
    END ELSE
        FileName = GETFLNM(FilePath)
        IO = FilePath
        OPEN FileName TO F.temp THEN
            rc = IOCTL(F.temp, JBC_COMMAND_GETFILENAME, IO)
        END
    END
!
    RETURN FIELD(TRIM(IO<1>), ' ', 2, 99)
