    FUNCTION SVN_EXEC(Args, Capturing)
    shell = CHAR(255):'k'
    shellend = ' 2>&1'
    command = 'svn'
    cmd = shell:command:' --non-interactive ':Args:shellend
    icmd = shell:command:' ':Args
    IF Capturing THEN
        EXECUTE cmd CAPTURING IO
        IF FIELD(Args, ' ', 1) # 'info' AND (INDEX(IO, 'rror', 1) OR INDEX(IO, 'failed', 1)) THEN
            INCLUDE EB.INCLUDES SRC_DEBUG
            svn_io = 'svn_':SYSTEM(18)
            EXECUTE icmd:' > ':svn_io:shellend
            OPEN '.' THEN
                READ IO FROM svn_io THEN
                    DELETE svn_io
                END
            END
        END
        IO = CONVERT(IO, CHAR(13):CHAR(10), @AM)
        IO = CHANGE(IO, @AM:@AM, @AM)
        IO = TRIM(IO, @AM, 'T')
    END ELSE
        EXECUTE icmd:shellend
        IO = ''
    END
    RETURN IO
