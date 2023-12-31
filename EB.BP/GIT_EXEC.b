    FUNCTION GIT_EXEC(Args, Capturing)
! * COPIED from SVN program
    INCLUDE JBC.h
    EQU MAX TO 999999
    shell = CHAR(255):'k'
    shellend = ' 2>&1'
    noprompt = ' '
    command = 'git'
    sub_command = FIELD(Args, ' ', 1)
    new_Args = Args[COL2(), MAX]
    BEGIN CASE
        CASE sub_command = 'info'
            sub_command = 'rev-parse --show-toplevel'
        CASE sub_command = 'checkout'
            sub_command = 'add'
        CASE sub_command = 'revert'
            sub_command = 'checkout'
        CASE sub_command = 'delete'
            sub_command = 'rm'
            IF NOT(INDEX(Args, '--force', 1)) THEN
                sub_command := ' --cached '
            END
    END CASE

    dc = DCOUNT(new_Args, ' ')
    IF dc > 1 THEN
        pre_Args = FIELD(new_Args, ' ', 1, dc - 1):' '
        new_Args = new_Args[COL2()+1, MAX]
    END ELSE pre_Args = ''
    IF sub_command NE 'commit' THEN
        dir_dc = DCOUNT(new_Args, DIR_DELIM_CH)
        IF dir_dc > 1 THEN
            fileName = FIELD(new_Args, DIR_DELIM_CH, dir_dc)
!            command = 'cd ':CHANGE(new_Args[1, COL1()-1], '/', DIR_DELIM_CH):' && ':command
!            new_Args = fileName
        END
    END
    new_Args = sub_command:' ':pre_Args:new_Args
    cmd = command:noprompt:new_Args
    icmd = command:' ':new_Args

!DEBUG
    IF Capturing THEN
        EXECUTE shell:cmd:shellend CAPTURING IO
        IF FIELD(Args, ' ', 1) NE 'info' AND (INDEX(IO, 'rror', 1) OR INDEX(IO, 'failed', 1) OR INDEX(IO, 'fatal', 1)) THEN
            INCLUDE EB.INCLUDES SRCDBG
            git_io = 'git_':SYSTEM(18)
            home = GETENV('HOME')
            EXECUTE shell:icmd:' > ':home:DIR_DELIM_CH:git_io:shellend
            OPEN home THEN
                READ IO FROM git_io THEN
                    DELETE git_io
                END
            END
        END
        CONVERT CHAR(13):CHAR(10) TO @AM IN IO
        IO = CHANGE(IO, @AM:@AM, @AM)
        IO = TRIM(IO, @AM, 'T')
    END ELSE
        EXECUTE shell:icmd:shellend
        IO = ''
    END
    RETURN IO
