    FUNCTION EBJSHOW(command)
    INCLUDE JBC.h
    COMMON /EBJSHOW/ commands, outputs
!    DEBUG
    IF command[1,2] = '-c' THEN
        prog = FIELD(command,' ',2)
        IF (prog 'R#2') = '.b' THEN prog = prog[1, LEN(prog)-2]
        command = '-c ':prog
    END
    LOCATE command IN commands SETTING pos THEN
        io = RAISE(outputs<pos>)
    END ELSE
        cmd = 'jshow ':command
        EXECUTE cmd CAPTURING io
        IF io<1> EQ cmd THEN DEL io<1>
        io = TRIM(io, @AM, 'T')
        INS command BEFORE commands<pos>
        INS LOWER(io) BEFORE outputs<pos>
    END

    RETURN (io)
