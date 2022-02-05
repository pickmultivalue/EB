    FUNCTION EBJSHOW(command)
    INCLUDE JBC.h
!    DEBUG
    IF command[1,2] = '-c' THEN
        prog = FIELD(command,' ',2)
        IF (prog 'R#2') = '.b' THEN prog = prog[1, LEN(prog)-2]
        command = '-c ':prog
    END
    cmd = 'jshow ':command
    EXECUTE cmd CAPTURING io
    IF io<1> EQ cmd THEN DEL io<1>
    io = TRIM(io, @AM, 'T')
    RETURN (io)
