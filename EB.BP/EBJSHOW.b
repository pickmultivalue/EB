    FUNCTION EBJSHOW(command)
    cmd = 'jshow ':command:' 2>&1'
    EXECUTE cmd CAPTURING io
    IF io<1> EQ cmd THEN DEL io<1>
    io = TRIM(io, @AM, 'T')
    RETURN (io)
