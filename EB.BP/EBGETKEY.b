    SUBROUTINE EBGETKEY(match, chars)
    ECHO OFF
    LOOP
        chars = ''
        timeout = 1000
        LOOP
            INPUT c,0: FOR timeout ELSE BREAK
            chars := c
            timeout = 1
        REPEAT
        IF LEN(match) AND NOT(UPCASE(chars) MATCH match) THEN
            chars = ''
            CRT CHAR(7):
        END
    UNTIL LEN(chars) DO REPEAT
    IF chars = CHAR(13) THEN chars = ''
    ECHO ON
    CRT
    RETURN
