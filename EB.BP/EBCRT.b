    DIM SCREEN.PARAMS(65)
    INCLUDE EB.EQUS SCREEN.PARAMS
    cmd = SYSTEM(1000)
    OPEN 'EB.PARAMS' TO f.params ELSE STOP 201, 'EB.PARAMS'
    OPEN 'EB.EQUS' TO f.equs ELSE STOP 201, 'EB.EQUS'
    READ equs FROM f.equs, 'SCREEN.PARAMS' ELSE STOP 202, 'SCREEN.PARAMS'
    ttype = cmd<2>
    key = 'CRT@':ttype
    MATREAD SCREEN.PARAMS FROM f.params, key THEN
        status = 'updated'
    END ELSE
        MAT SCREEN.PARAMS =''
        CRT 'New term type...';RQM
        status = 'created'
    END
    loc = 0
    LOOP
        REMOVE line FROM equs AT loc SETTING delim
        setting = TRIM(FIELD(line, '!', 2))
        attr = OCONV(line, 'MCN')
        IF LEN(setting) AND attr MATCHES '1N0N' THEN
            chars = SCREEN.PARAMS(attr)
            save_chars = chars
            GOSUB get_key
            IF LEN(chars) > 0 THEN
                IF chars = ' ' THEN chars = ''
                IF chars # save_chars THEN
                    SCREEN.PARAMS(attr) = chars
                    CRT OCONV(chars, 'MX')
                    CRT OCONV(chars, 'MCP')
                END
            END
        END
    WHILE delim DO REPEAT
    len = 0
    FOR attr = 2 TO 100
        l = LEN(SCREEN.PARAMS(attr))
        IF l > len THEN len = l
    NEXT attr
    SCREEN.PARAMS(1) = len
    setting = 'Update ':key
    chars = 'N'
    GOSUB get_key
    IF OCONV(chars, 'MCU') = 'Y' THEN
        MATWRITE SCREEN.PARAMS ON f.params, key
        CRT key:' ':status
    END
    STOP
get_key:
    PROMPT ''
    l = LEN(chars)
    newchars = ''
    FOR i = 1 TO l
        c = chars[i,1]
        s = SEQ(c)
        BEGIN CASE
            CASE s = 27
                c = '<esc>'
            CASE s = 0
                c = 'char(0)'
            CASE s < 27
                c = '<ctrl-':CHAR(64+s):'>'
            CASE 1
                p = OCONV(c, 'MCP')
                IF c # p THEN
                    c = '<hex ':OCONV(c, 'MX'):'>'
                END
        END CASE
        newchars := c
    NEXT i
    CRT setting:' (':newchars:') @(-':
    INPUT nbr:
    CRT ')'
    chars = @(-nbr)
    ECHO ON
    CRT
    RETURN
