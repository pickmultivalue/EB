! PROGRAM EBKEYS
    DIM EB$CHARS(100)
    INCLUDE EB.EQUS EB.CHARS
    cmd = SYSTEM(1000)
    OPEN 'EB.PARAMS' TO f.params ELSE STOP 201, 'EB.PARAMS'
    OPEN 'EB.EQUS' TO f.equs ELSE STOP 201, 'EB.EQUS'
    READ equs FROM f.equs, 'EB.CHARS' ELSE STOP 202, 'EB.CHARS'
    ttype = cmd<2>
    specific_setting = cmd<3>
    key = 'EB.CHARS@':ttype
    MATREAD EB$CHARS FROM f.params, key THEN
        status = 'updated'
    END ELSE
        MAT EB$CHARS =''
        CRT 'New term type...';RQM
        status = 'created'
    END
    loc = 0
    LOOP
        REMOVE line FROM equs AT loc SETTING delim
        setting = TRIM(FIELD(line, '!', 2))
        IF NOT(LEN(specific_setting)) OR setting = specific_setting THEN
            attr = OCONV(line, 'MCN')
            IF LEN(setting) AND attr MATCHES '1N0N' THEN
                chars = EB$CHARS(attr)
                save_chars = chars
                GOSUB get_key
                IF LEN(chars) > 0 THEN
                    IF chars = ' ' THEN chars = ''
                    FOR i = 2 TO 100
                        existing = EB$CHARS(i)
                        IF existing = chars AND i # attr THEN
                            CRT 'Sequence already in use in attr ':i:
                            BREAK
                        END
                    NEXT i
                    IF chars # save_chars THEN
                        IF i > 100 THEN
                            EB$CHARS(attr) = chars
                            CRT OCONV(chars, 'MX')
                            CRT OCONV(chars, 'MCP')
                        END
                    END
                END
            END
        END

    WHILE delim DO REPEAT
    len = 0
    FOR attr = 2 TO 100
        l = LEN(EB$CHARS(attr))
        IF l > len THEN len = l
    NEXT attr
    EB$CHARS(1) = len
    attr = ''
    setting = 'Update ':key
    chars = 'N'
    GOSUB get_key
    IF OCONV(chars, 'MCU') = 'Y' THEN
        MATWRITE EB$CHARS ON f.params, key
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
    CRT attr,setting:' (':newchars:') ? ':
    ECHO OFF
    chars = ''
    timeout = 1000
    LOOP
        INPUT c,0: FOR timeout ELSE BREAK
        chars := c
        timeout = 1
    REPEAT
    IF chars = CHAR(13) THEN chars = ''
    ECHO ON
    CRT
    RETURN
