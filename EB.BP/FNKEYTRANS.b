    FUNCTION FNKEYTRANS(chars)
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
            CASE s < 32
                c = '<ctrl-':CHAR(64+s):'>'
            CASE 1
                p = OCONV(c, 'MCP')
                IF c # p THEN
                    c = '<hex ':OCONV(c, 'MX'):'>'
                END
        END CASE
        newchars := c
    NEXT i

    RETURN (newchars)
