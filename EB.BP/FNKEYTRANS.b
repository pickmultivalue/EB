    FUNCTION FNKEYTRANS(chars)
    COMMON /FNKEYTRANS/ ttTrans, ttTransSub
    DEFFUN EBJSHOW()
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
                IF c NE p THEN
                    c = '<hex ':OCONV(c, 'MX'):'>'
                END
        END CASE
        newchars := c
    NEXT i
!
! Check for TType translator
!
    IF UNASSIGNED(ttTrans) THEN ttTrans = 0
    IF NOT(ttTrans) THEN
        ttTrans = 'tt':SYSTEM(7):'_TRANS'
        ttTransSub = EBJSHOW('-c ':ttTrans)
    END
    IF LEN(ttTransSub) THEN CALL @ttTrans(newchars)
!
    RETURN (newchars)
