    SUBROUTINE EB_LOWER(record, changed)
    INCLUDE EB.INCLUDES jbcReserved.h
    amc = DCOUNT(record, @AM)
    FOR a = 1 TO amc
        line = record<a>
        wc = DCOUNT(line,' ')
        spc = ' '
        newline = ''
        FOR w = 1 TO wc
            IF w EQ wc THEN spc = ''
            word = FIELD(line,' ',w)
            IF LEN(word) THEN
                LOCATE word IN jbcReserved BY 'AL' SETTING pos THEN
                    word = DOWNCASE(word)
                    changed = 1
                END
            END
            newline := word:spc
            record<a> = newline
        NEXT w
    NEXT a
    RETURN
