! PROGRAM EBFIND
    CALL JBASEParseCommandLine1(dashes, parens, sent)
    IF LEN(sent) EQ 0 THEN STOP
    types = ''
    dir = '.' 
    sort = '' 
    IF LEN(dashes) THEN
        FINDSTR '-d' IN dashes SETTING pos THEN
            dir = dashes<pos>[3,99] 
        END
        FINDSTR '-s' IN dashes SETTING pos THEN
            sort = '|/usr/bin/sort' 
        END
        FINDSTR '-t' IN dashes SETTING pos THEN
            types = dashes<pos>[3,99] 
        END
    END
    sent = DQUOTE(CHANGE(sent, @AM, ' '))
    IF LEN(types) THEN
        types = ' -name "*.':types:'"'
    END 
    EXECUTE @IM:'kEB `find ':dir:' -type f ':types:' -exec fgrep -l ':sent:' {} \;':sort:'`' 
