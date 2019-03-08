! PROGRAM EBFIND
    CALL JBASEParseCommandLine1(dashes, parens, sent)
    IF LEN(sent) EQ 0 THEN STOP
    types = ''
    dir = '.' 
    sort = '' 
    nocase = ''
    findkey = @FALSE 
    IF LEN(dashes) THEN
        FINDSTR '-d' IN dashes SETTING pos THEN
            dir = dashes<pos>[3,99] 
        END
        FINDSTR '-i' IN dashes SETTING pos THEN
            nocase = 'i' 
        END
        FINDSTR '-k' IN dashes SETTING pos THEN
            findkey = @TRUE 
        END
        FINDSTR '-s' IN dashes SETTING pos THEN
            sort = '|/usr/bin/sort' 
        END
        FINDSTR '-t' IN dashes SETTING pos THEN
            types = dashes<pos>[3,99] 
        END
    END
    sent = CHANGE(sent, @AM, ' ')
    IF sent[1,1] NE '"' THEN sent = DQUOTE(sent) 
    find_cmd = 'find ':dir:' -type f' 
    IF findkey THEN
        find_cmd :=' -name ':DQUOTE(sent)
    END ELSE 
        IF LEN(types) THEN
            find_cmd := ' -name "*.':types:'"'
        END 
        find_cmd :=' -exec fgrep -l':nocase:' -- ':sent:' {} \;':sort
    END 
    EXECUTE @IM:'k':find_cmd CAPTURING list
    IF LEN(list) THEN 
        EXECUTE @IM:'kEB ':CHANGE(list, @AM, ' ') 
    END ELSE CRT 'No match' 
