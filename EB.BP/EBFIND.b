! PROGRAM EBFIND
    CALL JBASEParseCommandLine1(dashes, parens, sent)
    types = ''
    dir = '.' 
    ignore = '' 
    sort = '' 
    nocase = ''
    verbose = @FALSE 
    findkey = @FALSE 
    showHelp = @FALSE
    IF LEN(dashes) THEN
        FINDSTR '-?' IN dashes SETTING pos THEN
            showHelp = @TRUE
        END
        FINDSTR '-h' IN dashes SETTING pos THEN
            showHelp = @TRUE
        END
        IF showHelp THEN
            CRT; CRT 'EBFIND uses "find" to search for matching files then executes EB on the result'
            CRT
            CRT 'Options:'
            CRT
            CRT '-d<dir>       starting dir'
            CRT '-i            ignore case'
            CRT '-k<name>      find file with <name>'
            CRT '-s            sort the result'
            CRT '-t<types>     comma delimited list of types'
            CRT '-I<ignore>    pattern to ignore (e.g. -I"*QA*")'
            CRT '-v            display the find command used'
            CRT
            STOP
        END
        FINDSTR '-d' IN dashes SETTING pos THEN
            dir = dashes<pos>[3,99] 
        END
        FINDSTR '-i' IN dashes SETTING pos THEN
            nocase = 'i' 
        END
        FINDSTR '-k' IN dashes SETTING pos THEN
            findkey = @TRUE 
        END
        FINDSTR '-v' IN dashes SETTING pos THEN
            verbose = @TRUE 
        END
        FINDSTR '-s' IN dashes SETTING pos THEN
            sort = '|/usr/bin/sort' 
        END
        FINDSTR '-t' IN dashes SETTING pos THEN
            types = dashes<pos>[3,99] 
        END
        FINDSTR '-I' IN dashes SETTING pos THEN
            ignore = dashes<pos>[3,99]
        END
    END
    IF LEN(sent) EQ 0 THEN STOP
    sent = CHANGE(sent, @AM, ' ')
    IF sent[1,1] NE '"' THEN sent = DQUOTE(sent) 
    find_cmd = 'find ':dir:' -type f'
    IF NOT(LEN(types)) THEN find_cmd := ' \( ! -iname "*.o*" -a ! -iname "*.so" -a ! -iname "*.dll" \)'
    IF LEN(ignore) THEN find_cmd := ' -path ':DQUOTE(ignore):' -prune -o' 
    IF findkey THEN
        find_cmd :=' -name ':DQUOTE(sent)
    END ELSE 
        IF LEN(types) THEN
            matching = ''
            FOR t = 1 TO DCOUNT(types, ',')
                matching<-1> = '-name "*.':FIELD(types,',',t):'"'
            NEXT t
            find_cmd := ' \( ':CHANGE(matching, @AM, ' -oE '):' \)' 
        END 
        find_cmd :=' -exec fgrep -l':nocase:' -- ':sent:' {} \;':sort
    END
    IF verbose THEN CRT;CRT find_cmd;CRT 
    EXECUTE @IM:'k':find_cmd CAPTURING list
    IF LEN(list) THEN 
        EXECUTE @IM:'kEB ':CHANGE(list, @AM, ' ') 
    END ELSE CRT 'No match' 
