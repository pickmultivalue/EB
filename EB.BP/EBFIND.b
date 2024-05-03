! PROGRAM EBFIND
    DEFFUN EBJSHOW()
    CALL JBASEParseCommandLine1(dashes, parens, sent)
    fname = ''
    types = ''
    dir = '.'
    ignore = ''
    sort = ''
    nocase = ''
    verbose = @FALSE
    findkey = ''
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
            CRT '-f"file"      search file'
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
        FINDSTR '-f' IN dashes SETTING pos THEN
            fname = dashes<pos>[3,99]
            IF INDEX(\'"\:'\', fname[1,1], 1) THEN
                fname = FIELD(fname, fname[1,1], 2)
            END
        END
        FINDSTR '-i' IN dashes SETTING pos THEN
            nocase = 'i'
        END
        FINDSTR '-k' IN dashes SETTING pos THEN
            findkey = dashes<pos>[3,99]
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
    IF LEN(sent) EQ 0 AND NOT(findkey OR types) THEN STOP
    IF LEN(fname) OR NOT(LEN(EBJSHOW('-c grep'))) THEN
        IF LEN(fname) EQ 0 THEN fname = sent<1>
        find_cmd = 'SEARCH ':fname
        filter = ''
        IF LEN(types) THEN
            filter = '"[.':types:'"'
        END
        IF LEN(findkey) THEN
            filter = '"':findkey:'"'
        END
        IF sent<1> EQ 'EBFIND' THEN DEL sent<1>
        IF LEN(filter) THEN
            IF LEN(sent) THEN DATA find_cmd
            find_cmd = 'SSELECT ':fname:' = ':filter
        END ELSE find_cmd := ' *'
        IF sent[1,1] EQ '"' THEN
            CONVERT @AM:'"' TO ' ' IN sent
        END
        IF LEN(sent) THEN
            DATA sent
            DATA ''
        END
        EXECUTE find_cmd CAPTURING io RTNLIST list
    END ELSE
        sent = CHANGE(sent, @AM, ' ')
        IF sent[1,1] NE '"' THEN sent = DQUOTE(sent)
        find_cmd = 'find ':dir
        IF LEN(ignore) THEN find_cmd := ' -path ':DQUOTE(ignore):' -prune -o'
        find_cmd:= ' -type f'
        IF findkey THEN
            find_cmd:= ' -name ':DQUOTE(findkey)
        END ELSE
            IF LEN(types) THEN
                matching = ''
                FOR t = 1 TO DCOUNT(types, ',')
                    matching<-1> = '-name "*.':FIELD(types,',',t):'"'
                NEXT t
                find_cmd:= ' \( ':CHANGE(matching, @AM, ' -oE '):' \)'
            END
        END
        find_cmd:= ' -exec grep -E'
        IF NOT(LEN(types)) THEN
            find_cmd := ' -I'
        END
        find_cmd:= ' -l':nocase:' -- ':sent:' {} \;':sort
        IF verbose THEN CRT;CRT find_cmd;CRT
        EXECUTE @IM:'k':find_cmd CAPTURING list
    END
    IF LEN(list) THEN
        list = SORT(list)
        OPEN fname THEN
            EXECUTE 'EB ':fname PASSLIST list
        END ELSE
            EXECUTE @IM:'kEB ':TRIM(fname:' ':CHANGE(list, @AM, ' '))
        END
    END ELSE CRT 'No match'
