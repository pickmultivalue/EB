! Execute EB with view mode
!
    CALL JBASEParseCommandLine1(args,opts,sent)
    IF LEN(opts) EQ 0 THEN opts = '('
    opts := 'V'
    EXECUTE 'EB ':TRIM(args:CHANGE(sent, @AM, ' ')):' ':opts
