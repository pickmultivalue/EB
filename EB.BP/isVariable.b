    FUNCTION isVariable(prior, variable, after, ...)
    variable_can_include_delims = @false
    $option jabba
    args = new object('$vararg')
    if args->size() then
        variable_can_include_delims = args->next()
    end
    result = @FALSE
    DELIMS=' ():;+-*/,&!^#=<>[]{}':@AM:@VM:@SVM:@TAB
    if variable_can_include_delims then
        FOR L = 1 TO LEN(variable)
            C = variable[L,1]
            pos = INDEX(DELIMS, C, 1)
            IF pos THEN
                DELIMS = DELIMS[1,pos-1]:DELIMS[pos+1, LEN(DELIMS)]
            END
        NEXT L
    end
    LeftS  = '(<[{'
    RIGHTS = ')>]}'
    Left  = ''
    QuoteS = \"'\:'\'
    Quote = ''
    IF INDEX(DELIMS, prior, 1) AND INDEX(DELIMS, after, 1) THEN
        FOR P = 1 TO LEN(variable)
            C = variable[P,1]
            IF Quote EQ '' THEN
                Q = INDEX(QuoteS, C, 1)
                IF Q THEN
                    Quote = QuoteS[Q,1]
                    CONTINUE
                END
                IF Left NE '' THEN
                    IF C EQ RIGHTS[Left<1>,1] THEN
                        DEL Left<1>
                        CONTINUE
                    END
                END
                LR = INDEX(LeftS, C, 1)
                IF LR THEN
                    INS LR BEFORE Left<1>
                    CONTINUE
                END
            END ELSE
                IF C EQ Quote THEN
                    Quote = '' ;! reset
                END
            END
        NEXT P
        result = P GT LEN(variable) AND Quote EQ '' AND Left EQ ''
        result = Quote EQ '' AND Left EQ ''
    END

    RETURN result
