    FUNCTION isExpression(variable)
    result = @FALSE
    LeftS  = '(<[{'
    RIGHTS = ')>]}'
    Left  = ''
    QuoteS = \"'\:'\'
    Quote = ''
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

    RETURN result
