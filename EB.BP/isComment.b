    FUNCTION isComment(token)
    COMMON /EB_LEXER/reservedWords, colors, comments, commentlen, incomment
    INCLUDE EB.INCLUDES lex.h
    test = TRIM(token)
!    IF test[1,1] MATCHES "*":@VM:"!":@VM:"#" THEN RETURN IS_COMMENT
    commentchars = test[1,commentlen]
    result = IS_UNKNOWN
    LOCATE commentchars IN comments<1> SETTING cpos THEN
        incomment = LEN(comments<2,cpos>)
        result = IS_COMMENT
    END ELSE
        commentchars = test[LEN(test)-commentlen+1, commentlen]
        LOCATE commentchars IN comments<2> SETTING cpos THEN
            ppos = INDEX(test, comments<1,cpos>, 1)
            IF NOT(ppos) THEN result = IS_COMMENT
            incomment = @FALSE
        END
        IF incomment THEN result = IS_COMMENT
    END
    RETURN result
