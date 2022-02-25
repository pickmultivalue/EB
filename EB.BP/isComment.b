    FUNCTION isComment(token)
!
    COMMON /EB_LEXER/reservedWords, colors, comments, commentlen, incomment
    INCLUDE EB.INCLUDES lex.h
    EQU tab TO CHAR(9), spc TO ' '
!
    test = TRIM(CHANGE(token, tab, spc))
    hash = 'L#':commentlen
    commentchars = test hash
    terminatechars = test[LEN(test)-commentlen+1, commentlen]
!
    result = IS_UNKNOWN
    incomment = @FALSE
!
    LOCATE commentchars IN comments<1> SETTING cpos THEN
        incomment = LEN(comments<2,cpos>)
        result = IS_COMMENT
    END
!
    LOCATE terminatechars IN comments<2> SETTING cpos THEN
        ppos = INDEX(test, comments<1,cpos>, 1)
        IF NOT(ppos) THEN result = IS_COMMENT
        incomment = @FALSE
    END
!
    IF incomment THEN result = IS_COMMENT
!
    RETURN result
