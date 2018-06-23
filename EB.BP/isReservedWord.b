    FUNCTION isReservedWord(token)
    COMMON /EB_LEXER/reservedWords, colors, comments, commentlen
    DEFC INT JBASEEmulateGETINT(INT, INT)
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    INCLUDE EB.INCLUDES lex.h
    LOCATE token IN reservedWords<am_start> SETTING pos THEN RETURN IS_RESERVED_WORD
    RETURN IS_UNKNOWN
