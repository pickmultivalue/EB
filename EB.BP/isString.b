* @(#) isString.b Ported to jBASE 07:23:52  18 FEB 2010
    FUNCTION isString(token)
    INCLUDE EB.INCLUDES lex.h
    test = TRIM(token)
    IF LEN(test) < 2 THEN RETURN IS_UNKNOWN
    IF test[1,1] NE test[-1,1] THEN RETURN IS_UNKNOWN
    FINDSTR test[1,1] IN wraps SETTING a,b,c THEN RETURN IS_STRING
    RETURN IS_UNKNOWN
