* @(#) isNumber.b Ported to jBASE 07:23:52  18 FEB 2010
    FUNCTION isNumber(token)
    INCLUDE EB.INCLUDES lex.h
    IF TRIM(token) = "" THEN RETURN IS_UNKNOWN
    IF token MATCHES "1N0N" THEN RETURN IS_NUMBER
    RETURN IS_UNKNOWN
