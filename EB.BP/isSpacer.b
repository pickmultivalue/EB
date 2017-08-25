* @(#) isSpacer.b Ported to jBASE 07:23:52  18 FEB 2010
    FUNCTION isSpacer(token)
    INCLUDE EB.INCLUDES lex.h
    IF TRIM(token) = "" THEN RETURN IS_SPACER
    RETURN IS_UNKNOWN
