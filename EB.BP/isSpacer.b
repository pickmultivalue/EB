    FUNCTION isSpacer(token)
    INCLUDE EB.INCLUDES lex.h
    IF TRIM(token) = "" THEN RETURN IS_SPACER
    RETURN IS_UNKNOWN
