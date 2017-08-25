* @(#) isSeparator.b Ported to jBASE 07:23:52  18 FEB 2010
    FUNCTION isSeparator(token)
    INCLUDE EB.INCLUDES lex.h
    FINDSTR token IN breaks SETTING a,b,c THEN RETURN IS_SEPERATOR
    RETURN IS_UNKNOWN
