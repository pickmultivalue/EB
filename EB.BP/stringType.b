* @(#) stringType.b Ported to jBASE 07:23:52  18 FEB 2010
    FUNCTION stringType(type)
    INCLUDE EB.INCLUDES lexEquates.h
    BEGIN CASE
CASE type = IS_COMMENT
    result = "IS_COMMENT"
CASE type = IS_NUMBER
    result = "IS_NUMBER"
CASE type = IS_RESERVED_WORD
    result = "IS_RESERVED_WORD"
CASE type = IS_SEPERATOR
    result = "IS_SEPERATOR"
CASE type = IS_SPACER
    result = "IS_SPACER"
CASE type = IS_STRING
    result = "IS_STRING"
CASE 1
    result = "IS_UNKNOWN"
END CASE
RETURN result
