* @(#) isComment.b Ported to jBASE 07:23:52  18 FEB 2010
    FUNCTION isComment(token)
    COMMON /EB_LEXER/reservedWords, colors, comments, commentlen
    INCLUDE EB.INCLUDES lex.h
    test = TRIM(token)
!    IF test[1,1] MATCHES "*":@VM:"!":@VM:"#" THEN RETURN IS_COMMENT
    IF test[1,commentlen] MATCHES comments THEN RETURN IS_COMMENT
    RETURN IS_UNKNOWN
