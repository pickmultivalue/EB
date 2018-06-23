    FUNCTION lexLine(wholeline, line,colours)
    COMMON /EB_LEXER/reservedWords, colors, comments, commentlen, incomment
* token should have the following structure
*
*   token
*   type
*   col
*   len
*
    INCLUDE EB.INCLUDES lex.h

    result        = line
    currentPos    = 1
    endPos        = LEN(line)+1
    lastProcessed = 1
    commentFlag   = 0
    type          = IS_COMMENT
    col           = colors<COL_COMMENT>

    IF line = ""  THEN RETURN result

*-- Flag as whole line is a comment
    temp = TRIM(wholeline)
    CHANGE " " TO "" IN temp
!    IF temp[1,1] MATCHES "*":@VM:"!":@VM:"#" OR isComment(temp) THEN
    IF isComment(temp) THEN
        result<2> = line:@SM:IS_COMMENT:@SM:colors<COL_COMMENT>:@SM:LEN(line)
        RETURN result
    END

    lastToken = ""
    more = 1

*-- Set a node for the BG Color
*-->    result<2,-1> = @SM:IS_UNKNOWN:@SM:colors<COL_BACKGROUND>:@SM:0

    LOOP WHILE more
        token = nextToken(currentPos,endPos,line,lastProcessed)

*        CRT @(-4):currentPos,endPos,DQUOTE(line),lastProcessed,DQUOTE(token)

        currentPos = lastProcessed + 1
        more = ( currentPos < endPos )
        col = colors<COL_RESERVED_WORD>

        BEGIN CASE
            CASE isNumber(token )
                type = IS_NUMBER
                col = colors<COL_NUMBER>

            CASE isReservedWord(token)
                type = IS_RESERVED_WORD
                col = colors<COL_RESERVED_WORD>

            CASE isSeparator(token)
                IF lastToken = ";" AND ( token = "*" OR  token = "!" ) THEN
                    commentFlag = 1
                    type = IS_COMMENT
                    col = colors<COL_COMMENT>
*-------------- now chop the rest of the line and exit...

                    token = token:line[currentPos,9999999]
                    more = 0

                END ELSE
                    type = IS_SEPERATOR
                    col = colors<COL_SEPERATOR>         ;* this is a test
                END

            CASE TRIM(token) = "" AND token[1,1] = " "        ;* isSpacer(token)
                type = IS_SPACER
                col = colors<COL_RESERVED_WORD>

            CASE isString(token)
                type = IS_STRING
                col = colors<COL_STRING>

            CASE 1
                type = IS_UNKNOWN

                IF token[1,1] = "@" THEN
                    type = IS_RESERVED_WORD
                    col = colors<COL_RESERVED_WORD>
                END ELSE
                    col = colors<COL_UNKNOWN>
                END
        END CASE
        lastToken = token
        result<2,-1> =  token:@SM:type:@SM:col:@SM:LEN(token)
    REPEAT

*-- Append a NULL thats white
*-->    result<2,-1> = @SM:IS_SPACER:@SM:colors<COL_RESERVED_WORD>:@SM:0

*    CRT
*    CRT
*    CRT result
*    INPUT xxxx


    RETURN result
