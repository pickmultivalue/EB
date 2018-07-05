    FUNCTION lexLine(wholeline, line,colours)
    COMMON /EB_LEXER/reservedWords, colors, comments, commentlen, incomment
! token should have the following structure
!
!   token
!   type
!   col
!   len
!
    INCLUDE EB.INCLUDES lex.h
    EQU tab TO CHAR(9), spc TO ' ', sc TO ';'
!
    result        = line
    currentPos    = 1
    endPos        = LEN(line)+1
    lastProcessed = 1
    commentFlag   = 0
    type          = IS_COMMENT
    col           = colors<COL_COMMENT>

    IF line = ""  THEN RETURN result

!-- Flag as whole line is a comment
    temp = TRIM(CHANGE(wholeline, tab, spc))
    CONVERT spc TO "" IN temp
    IF isComment(temp) THEN
        result<2> = line:@SM:IS_COMMENT:@SM:colors<COL_COMMENT>:@SM:LEN(line)
        RETURN result
    END

    lastToken = ""
    more = @TRUE

!-- Set a node for the BG Color
!-->    result<2,-1> = @SM:IS_UNKNOWN:@SM:colors<COL_BACKGROUND>:@SM:0
    LOOP WHILE more
        token = nextToken(currentPos,endPos,line,lastProcessed)

        currentPos = lastProcessed + 1
        more = (currentPos LT endPos)
        col = colors<COL_RESERVED_WORD>

        BEGIN CASE
            CASE isNumber(token)
                type = IS_NUMBER
                col = colors<COL_NUMBER>

            CASE isReservedWord(token)
                type = IS_RESERVED_WORD
                col = colors<COL_RESERVED_WORD>

            CASE isSeparator(token)
                IF lastToken EQ sc AND (LEN(token) AND INDEX("*!/", token[1,1],1)) THEN
                    commentFlag = 1
                    type = IS_COMMENT
                    col = colors<COL_COMMENT>
!-------------- now chop the rest of the line and exit...

                    token := line[currentPos,9999999]
                    more = @FALSE

                END ELSE
                    type = IS_SEPERATOR
                    col = colors<COL_SEPERATOR>        ;! this is a test
                END

            CASE TRIM(token) EQ "" AND token[1,1] EQ spc ;! isSpacer(token)
                type = IS_SPACER
                col = colors<COL_RESERVED_WORD>

            CASE isString(token)
                type = IS_STRING
                col = colors<COL_STRING>

            CASE 1
                IF lastToken EQ sc AND (LEN(token) AND INDEX("*!/", token[1,1],1)) THEN
                    commentFlag = 1
                    type = IS_COMMENT
                    col = colors<COL_COMMENT>
!-------------- now chop the rest of the line and exit...

                    token := line[currentPos,9999999]
                    more = @FALSE
                END ELSE
                    type = IS_UNKNOWN

                    IF token[1,1] EQ "@" THEN
                        type = IS_RESERVED_WORD
                        col = colors<COL_RESERVED_WORD>
                    END ELSE
                        col = colors<COL_UNKNOWN>
                    END
                END
        END CASE
        IF NOT(lastToken EQ sc AND LEN(TRIM(token)) EQ 0) THEN
            lastToken = token
        END
        result<2,-1> =  token:@SM:type:@SM:col:@SM:LEN(token)
    REPEAT

    RETURN result
