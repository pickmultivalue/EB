    FUNCTION nextToken(currentPos,endPos,line,lastProcessed)

    EQU spc TO ' '
    inQuotes = @FALSE
    buildingToken = @FALSE
    quoteChar = ""
    token = ""
    result = ""
    buildingSpace = @FALSE

    INCLUDE EB.INCLUDES lexCharTypes.h

    FOR lastProcessed = currentPos TO endPos
        char = line[lastProcessed,1]

        BEGIN CASE
            CASE INDEX(wraps,char,1)
                IF inQuotes THEN
                    IF quoteChar EQ char THEN
                        token := char
                        result = token
                        RETURN result
                    END ELSE token := char
                END ELSE
                    inQuotes = @TRUE
                    quoteChar = char
                    token := char
                END

            CASE inQuotes
                token := char
                buildingToken = @TRUE

            CASE char EQ spc
                IF buildingToken THEN
                    result = token
                    lastProcessed -= 1
                END ELSE
                    x = 0
                    IF line[lastProcessed+1, 1] EQ spc THEN
                        LOOP WHILE line[lastProcessed, 1] EQ spc
                            lastProcessed += 1
                            x += 1
                        REPEAT
                        result = SPACE(x)
                        lastProcessed -= 1
                    END ELSE
                        result = spc
                    END
                END
                RETURN result

            CASE INDEX(numbers,char,1)
                token := char
                buildingToken = @TRUE
            CASE INDEX(breaks,char,1) AND NOT(inQuotes)
                IF buildingToken THEN
                    result = token
                    lastProcessed -= 1
                END ELSE
                    result = char
                END
                RETURN result
            CASE 1
                token := char
                buildingToken = @TRUE
        END CASE
    NEXT lastProcessed

    result = token
    RETURN result
