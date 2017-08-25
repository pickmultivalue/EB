* @(#) nextToken.b Ported to jBASE 07:23:52  18 FEB 2010
    FUNCTION nextToken(currentPos,endPos,line,lastProcessed)
    
    inQuotes = 0
    buildingToken = 0
    quoteChar = ""
    token = ""
    result = ""
    buildingSpace = 0
    
    INCLUDE EB.INCLUDES lexCharTypes.h
    
    FOR lastProcessed = currentPos TO endPos
        char = line[lastProcessed,1]
        
        BEGIN CASE
    CASE INDEX(wraps,char,1)
        IF inQuotes THEN
        IF quoteChar = char THEN
        token := char
        result = token
        RETURN result
    END ELSE token := char
END ELSE
inQuotes = 1
quoteChar = char
token := char
END

CASE inQuotes
token := char
buildingToken = 1

CASE char = " "
IF buildingToken THEN
result = token
lastProcessed -= 1
END ELSE
x = 0
IF line[ (lastProcessed+1) , 1] = " " THEN
LOOP WHILE line[ (lastProcessed) , 1] = " "
lastProcessed += 1
x += 1
REPEAT
result = SPACE(x)
lastProcessed -= 1
END ELSE
result = " "
END
END
RETURN result

CASE INDEX(numbers,char,1)
token := char
buildingToken = 1
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
buildingToken = 1
END CASE
NEXT lastProcessed

result = token
RETURN result
