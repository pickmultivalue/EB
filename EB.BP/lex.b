* @(#) lex.b Ported to jBASE 07:23:52  18 FEB 2010
    INCLUDE EB.INCLUDES lex.h
    
    DEFFUN initColors()
    
*-- Default set
    colors = initColors()
    
    fileName = SENTENCE(1)
    itemName = SENTENCE(2)
    fin = ""
    
    IF fileName = "" THEN
    fileName = "bp"
    itemName = "fixTable.b"
*        itemName = "jDPTest.b"
    END
    
    OPEN fileName TO file ELSE STOP 201
    READ rec FROM file,itemName ELSE STOP 203
    
    CRT "Processing", fileName:"\":itemName
    
    FOR lineCount = 1 TO DCOUNT(rec,@FM)
        fin<-1> = LOWER(lexLine(rec<lineCount>,colors) )
    NEXT lineCount
    
    CRT
    FOR lineCount = 1 TO DCOUNT(fin,@FM)
        tokens = RAISE(RAISE(fin<lineCount,2>))
        sitokenCounte = 0
        FOR tokenCount = 1 TO DCOUNT(tokens,@FM)
            CRT tokens<tokenCount,3>:tokens<tokenCount,1>:
            sitokenCounte += tokens<tokenCount,4>
        NEXT tokenCount
        CRT C_WHITE
    NEXT lineCount
    STOP
