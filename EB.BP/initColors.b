* @(#) initColors.b Ported to jBASE 07:23:52  18 FEB 2010
    FUNCTION initColors
    INCLUDE EB.INCLUDES lex.h
*-- This function reads jed.ini, and looks up the color scheme set in the env var COLOR_JED
    commandLookup = ""
    commandLookup<COL_UNKNOWN>       = "COL_UNKNOWN"
    commandLookup<COL_COMMENT>       = "COL_COMMENT"
    commandLookup<COL_NUMBER>        = "COL_NUMBER"
    commandLookup<COL_RESERVED_WORD> = "COL_RESERVED_WORD"
    commandLookup<COL_SEPERATOR>     = "COL_SEPERATOR"
    commandLookup<COL_SPACER>        = "COL_SPACER"
    commandLookup<COL_STRING>        = "COL_STRING"
    commandLookup<COL_BACKGROUND>    = "COL_BACKGROUND"
    commandLookup<COL_MENU_FG>       = "COL_MENU_FG"
    commandLookup<COL_MENU_BG>       = "COL_MENU_BG"
    
    fgLookup=""
    fgLookup<1,1> = "C_BLUE"
    fgLookup<1,2> = "C_CYAN"
    fgLookup<1,3> = "C_WHITE"
    fgLookup<1,4> = "C_YELLOW"
    fgLookup<1,5> = "C_BLACK"
    fgLookup<1,6> = "C_GREEN"
    fgLookup<1,7> = "C_MAGENTA"
    fgLookup<1,8> = "C_BLACK"
    
    fgLookup<2,1> = C_BLUE
    fgLookup<2,2> = C_CYAN
    fgLookup<2,3> = C_WHITE
    fgLookup<2,4> = C_YELLOW
    fgLookup<2,5> = C_BLACK
    fgLookup<2,6> = C_GREEN
    fgLookup<2,7> = C_MAGENTA
    fgLookup<2,8> = C_BLACK
    
    bgLookup=""
    bgLookup<1,1> = "B_BLUE"
    bgLookup<1,2> = "B_CYAN"
    bgLookup<1,3> = "B_WHITE"
    bgLookup<1,4> = "B_YELLOW"
    bgLookup<1,5> = "B_BLACK"
    bgLookup<1,6> = "B_GREEN"
    bgLookup<1,7> = "B_MAGENTA"
    bgLookup<1,8> = "B_BLACK"
    
    bgLookup<2,1> = B_BLUE
    bgLookup<2,2> = B_CYAN
    bgLookup<2,3> = B_WHITE
    bgLookup<2,4> = B_YELLOW
    bgLookup<2,5> = B_BLACK
    bgLookup<2,6> = B_GREEN
    bgLookup<2,7> = B_MAGENTA
    bgLookup<2,8> = B_BLACK
    
*-- Default set
    colors = ""
    colors<COL_COMMENT>        = C_BLUE
    colors<COL_NUMBER>         = C_CYAN
    colors<COL_RESERVED_WORD>  = C_WHITE
    colors<COL_SEPERATOR>      = C_YELLOW
    colors<COL_SPACER>         = C_BLACK
    colors<COL_STRING>         = C_GREEN
    colors<COL_UNKNOWN>        = C_CYAN
    colors<COL_BACKGROUND>     = B_BLACK
    colors<COL_MENU_FG>        = C_WHITE
    colors<COL_MENU_BG>        = B_BLUE
    
*-- Read iNI file,  if none the return a default set
    jb = GETENV("JBCRELEASEDIR")
#ifdef WIN32
    srcName = jb:"\config"
#else
    srcName = jb:"/config"
#endif
    
    OPEN srcName TO file ELSE
    RETURN colors
    END
    
    READ rec FROM file,"jed.ini" ELSE
    RETURN colors
    END
    
    rec = UPCASE(rec)
    config = TRIM(UPCASE(GETENV("COLOR_JED")))
    config := ":"
    
    GOSUB parseFile
    RETURN colors
    
parseFile: 
    FINDSTR config IN rec SETTING startPos ELSE RETURN
    x = startPos
    LOOP WHILE 1
        x += 1
        line = rec<x>
        CHANGE " " TO "" IN line
        IF line[1,1] = "#" THEN CONTINUE
        CHANGE "=" TO @FM IN line
        
        IF line = "" THEN EXIT
        FINDSTR ":" IN line SETTING POS THEN EXIT
        
        lable = line<1>
        color = line<2>
        FINDSTR lable IN commandLookup SETTING colorPos ELSE CONTINUE
        FINDSTR color IN fgLookup<1> SETTING fm,fgCol THEN
        colors<colorPos> = fgLookup<2,fgCol>
    END ELSE
    FINDSTR color IN bgLookup<1> SETTING fm,bgCol THEN
    colors<colorPos> = bgLookup<2,bgCol>
    END
    END
    REPEAT
    RETURN
    
