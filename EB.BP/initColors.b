    FUNCTION initColors(MAT COLOURS)
    INCLUDE EB.INCLUDES lex.h
    DIM COLOURS(10)
    INCLUDE EB.EQUS COLOURS
    IF LEN(C_WHITE) NE 0 THEN
        WHITE<1,1> = C_WHITE
        YELLOW<1,1> = C_YELLOW
        MAGENTA<1,1> = C_MAGENTA
        RED<1,1> = C_RED
        CYAN<1,1> = C_CYAN
        GREEN<1,1> = C_GREEN
        BLUE<1,1> = C_BLUE
        BLACK<1,1> = C_BLACK
        WHITE<1,2> = B_WHITE
        YELLOW<1,2> = B_YELLOW
        MAGENTA<1,2> = B_MAGENTA
        RED<1,2> = B_RED
        CYAN<1,2> = B_CYAN
        GREEN<1,2> = B_GREEN
        BLUE<1,2> = B_BLUE
        BLACK<1,2> = B_BLACK
    END
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

    fgLookup<2,1> = BLUE<1,1>
    fgLookup<2,2> = CYAN<1,1>
    fgLookup<2,3> = WHITE<1,1>
    fgLookup<2,4> = YELLOW<1,1>
    fgLookup<2,5> = BLACK<1,1>
    fgLookup<2,6> = GREEN<1,1>
    fgLookup<2,7> = MAGENTA<1,1>
    fgLookup<2,8> = BLACK<1,1>

    bgLookup=""
    bgLookup<1,1> = "B_BLUE"
    bgLookup<1,2> = "B_CYAN"
    bgLookup<1,3> = "B_WHITE"
    bgLookup<1,4> = "B_YELLOW"
    bgLookup<1,5> = "B_BLACK"
    bgLookup<1,6> = "B_GREEN"
    bgLookup<1,7> = "B_MAGENTA"
    bgLookup<1,8> = "B_BLACK"

    bgLookup<2,1> = BLUE<1,2>
    bgLookup<2,2> = CYAN<1,2>
    bgLookup<2,3> = WHITE<1,2>
    bgLookup<2,4> = YELLOW<1,2>
    bgLookup<2,5> = BLACK<1,2>
    bgLookup<2,6> = GREEN<1,2>
    bgLookup<2,7> = MAGENTA<1,2>
    bgLookup<2,8> = BLACK<1,2>

*-- Default set
    colors = ""
    IF LEN(BLUE) THEN
        colors<COL_COMMENT>        = BLUE<1,1>
        colors<COL_NUMBER>         = CYAN<1,1>
        colors<COL_RESERVED_WORD>  = WHITE<1,1>
        colors<COL_SEPERATOR>      = YELLOW<1,1>
        colors<COL_SPACER>         = BLACK<1,1>
        colors<COL_STRING>         = GREEN<1,1>
        colors<COL_UNKNOWN>        = CYAN<1,1>
        colors<COL_BACKGROUND>     = BLACK<1,2>
        colors<COL_MENU_FG>        = WHITE<1,1>
        colors<COL_MENU_BG>        = BLUE<1,2>
    END

*-- Read iNI file,  if none the return a default set
    jb = GETENV("JBCGLOBALDIR")
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
