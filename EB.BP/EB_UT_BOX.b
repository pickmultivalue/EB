    SUBROUTINE EB_UT_BOX(MAT SCREEN.PARAMS,FG_STERM,C.COL,C.ROW,C.WIDTH,C.DEPTH,IMAGE,BOX.CLEAR,BOX.DRAW)
*
    DIM SCREEN.PARAMS(100)
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS STD.EQUS
    IF IMAGE=1 THEN PRINT.IT=TRUE ELSE PRINT.IT=FALSE
    IF IMAGE+0<0 THEN SAVE.IT=TRUE ELSE SAVE.IT=FALSE
    PWIDTH=SYSTEM(2)-1
    PDEPTH=SYSTEM(3)-1
    COL=C.COL
    ROW=C.ROW
    IF C.WIDTH GT PWIDTH THEN C.WIDTH=PWIDTH
    IF C.DEPTH GT PDEPTH THEN C.DEPTH=PDEPTH
    WIDTH=C.WIDTH
    DEPTH=C.DEPTH
    EMBED=EMBED.ATTR<1,2>
    DEPTH+=(ROW+2)
    WIDTH-=1
    RNET=INDEX(CRT.BOX,'\\box',1)
    IF COL#'' THEN
        COL-=(1-EMBED)
        IF COL<0 THEN COL=0
        BEGIN CASE
            CASE 1
                BOX.CLEAR=@(COL,ROW)
                IF CLR.BOX='' THEN
                    IF WIDTH=(PWIDTH-1) AND DEPTH=22 AND CLEOP#'' THEN
                        BOX.CLEAR:=CLEOP
                    END ELSE
                        W=WIDTH-(2*EMBED-1)
                        BEGIN CASE
                            CASE CRT.STRING#''
                                BLANK=CRT.STRING:CRT.COLS<1,W-1>:' '
                            CASE WIDTH>=78 AND CLEOL#''
                                BLANK=CLEOL
                            CASE 1
                                BLANK=SPACE(W)
                        END CASE
                        D=DEPTH-1
                        FOR I=ROW TO D
                            BOX.CLEAR:=@(COL,I):BLANK
                        NEXT I
                    END
                    BOX.CLEAR:=@(COL,ROW)
                END ELSE
                    BLANK=CRT.ROWS<1,DEPTH-1>
                    IF BLANK='' THEN BLANK=CRT.COLS<1,DEPTH-1>
                    BOX.CLEAR=@(COL,ROW):CLR.BOX:BLANK:CRT.COLS<1,COL+WIDTH-1>:' '
                END
        END CASE
        IF PRINT.IT THEN
            INCLUDE EB.OS.INCLUDES PC.OFF.CURSOR
            CRT BOX.CLEAR:
            INCLUDE EB.OS.INCLUDES PC.BLOCK.CURSOR
        END ELSE
            IMAGE=BOX.CLEAR
        END
    END
!
    WIDTH-=1
    BEGIN CASE
        CASE 1
            IF COL='' OR CRT.BOX='' THEN
                IF CRT.STRING='' THEN
                    LINE=STR(GR.HZ,WIDTH-2)
                END ELSE LINE=CRT.STRING:CRT.COLS<1,WIDTH-3>:GR.HZ
                IF WIDTH THEN
                    TOP=GRON:GR.TL:LINE:GR.TR:GROFF
                    BOTTOM=GRON:GR.BL:LINE:GR.BR:GROFF
                END ELSE TOP=GR.VT; BOTTOM=GR.VT
                IF COL#'' THEN
                    Col2=COL+WIDTH-1
                    BOX.DRAW=@(COL,ROW)
                    ROW+=1
                END ELSE BOX.DRAW=''
                BOX.DRAW:=TOP
                DEPTH-=2
                IF COL#'' THEN
                    FOR I=ROW TO DEPTH
                        BOX.DRAW:=@(COL,I):GRON:GR.VT:GROFF:@(Col2,I):GRON:GR.VT:GROFF
                    NEXT I
                    BOX.DRAW:=@(COL,I):BOTTOM
                    BOX.DRAW:=@(COL,ROW)
                END ELSE
                    COL=STR(BACK,WIDTH):DOWN; Col2=STR(FWD,WIDTH-2)
                    FOR I=2 TO DEPTH
                        BOX.DRAW:=COL:GRON:GR.VT:GROFF:Col2:GRON:GR.VT:GROFF
                    NEXT I
                    BOX.DRAW:=COL:BOTTOM
                    BOX.DRAW:=COL:STR(UP,DEPTH)
                END
            END ELSE
                BLANK=CRT.ROWS<1,DEPTH-1>
                IF BLANK='' THEN BLANK=CRT.COLS<1,DEPTH-1>
                IF COL#'' THEN BOX.DRAW=@(COL,ROW) ELSE BOX.DRAW=''
                BOX.DRAW:=CRT.BOX:BLANK:CRT.COLS<1,COL+WIDTH-1>
            END
    END CASE
    IF PRINT.IT THEN
        INCLUDE EB.OS.INCLUDES PC.OFF.CURSOR
        CRT BOX.DRAW:
        INCLUDE EB.OS.INCLUDES PC.BLOCK.CURSOR
    END ELSE
        IMAGE:=BOX.DRAW
    END
    RETURN
