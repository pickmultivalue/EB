    SUBROUTINE EB_ERRMSG(MSG, wait)
    INCLUDE EB.EQUS EB.COMMON
    GO MAIN$
!
!=========== Program's Purpose ===============
!
! Display error message
!
!=============================================
!
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS SCR.PARAMS
    INCLUDE EB.EQUS CRT.PARAMS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS COLOURS
MAIN$:!
!
    GUI=0
    IF MSG[1,2]='E$' OR MSG[1,2]='D$' THEN CALL EB_BLD_MSG(MSG)
    IF INDEX(CLEOL,TRIM(MSG),1) THEN
        FG_ERROR.FLAG=FALSE
    END ELSE FG_ERROR.FLAG=TRUE
    IF FG_DIALOG.BOX THEN
        IF GUI THEN
            CALL EB_GUI_SEND('m',MSG)
            IF INDEX(MSG,BELL,1) THEN
                CALL EB_GUI_SEND('r','')
                ECHO OFF
                INPUT RESP,1:
                ECHO ON
            END
            MSG=TRIM(MSG)
        END ELSE
            CALL EB_WIN_INFOBOX('Message', MSG, 15,70,20+LEN(MSG),75); RQM
!      CALL EB_WIN_INFOBOX(
!      SCRIPT='
!      CALL WIN_COMSUB('Infobox "':MSG:'",T')
!      IF NOT(INDEX(CLEOL,TRIM(MSG),1)) THEN CALL EB_MERRMSG('',MSG,'',RESP,'OK')
        END
        RETURN      ;! disable until ready
    END
!
    MSG=SWAP(MSG,'|M',' ')
    CONVERT VM:SVM TO AM:AM IN MSG
    IF wait THEN
        W = 0
        D = DCOUNT(MSG, AM)
        FOR A = 1 TO D
            W = MAXIMUM(W:AM:LEN(MSG<A>))
        NEXT A
        W += 6
        C.COL = (PWIDTH-W)/2 "0"
        C.ROW = (PDEPTH-D)/2 "0"
        CALL EB_BOX(C.COL, C.ROW, W, D, TRUE, BOX.CLEAR, BOX.DRAW)
        C.COL += 2
        FOR A = 1 TO D
            CRT @(C.COL, C.ROW+A):MSG<A>:
        NEXT A
        IF wait THEN
            INPUT X,0:
            CRT BOX.CLEAR
        END
        RETURN
    END
    FLD.COLOUR=FG_COLOURS<1,2,1>
    BACK.COLOUR=FG_COLOURS<1,2,2>
    SAVE.COLOURS=FG_CURR.COLOURS:AM:FG_PREV.COLOURS
    COL=0; ROW=PDEPTH
    CRT CURS.OFF<1,1>:
    CRT @(COL,ROW):FG:
    IF INDEX(CLEOL,TRIM(MSG),1) THEN
        CRT CLEOL:
    END ELSE
        HASH='L#':PWIDTH-1
        IF WHITE<1,1>#'' THEN CALL EB_CH_COLOUR(FLD.COLOUR,BACK.COLOUR)
        FG_CURR.COLOURS=SAVE.COLOURS<1>; FG_PREV.COLOURS=SAVE.COLOURS<2>
!
        CRT MSG HASH:
        CRT FG_CURR.COLOURS:
    END
    IF FG_TUTORIAL<1>=1 THEN RQM
!
    RETURN
