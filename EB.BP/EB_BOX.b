    SUBROUTINE EB_BOX(C.COL,C.ROW,C.WIDTH,C.DEPTH,IMAGE,BOX.CLEAR,BOX.DRAW)
* @(#) EB_BOX.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.BOX Ported to jBASE 16:15:13  27 JUL 2001
    INCLUDE EB.EQUS EB.COMMONS
    GO MAIN$
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS STD.EQUS
    MAIN$:!
!
! Print a Graphic Box
!
!=====================
! Peter Falson
! 9 Aug 1991
!====================
!
! 02/02/93 - mods to allow null col and row to print a box from
!            current cursor position.
! 06/08/92 - check to see if width and depth uses whole screen
!            and use CLEOP and CLEOL where applicable
!
    CALL EB_UT_BOX(MAT SCREEN.PARAMS,FG$STERM,C.COL,C.ROW,C.WIDTH,C.DEPTH,IMAGE,BOX.CLEAR,BOX.DRAW)
    RETURN
END
