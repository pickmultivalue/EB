    SUBROUTINE EB_SETUPSWITCH(HFLNM, SFLNM)
* @(#) EB_SETUPSWITCH.b Ported to jBASE 07:23:52  18 FEB 2010
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(100),EB.FILE.LIST
    COM RDSP(100), CHANGES(100)
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE JBC.h
    DEFFUN EB_EXPANDFLNM()
    DEFFUN SVN_GETHOMEPATH()
    EQU TRUE TO 1, FALSE TO 0
    MAIN$:!
    Y = FIELD(FLNM, DIR_DELIM_CH, DCOUNT(FLNM, DIR_DELIM_CH))
    Z = FLNM
    DICT = DCT<2>
    HFLNM = SVN_GETHOMEPATH(FLNM)
    IF FLNM = HFLNM THEN
    FLNM = EB_EXPANDFLNM(Y)
END ELSE
FLNM = HFLNM
END
CALL EB_OPEN(DICT,FLNM,HFIL,0,POS)
IF NOT(POS) THEN
HFIL = FIL
FLNM = Z
END
HFLNM = FLNM
FLNM = Z
SFIL = FIL
SFLNM = FLNM
RETURN
