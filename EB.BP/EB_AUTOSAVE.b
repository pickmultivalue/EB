    SUBROUTINE EB_AUTOSAVE
* @(#) EB_AUTOSAVE.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.AUTOSAVE Ported to jBASE 16:15:13  27 JUL 2001
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(100),EB.FILE.LIST
    COM RDSP(100), CHANGES(100)
    GO MAIN$
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS EB.EQUS
    EQU AM TO CHAR(254)
    MAIN$:!
    WRITE PSTIME:AM:REC ON JET.PASTE,ITNM:'.sav'
    PREV.TIME=TIME()
    RETURN
END
