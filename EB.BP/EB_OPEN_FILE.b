    SUBROUTINE EB_OPEN_FILE(FILE.NAME,POS)
!
!=========== Program's Purpose ===============
!
! Open file to OPEN.FILES array
!
!=========== Mods/Fixes History ==============
!
!
!=============================================
!
    INCLUDE EB.OS.INCLUDES FILE.COMMON
    EQU TRUE TO 1, FALSE TO 0, MAX.FILES TO 300
    DEFC INT JBASEEmulateGETINT(INT, INT)
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    vm_start=IF_COMPILED_PRIME
    THIS.FILE=FILE.NAME
    DICT=FIELD(THIS.FILE,' ',1)
    IF DICT='DICT' THEN THIS.FILE=THIS.FILE[6,99] ELSE DICT=''
    LOCATE FILE.NAME IN OPEN.FILE.LIST<am_start> SETTING POS ELSE
        IF POS>MAX.FILES THEN POS=10
        OPEN DICT,THIS.FILE TO OPENED.FILES(POS) THEN
            OPEN.FILE.LIST<POS>=FILE.NAME
        END ELSE POS=0
    END
    RETURN
END
