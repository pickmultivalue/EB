    FUNCTION EB_EXPANDFLNM(FLNM)
!
    DEFFUN GETFULLPATH()
    EQU SPC TO ' '
    INCLUDE JBC.h
    INCLUDE EB.INCLUDES PWD
!
    DICT=FIELD(FLNM,' ',1)
    IF DICT='DICT' THEN
        FLNM=FLNM[COL2()+1,-1]
        DICT:=SPC
    END ELSE DICT=''
    IF INDEX(FLNM,DIR_DELIM_CH,1) THEN
        FullPath=FLNM
    END ELSE
        FullPath = GETFULLPATH(FLNM)
    END
    IF FullPath[1,2]=('.':DIR_DELIM_CH)[1,LEN(FullPath)] THEN
        FullPath=pwd:FullPath[2,LEN(FullPath)]
    END
    RETURN DICT:FullPath
