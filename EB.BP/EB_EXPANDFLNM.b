    FUNCTION EB_EXPANDFLNM(FLNM)
    INCLUDE EB.INCLUDES EB_EXPANDFLNM
!
    DEFFUN GETFULLPATH()
    EQU SPC TO ' '
    INCLUDE JBC.h
    INCLUDE EB.INCLUDES PWD
    LOCATE FLNM IN filenames SETTING pos THEN
        result = paths<pos>
    END ELSE
        ORIG_FLNM = FLNM
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
        IF INDEX(FullPath, 'jshow', 1) THEN
            RETURN ORIG_FLNM
        END
        result = CHANGE(DICT:FullPath, DIR_DELIM_CH:'.':DIR_DELIM_CH, DIR_DELIM_CH)
        INS FLNM BEFORE filenames<pos>
        INS result BEFORE paths<pos>
    END

    RETURN TRIM(result,DIR_DELIM_CH,'T')
