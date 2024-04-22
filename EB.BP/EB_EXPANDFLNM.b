    FUNCTION EB_EXPANDFLNM(FLNM)
    COMMON /EB_EXPANDFLNM/ filenames, paths
    IF UNASSIGNED(filenames) THEN
        filenames = ''
        paths = ''
    END
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
        FullPath = CHANGE(FullPath, DIR_DELIM_CH, @AM)
        dc = DCOUNT(FullPath, @AM)
        FOR i = dc TO 2 STEP -1
            IF FullPath<i> EQ '.' THEN DEL FullPath<i>
        NEXT
        FullPath = CHANGE(FullPath, @AM, DIR_DELIM_CH)
        result = DICT:FullPath
        INS FLNM BEFORE filenames<pos>
        INS result BEFORE paths<pos>
    END

    RETURN result
