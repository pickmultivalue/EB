    SUBROUTINE EB_OPEN(DNAME,FNAME,F.VAR,MUST.EXIST,POS)
    INCLUDE EB.INCLUDES EB_OPEN
    INCLUDE EB.EQUS EB.COMMON
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS OTHER.PARAMS
    EQU TRUE TO 1, FALSE TO 0
    MAIN$:!
    INCLUDE EB.OS.INCLUDES OS.ERRORS
    INCLUDE JBC.h
    fullname = TRIM(FNAME:' ':DNAME)
    LOCATE fullname IN dfname SETTING pos THEN
        DFNAME = parsed<pos>
    END ELSE
        DFNAME = CHANGE(FNAME, DIR_DELIM_CH:'.':DIR_DELIM_CH, DIR_DELIM_CH)
        IF MD_flag THEN
            READ FACC FROM F.MD,FNAME THEN
                FACC=FACC<2>:DIR_DELIM_CH:FACC<3>
                DFNAME:=DIR_DELIM_CH:FACC
            END
        END
        INS fullname BEFORE dfname<pos>
        INS DFNAME BEFORE parsed<pos>
    END
    GOSUB OPEN.FILE
    IF DNAME='' THEN
        DFNAME=FNAME
    END ELSE DFNAME=DNAME:',':FNAME
    RETURN
OPEN.FILE:!
    IF UNASSIGNED(EB.FILE.LIST) THEN EB.FILE.LIST = ''
    IF DFNAME[1,3] EQ ('..':DIR_DELIM_CH) THEN
        rc = GETCWD(TMP)
        DFNAME = FIELD(TMP,DIR_DELIM_CH,1,COUNT(TMP,DIR_DELIM_CH)):DFNAME[3,-1]
    END
    LOCATE DFNAME IN EB.FILE.LIST<1, am_start> SETTING POS THEN
        POS = EB.FILE.LIST<2,POS>
    END ELSE
        OPEN DNAME,FNAME TO EB.FILES(POS) THEN
            EB.FILE.LIST<1,POS>=DFNAME
            EB.FILE.LIST<2,POS> = POS
            IF IOCTL(EB.FILES(POS),JBC_COMMAND_GETFILENAME,FullPath) THEN
                FullPath = CHANGE(FullPath, DIR_DELIM_CH:'.':DIR_DELIM_CH, DIR_DELIM_CH)
                LOCATE FullPath IN EB.FILE.LIST<1, am_start> SETTING SPOS ELSE
                    EB.FILE.LIST<1,SPOS> = FullPath
                    EB.FILE.LIST<2,SPOS> = POS
                END
            END
        END ELSE
            IF MUST.EXIST THEN STOP OPER,FNAME
            POS=FALSE
        END
    END
    IF POS THEN F.VAR=EB.FILES(POS)
    RETURN
