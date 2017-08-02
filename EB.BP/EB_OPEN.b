    SUBROUTINE EB_OPEN(DNAME,FNAME,F.VAR,MUST.EXIST,POS)
* @(#) EB_OPEN.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.OPEN Ported to jBASE 16:15:15  27 JUL 2001
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(100),EB.FILE.LIST
    COM RDSP(100), CHANGES(100)
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS OTHER.PARAMS
    EQU TRUE TO 1, FALSE TO 0
MAIN$:!
    INCLUDE EB.OS.INCLUDES OS.ERRORS
    INCLUDE JBC.h
!
! OPEN MD first
!
    DFNAME='MD':DIR_DELIM_CH:FG$TACC.NAME
    INFO=DNAME
    INFO<2>=FNAME
    DNAME=''; FNAME='MD'; MUST.EXIST=TRUE
    GOSUB OPEN.FILE
    DNAME=INFO<1>
    FNAME=INFO<2>
    IF FNAME='MD' THEN RETURN
    MUST.EXIST=INFO<3>
    READ FACC FROM F.MD,FNAME THEN
        FACC=FACC<2>:DIR_DELIM_CH:FACC<3>
        DFNAME:=DIR_DELIM_CH:FACC
    END ELSE
        DFNAME=FNAME:DIR_DELIM_CH:FG$TACC.NAME
    END
    GOSUB OPEN.FILE
    IF DNAME='' THEN
        DFNAME=FNAME
    END ELSE DFNAME=DNAME:',':FNAME
    RETURN
OPEN.FILE:!
    IF UNASSIGNED(EB.FILE.LIST) THEN EB.FILE.LIST = ''
    LOCATE DFNAME IN EB.FILE.LIST<am_start> SETTING POS ELSE
        OPEN DNAME,FNAME TO EB.FILES(POS) THEN
            EB.FILE.LIST<POS>=DFNAME
        END ELSE
            IF MUST.EXIST THEN STOP OPER,FNAME
            POS=FALSE
        END
    END
    IF POS THEN F.VAR=EB.FILES(POS)
    RETURN