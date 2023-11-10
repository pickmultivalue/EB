    SUBROUTINE EB_COMPARE(MAT RDSP,FIL,REC,CHANGED,MREC,POS,READ.AGAIN,LCOL,LROW,ROW,INDROW,PR,MSG.CLR,MSG.AKN,FLNM,MFLNM,ITNM,MITNM,DCT,MDCT)
    INCLUDE JBC.h
    INCLUDE EB.INCLUDES EB_LEXER
    INCLUDE EB.INCLUDES lex.h
!
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    GO MAIN$
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS ACT.CODES
    INCLUDE EB.EQUS EB.EQUS
    EQU AM TO CHAR(254), VM TO CHAR(253), SVM TO CHAR(252)
    EQU ESC TO CHAR(27), BELL TO CHAR(7), FALSE TO 0
MAIN$:!
    DIM RDSP(100)
!
    READ.AGAIN=0
    CNT=DCOUNT(REC,AM)
5010 CRT MSG.CLR:"Compare from file "
    ILEN=70
    IF MFLNM NE '' THEN IDATA=MFLNM ELSE IDATA=FLNM
    GOSUB 1500      ;! input the field
    MFLNM=IDATA
    CRT MSG.AKN:
    IF MFLNM=ESC THEN GO 5090
    IF MFLNM=FLNM AND POS=1 THEN        ;! default was accepted
        MFL=FIL
    END ELSE
        OPEN MFLNM TO MFL ELSE
            CRT MSG.CLR:"File  ":MFLNM:" does not exist"
            IDATA=''; ILEN=1; GOSUB 1500;
!            IF OCONV(IDATA,'MCL')=IDATA THEN DMY=OCONV(IDATA,'MCU') ELSE DMY=IDATA
            GO 5010
        END
    END
5020 CRT MSG.CLR:"Item name ":
    ILEN=69
    IF MITNM NE '' THEN IDATA=MITNM ELSE IDATA=ITNM
    GOSUB 1500      ;! input the field
    IF FG_ACT.CODE=FG_OPT.CODE THEN
        VALIDATE=''; VALIDATE<5>=1
        CALL EB_CHOICES(50,5,'','',MFLNM,'',IDATA,1,VALIDATE,0,'L#30',MFLNM:' items')
    END
    MITNM=IDATA
    CRT MSG.AKN:
    IF FG_ACT.CODE=FG_ABT.CODE THEN GO 5090
    IF FG_ACT.CODE=FG_BCK.CODE THEN GO 5010
!
    IF MFLNM=FLNM AND MITNM=ITNM AND POS=1 THEN   ;!merge from memory.
        READ.AGAIN=0; MCNT=CNT
    END ELSE
        READ.AGAIN=1
        WRITE REC ON FIL,ITNM:".COMP2"
! can now use REC for the merge item, temporarily.
        READ REC FROM MFL,MITNM ELSE
            CRT MSG.CLR:BELL:"Item Not On File ":PR
            IDATA=''; ILEN=1; GOSUB 1500        ;! DMY=OCONV(IDATA,'MCU')
            GO 5020
        END
        MCNT=DCOUNT(REC,AM)
        WRITE REC ON FIL,ITNM:".COMP1"
    END
    ITNM1 = FLNM:DIR_DELIM_CH:ITNM:'.COMP1'
    ITNM2 = FLNM:DIR_DELIM_CH:ITNM:'.COMP2'
    EXECUTE 'COMPARE_ITEM ':ITNM1:' ':ITNM2:' -t'
    DELETE FIL,ITNM:".COMP1"
    CHANGED=1
5080 !
    IF INDROW<1 THEN INDROW=1
5090 !
    IF READ.AGAIN THEN
        READ REC FROM FIL,ITNM:".COMP2" ELSE REC=""
        DELETE FIL,ITNM:".COMP2"
        READ.AGAIN=0
    END
    RETURN
    INCLUDE EB.INCLUDES CRT.LN
!
1500 !
!
    ECHO ON
    INPTYPE='AN'
    CALL EB_UT_WP(IDATA,INPTYPE,ILEN,1,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',ESC)
    FG_TIMEDOUT=FALSE
    RETURN
