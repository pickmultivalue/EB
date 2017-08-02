    SUBROUTINE EB_MOVE(FIL,REC,MSG.CLR,MSG.AKN,PR,FLNM,MFLNM,ITNM,MITNM,DCT,MDCT)
* @(#) EB_MOVE.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.MOVE Ported to jBASE 16:15:15  27 JUL 2001
!
    INCLUDE EB.EQUS EB.COMMONS
    GO MAIN$
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS ACT.CODES
!
    EQU AM TO CHAR(254), ESC TO CHAR(27), BELL TO CHAR(7)
    MAIN$:!
!
5010 CRT MSG.CLR:"Move to file ":
    ILEN=63
    IF MFLNM#'' THEN IDATA=MFLNM ELSE IDATA=FLNM
    GOSUB 1500      ;! input the field
    MFLNM=OCONV(IDATA,'MCU')
    CRT MSG.AKN:
    IF MFLNM=ESC THEN GO 5090
    IF MFLNM=FLNM THEN        ;! default was accepted
        MFL=FIL
    END ELSE
        MITNM=ITNM
        OPEN MFLNM TO MFL ELSE
            CRT MSG.CLR:"File  ":MFLNM:" does not exist"
            IDATA=''; ILEN=1; GOSUB 1500; DMY=OCONV(IDATA,'MCU')
            GO 5010
        END
    END
5020 CRT MSG.CLR:"Item name ":
    ILEN=69
    IF MITNM#'' THEN IDATA=MITNM ELSE IDATA=ITNM
    GOSUB 1500      ;! input the field
    IF FG$ACT.CODE=FG$OPT.CODE THEN
        VALIDATE=''; VALIDATE<5>=1
        CALL EB_CHOICES(50,5,'','',MFLNM,'',IDATA,1,VALIDATE,0,'L#30',MFLNM:' items')
    END
    IF IDATA=OCONV(IDATA,'MCP') THEN MITNM=OCONV(IDATA,'MCU')
    CRT MSG.AKN:
    IF FG$ACT.CODE=FG$ABT.CODE THEN GO 5090
    IF FG$ACT.CODE=FG$BCK.CODE THEN GO 5010
!
    IF MFLNM=FLNM AND MITNM=ITNM THEN
        GO 5010
    END ELSE
        READ MREC FROM MFL,MITNM THEN
            CRT MSG.CLR:BELL:"Item Already On File ":PR
            IDATA=''; ILEN=1; GOSUB 1500; DMY=OCONV(IDATA,'MCU')
            GO 5020
        END
    END
5070      ! perform Move
    CRT MSG.CLR:"To perform move, enter <Y>, else press <RETURN> to abort  ":
    IDATA='Y'; ILEN=1; GOSUB 1500; DMY=OCONV(IDATA,'MCU')
    CRT MSG.AKN:
    IF INDEX(ESC:'N',TRIM(DMY),1) THEN GO 5090    ;! abort the move
    IF DMY#"Y" THEN GO 5070
    WRITE REC ON MFL,MITNM
    DELETE FIL,ITNM
5090      !
    RETURN
!
1500      !
!
    ECHO ON
    INPTYPE='U'
    CALL EB_UT_WP(IDATA,INPTYPE,ILEN,1,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',ESC)
    RETURN
END
