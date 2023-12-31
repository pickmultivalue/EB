    SUBROUTINE EB_MERGE(MAT RDSP,FIL,REC,CHANGED,MREC,POS,READ.AGAIN,LCOL,LROW,ROW,INDROW,PR,MSG.CLR,MSG.AKN,FLNM,MFLNM,ITNM,MITNM,DCT,MDCT)
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
    DIM MITEM(10000)
!
    READ.AGAIN=0
    CNT=DCOUNT(REC,AM)
    label = "Merge after line "
    CRT MSG.CLR:label:
    ILEN=6; IDATA=INDROW+ROW-1; INPTYPE='N0'
    GOSUB 1500      ;! input the field
    AFTL=OCONV(IDATA,'MCU')
    CRT MSG.AKN:
    IF AFTL=ESC THEN GO 5090
    IF AFTL>CNT THEN AFTL=CNT
5010 CRT MSG.CLR:"Merge from file ":
    ILEN=SYSTEM(2)-LEN(label):@AM:200
    IF MFLNM NE '' THEN IDATA=MFLNM ELSE IDATA=FLNM
    GOSUB 1500      ;! input the field
    MFLNM=IDATA
    CRT MSG.AKN:
    IF MFLNM=ESC THEN GO 5090
!MDCT=DCT
!IF LEN(MFLNM)>6 AND MFLNM[1,5]="DICT " THEN MDCT="DICT"; MFLNM=MFLNM[6,LEN(MFLNM)-5]
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
!    IF IDATA=OCONV(IDATA,'MCP') THEN
!        IF OCONV(IDATA,'MCL')=IDATA THEN MITNM=OCONV(IDATA,'MCU') ELSE MITNM=IDATA
!    END
    MITNM=IDATA
    CRT MSG.AKN:
    IF FG_ACT.CODE=FG_ABT.CODE THEN GO 5090
    IF FG_ACT.CODE=FG_BCK.CODE THEN GO 5010
!
    IF MFLNM=FLNM AND MITNM=ITNM AND POS=1 THEN   ;!merge from memory.
        READ.AGAIN=0; MCNT=CNT
    END ELSE
        READ.AGAIN=1
        WRITE REC ON FIL,ITNM:".BAK"
! can now use REC for the merge item, temporarily.
        READ REC FROM MFL,MITNM ELSE
            CRT MSG.CLR:BELL:"Item Not On File ":PR
            IDATA=''; ILEN=1; GOSUB 1500        ;! DMY=OCONV(IDATA,'MCU')
            GO 5020
        END
        MCNT=DCOUNT(REC,AM)
    END
    X=1
5030 !
    Y=X+PDEPTH-1
    FOR K=X TO Y
        CRT @(0,K-X):CLEOL:K"R#4":" ":; CRTLN=REC<K>;CRT.X=1;CRT.Y=PWIDTH-4; GOSUB CRT.LN
    NEXT K
5040 !
    CRT MSG.CLR:"From line (or F or B)? ":
    ILEN=30; IDATA=""
    GOSUB 1500      ;! input the field
    FRLN=OCONV(IDATA,'MCU')
    CRT MSG.AKN:
    IF FRLN=ESC THEN GO 5090
    IF FRLN="^" OR FG_ACT.CODE=FG_BCK.CODE THEN GO 5020
    OK=0
    IF FRLN[1,1]='L' THEN SCODE=FRLN[2,99]; GOSUB FCODE; GO 5030
    IF FRLN="" OR FRLN="F" THEN FRLN="F1"; OK=1
    IF NUM(FRLN) THEN X=FRLN; GO 5045
    IF FRLN="B" THEN FRLN="B1"; OK=1
    IF FRLN[1,1]="F" AND FRLN[2,5] MATCHES("0N") THEN X+=(FRLN[2,5]*(PDEPTH-1)); OK=1
    IF FRLN[1,1]="B" AND FRLN[2,5] MATCHES("0N") THEN X-=(FRLN[2,5]*(PDEPTH-1)); OK=1
    IF NOT(OK) THEN GO 5040
5045 !
    IF X>MCNT THEN X=MCNT-(PDEPTH-1)
    IF X<1 THEN X=1
    IF NOT(NUM(FRLN)) THEN GO 5030
5050 !
    Y=X+(PDEPTH-1)
    FOR K=X TO Y
        CRT @(0,K-X):CLEOL:K"R#4":" ":; CRTLN=REC<K>[1,PWIDTH-4]; GOSUB CRT.LN
    NEXT K
5060 !
    IF FRLN > X THEN
        CRT MSG.CLR:"From line > last line, please re-enter":
        RQM
        GO 5030
    END
    CRT MSG.CLR:"To line (or E or F or B or ^)? ":
    ILEN=30; IDATA=""
    GOSUB 1500      ;! input the field
    TOLN=OCONV(IDATA,'MCU')
    CRT MSG.AKN:
    IF TOLN=ESC THEN GO 5090
    IF TOLN="^" OR FG_ACT.CODE=FG_BCK.CODE THEN GO 5040
    OK=0
    IF TOLN[1,1]='L' THEN SCODE=TOLN[2,99]; GOSUB FCODE; GO 5030
    IF TOLN='E' THEN TOLN=MCNT
    IF TOLN="" OR TOLN="F" THEN TOLN="F1"; OK=1
    IF NUM(TOLN) THEN GO 5065
    IF TOLN="B" THEN TOLN="B1"; OK=1
    IF TOLN[1,1]="F" AND TOLN[2,5] MATCHES("0N") THEN X+=(TOLN[2,5]*(PDEPTH-1)); OK=1
    IF TOLN[1,1]="B" AND TOLN[2,5] MATCHES("0N") THEN X-=(TOLN[2,5]*(PDEPTH-1)); OK=1
    IF NOT(OK) THEN GO 5060
    IF X>MCNT THEN X=MCNT-(PDEPTH-1)
    IF X<FRLN THEN X=FRLN
    GO 5050
5065 !
    IF TOLN<FRLN THEN GO 5060
    IF TOLN>MCNT THEN TOLN=MCNT
5070 ! perform Merge
    CRT MSG.CLR:"To perform merge, enter <Y>, else press <RETURN> to abort  ":
    IDATA='Y'; ILEN=1; GOSUB 1500; DMY=OCONV(IDATA,'MCU')
    CRT MSG.AKN:
    IF INDEX(ESC:'N',TRIM(DMY),1) THEN GO 5080    ;! abort the merge
    IF DMY NE "Y" THEN GO 5070
    MREC=""
    M=1
    FOR J=FRLN TO TOLN
        MREC<-1>=REC<J>
        MITEM(M)=REC<J>; M=M+1
    NEXT J
    NBR.LINES=TOLN - FRLN +1
    IF READ.AGAIN THEN
        READ REC FROM FIL,ITNM:".BAK" ELSE REC=""
        DELETE FIL,ITNM:".BAK"
    END
    READ.AGAIN=0
    IF REC='' THEN REC=MREC ELSE
        IF LROW>1 THEN CHECK.LINE=RDSP(LROW-1) ELSE CHECK.LINE=REC<INDROW-1>
        LLEN=LEN(CHECK.LINE)
        DUMMY=CHECK.LINE; LNM=1
        CALL EB_FORMAT(DUMMY,I,LNM)
        IF I THEN
!    DUMMY=MREC<1>
            DUMMY=MITEM(1)
            IF FIELD(TRIM(DUMMY),' ',1)='END' THEN I-=2
            LCOL=I; SCRL=1
            J=1
            LOOP WHILE DUMMY[J,1]=' ' AND DUMMY[J,1] NE '' DO J+=1 REPEAT
            I-=J
            DUMMY=SPACE(ABS(I))
        END ELSE DUMMY=''; I=-1
        FOR J=NBR.LINES TO 1 STEP -1
!    CHECK.LINE=MREC<J>
            CHECK.LINE=MITEM(J)
            IF CHECK.LINE[1,COMMENTLEN]=COMMENT ELSE
                IF I<0 THEN
                    CALL EB_EREPLACE(CHECK.LINE,DUMMY,'',1,1)
                END ELSE CHECK.LINE=DUMMY:CHECK.LINE
            END
            MREC<J>=CHECK.LINE
        NEXT J
        INS MREC BEFORE REC<AFTL+1>
        CALL EB_MARKADJ(AFTL+1,NBR.LINES,1)
    END
    CHANGED=1
5080 !
    IF INDROW<1 THEN INDROW=1
5090 !
    IF READ.AGAIN THEN
        READ REC FROM FIL,ITNM:".BAK" ELSE REC=""
        DELETE FIL,ITNM:".BAK"
        READ.AGAIN=0
    END
    RETURN
    INCLUDE EB.INCLUDES CRT.LN
!
1500 !
!
    ECHO ON
    CALL EB_UT_WP(IDATA,INPTYPE,ILEN,1,UMODE,CURS.ON,CURS.OFF,CURS.BLOCK,CURS.LINE,AM,'','',ESC)
    FG_TIMEDOUT=FALSE
    INPTYPE='LIT'
    RETURN
FCODE: ! find code
    CNT=1
    LOOP
        POS=INDEX(REC,SCODE,CNT)
        IF POS THEN Y=DCOUNT(REC[1,POS],AM)
    WHILE POS AND Y<=X DO CNT+=1 REPEAT
    IF POS THEN X=Y
    RETURN
