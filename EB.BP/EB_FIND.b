    SUBROUTINE EB_FIND(STR.POS,WHOLE.WORDS)
* @(#) EB_FIND.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.FIND Ported to jBASE 16:15:14  27 JUL 2001
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(100),EB.FILE.LIST
    COM RDSP(100), CHANGES(100)
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS ACT.CODES
    EQU MAX TO 999999
    MAIN$:!
!
! Subroutine to search for SSTR in MREC setting POS
! If WHOLE.WORDS then POS must be at valid variable
!
    DELIMS=' ():;+-*/,&!^#=<>[]@{}':AM:VM:SVM:TAB
!
    FOUND=FALSE
    SRCH.STRING=MREC
    L=LEN(SSTR)+2
    STR.POS=0
    IF FG$ACT.CODE=FG$BSEARCH.CODE THEN Temp1=LEN(MREC)
    LOOP
        IF FG$ACT.CODE=FG$BSEARCH.CODE THEN
            C1 = COUNT(SRCH.STRING[1,Temp1-2],SSTR)
            IF C1 THEN POS = INDEX(SRCH.STRING,SSTR,C1) ELSE
                C1 = COUNT(SRCH.STRING,SSTR)
                IF C1 THEN POS = INDEX(SRCH.STRING,SSTR,C1) ELSE
                    POS = 0
                END
            END
        END ELSE POS=INDEX(SRCH.STRING,SSTR,1)
        IF POS THEN
            IF WHOLE.WORDS THEN
                SRCH.STRING=SRCH.STRING[POS-1,MAX]
                STR.POS+=POS
                CHR1=SRCH.STRING[1,1]
                CHR2=SRCH.STRING[L,1]
                FOUND=INDEX(DELIMS,CHR1,1) AND INDEX(DELIMS,CHR2,1)
            END ELSE
                FOUND=TRUE
                STR.POS=POS
            END
        END
    WHILE POS AND NOT(FOUND) DO
        SRCH.STRING=SRCH.STRING[3,MAX]
!    STR.POS+=2
    REPEAT
    IF NOT(FOUND) THEN STR.POS=0
    RETURN
