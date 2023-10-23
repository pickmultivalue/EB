    SUBROUTINE EB_FIND(STR.POS,WHOLE.WORDS)
    INCLUDE EB.EQUS EB.COMMON
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS ACT.CODES
    EQU MAX TO 999999
    DEFFUN EB_REGEX()
MAIN$:!
!
! Subroutine to search for SSTR in MREC setting POS
! If WHOLE.WORDS then POS must be at valid variable
!
    DELIMS=' ():;+-*/,&!^#=<>[]@{}':AM:VM:SVM:TAB
!
    CASE.INSENSITIVE=WHOLE.WORDS[4,1]
    REGEX.SEARCH=WHOLE.WORDS[3,1]
    NOCOMMENTS=WHOLE.WORDS[2,1]
    WHOLE.WORDS=WHOLE.WORDS[1,1]
    FOUND=FALSE
    SRCH.STRING=MREC
    IF CASE.INSENSITIVE THEN SRCH.STRING=UPCASE(SRCH.STRING)
    SAVESSTR = SSTR
    SSTRINGS = ''
    ESCFLAG = @FALSE
    FOR L = 1 TO LEN(SSTR)
        C1 = SSTR[L,1]
        BEGIN CASE
            CASE C1 = '\'
                IF NOT(ESCFLAG) THEN
                    ESCFLAG = @TRUE
                    CONTINUE
                END
            CASE C1 = '|'
                IF NOT(ESCFLAG) THEN C1 = @AM
        END CASE
        SSTRINGS := C1
        ESCFLAG = @FALSE
    NEXT L
    DC = DCOUNT(SSTRINGS, @AM)
    STR.POS=0
    IF FG_ACT.CODE=FG_BSEARCH.CODE THEN Temp1=LEN(MREC)
    LOOP
        POS = @FALSE
        FOR CC = 1 TO DC UNTIL POS
            SSTR = SSTRINGS<CC>
            L=LEN(SSTR)
            IF CC LT DC AND L EQ 0 THEN
                CC=2
                SSTR = @AM:SSTRINGS<CC>
                L++
            END ELSE
                IF CC EQ DC-1 AND LEN(SSTRINGS<DC>) EQ 0 THEN
                    CC++
                    SSTR := @AM
                    L++
                END
            END
            IF FG_ACT.CODE=FG_BSEARCH.CODE THEN
                C1 = COUNT(SRCH.STRING[1,Temp1-2],SSTR)
                IF C1 THEN
                    POS = INDEX(SRCH.STRING,SSTR,C1)
                END ELSE
                    C1 = COUNT(SRCH.STRING,SSTR)
                    IF C1 THEN POS = INDEX(SRCH.STRING,SSTR,C1) ELSE
                        POS = 0
                    END
                END
            END ELSE
                IF REGEX.SEARCH THEN
                    POS=EB_REGEX(SRCH.STRING,SSTR, @FALSE)
                END ELSE POS=INDEX(SRCH.STRING,SSTR,1)
            END
            IF POS THEN
                IF POS<1,2> THEN
                    L = POS<1,2>
                    POS = POS<1,1>
                END
                IF WHOLE.WORDS OR NOCOMMENTS THEN
                    IF WHOLE.WORDS THEN
                        CHR1=SRCH.STRING[POS-1,1]
                        CHR2=SRCH.STRING[POS+L,1]
                        FOUND=INDEX(DELIMS,CHR1,1) AND INDEX(DELIMS,CHR2,1)
                    END ELSE FOUND = @TRUE
                    IF FOUND AND NOCOMMENTS THEN
                        COMMENTS = '!*"\':"'"
                        STOPSTR = @AM:COMMENTS
                        CDELIMS = STOPSTR:' ;'
                        SPOS = POS - 1
                        IF SPOS GT 0 THEN
                            LOOP
                                C = SRCH.STRING[SPOS,1]
                            UNTIL INDEX(STOPSTR, C, 1) OR SPOS EQ 1 DO
                                SPOS--
                            REPEAT
                            IF SPOS GT 1 AND INDEX(COMMENTS, C, 1) THEN
                                C = SRCH.STRING[SPOS-1,1]
                                IF INDEX(CDELIMS, C, 1) THEN
                                    FOUND = @FALSE
                                END
                            END
                        END
                    END
                    SRCH.STRING=SRCH.STRING[POS-1,MAX]
                    STR.POS+=POS
                END ELSE
                    FOUND=TRUE
                    STR.POS=POS
                END
            END
        NEXT CC
    WHILE POS AND NOT(FOUND) DO
        SRCH.STRING=SRCH.STRING[3,MAX]
!    STR.POS+=2
    REPEAT
    IF NOT(FOUND) THEN STR.POS=0
    SSTR = SAVESSTR
    RETURN
