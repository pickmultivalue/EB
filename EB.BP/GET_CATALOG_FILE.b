    FUNCTION GET_CATALOG_FILE(itnm)
!
! Program designed to find the source file a given program id
!
! 04 DEC 2009 Peter Falson (jBASE)
!
    EQU MAX TO 999999
    DEFFUN EBJSHOW()
    prog_id = itnm
    IF (prog_id 'R#2') = '.b' THEN
        prog_id = itnm[1, LEN(itnm)-2]
    END ELSE IF (prog_id 'R#6') = '.jabba' THEN
        prog_id = itnm[1, LEN(itnm)-6]
    END
    io = EBJSHOW('-c ':prog_id)
    fname = ''
    loc = 0
    classname = ''
    LOOP
        REMOVE line FROM io AT loc SETTING delim
        line = TRIM(line)
        IF UPCASE(FIELD(line,':',1)) EQ 'METHOD' THEN
            junk = TRIM(UPCASE(FIELD(line,':',4)))
            IF TRIM(UPCASE(FIELD(line,':',4))) EQ prog_id THEN
                class_name = TRIM(FIELD(line, ':', 2)):'.jabba'
                CONTINUE
            END
        END
        POS = INDEX(line, 'source file', 1)
        IF POS THEN
            fname = FIELD(line[POS, MAX], ' ', 3)
            delim = 0
        END
    WHILE delim DO REPEAT
    IF LEN(class_name) THEN
        fname<2> = class_name
    END
!
    RETURN fname
