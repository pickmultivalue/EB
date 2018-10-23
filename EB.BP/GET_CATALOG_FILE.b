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
    END
    io = EBJSHOW('-c ':prog_id)
    fname = ''
    loc = 0
    LOOP
        REMOVE line FROM io AT loc SETTING delim
        line = TRIM(line)
        POS = INDEX(line, 'source file', 1)
        IF POS THEN
            fname = FIELD(line[POS, MAX], ' ', 3)
            delim = 0
        END
    WHILE delim DO REPEAT
!
    RETURN fname