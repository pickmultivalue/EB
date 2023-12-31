    FUNCTION GET_CATALOG_FILES(prog_id)
!
! Program designed to find all the files a given program id
! is cataloged under
!
! 04 DEC 2009 Peter Falson (jBASE)
!
    EQU MAX TO 999999
    DEFFUN EBJSHOW()
    io = EBJSHOW('-c ':prog_id)
    fnames = ''
    loc = 0
    LOOP
        REMOVE line FROM io AT loc SETTING delim
        line = TRIM(line)
        POS = INDEX(line, 'source file', 1)
        IF POS THEN
            fname = FIELD(line[POS, MAX], ' ', 3)
            LOCATE fname IN fnames SETTING pos ELSE
                INS fname BEFORE fnames<pos>
            END
        END
    WHILE delim DO REPEAT
!
    RETURN fnames
