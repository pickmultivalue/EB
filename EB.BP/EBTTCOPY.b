! PROGRAM EBTTCOPY
    cmd = SYSTEM(1000)
    options = SYSTEM(15)
    overwrite = INDEX(options, 'O', 1)
    torig = cmd<2>
    tnew = cmd<3>
    IF LEN(tnew) = 0 THEN
        CRT 'Syntax: EBTTCOPY <original_type> <new_type>'
        STOP
    END
    OPEN 'EB.PARAMS' TO f.params ELSE STOP 201, 'EB.PARAMS'
    prefixes = 'CRT':@AM:'EB.CHARS':@AM:'COLOUR'
    FOR p = 1 TO 3
        key = prefixes<p>:'@':torig
        READ orig FROM f.params, key THEN
            nkey = prefixes<p>:'@':tnew
            READ new FROM f.params, nkey THEN
                IF NOT(overwrite) THEN
                    CRT nkey:' exists'
                    CONTINUE
                END
            END
            WRITE orig ON f.params, nkey
        END ELSE
            STOP 202, key
        END
    NEXT p
