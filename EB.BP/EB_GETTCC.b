    SUBROUTINE EB_GETTCC (TLINE,MAT SCREEN.PARAMS,TERM)
    DIM SCREEN.PARAMS(100)
    INCLUDE EB.EQUS TERM.TYPES
    MAT SCREEN.PARAMS=''
    TERM='J'; TERM.TYPE='vt100'
    OPEN 'EB.SECURITY' TO EB.SECURITY THEN
        MATREAD TERM.TYPES FROM EB.SECURITY,TLINE THEN
            TERM=TERM.TYPE
        END
        OPEN 'EB.PARAMS' TO EB.PARAMS THEN
            MATREAD SCREEN.PARAMS FROM EB.PARAMS,'CRT@':TERM.TYPE ELSE
                CRT TERM.TYPE:' not setup in EB.SECURITY'
                STOP
            END
        END
    END
    RETURN
END
