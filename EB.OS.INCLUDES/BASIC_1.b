#JPPFILENAME "JPPREPLACEBASIC_1.b/EB_GETTCC.b" "1"
#JPPFILENAME "EB_GETTCC.b" "1"
    SUBROUTINE EB_GETTCC (TLINE,MAT SCREEN.PARAMS,TERM)
* @(#) EB_GETTCC.b Ported to jBASE 07:23:52  18 FEB 2010

    DIM SCREEN.PARAMS(100)
    INCLUDE EB.EQUS TERM.TYPES
    MAT SCREEN.PARAMS=''
    TERM='J'; TERM.TYPE='vt100'
    OPEN 'EB.SECURITY' TO EB.SECURITY THEN
        MATREAD TERM.TYPES FROM EB.SECURITY,TLINE THEN
            TERM=TERM.TYPE
        END
        OPEN 'EB.PARAMS' TO EB.PARAMS THEN
            MATREAD SCREEN.PARAMS FROM EB.PARAMS,'CRT*':TERM.TYPE ELSE
                CRT TERM.TYPE:' not setup in EB.SECURITY'
                STOP
            END
        END
    END
    RETURN
END