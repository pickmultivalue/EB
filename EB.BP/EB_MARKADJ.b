    SUBROUTINE EB_MARKADJ(SROW,NBR.ROWS,DIRECTION)
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(500),EB.FILE.LIST
    COM RDSP(100), CHANGES(100)
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS STD.EQUS
    MAIN$:!
    POS=DCOUNT(MARKERS<1>,VM)
    EROW=SROW+NBR.ROWS-1
    FOR I=POS TO 1 STEP -1
        IROW=MARKERS<2,I,1>
        IF DIRECTION<0 THEN   ;! line deletion
            IF IROW>=SROW AND IROW<=EROW THEN
                DEL MARKERS<1,I>
                DEL MARKERS<2,I>
            END ELSE
                IF IROW>SROW THEN
                    MARKERS<2,I,1>=IROW-NBR.ROWS
                END
            END
        END ELSE
            IF IROW>=SROW THEN
                MARKERS<2,I,1>=IROW+NBR.ROWS
            END
        END
    NEXT I
    RETURN
