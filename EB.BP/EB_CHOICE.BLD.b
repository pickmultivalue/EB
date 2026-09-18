    SUBROUTINE EB_CHOICE.BLD(VALUES, ATTRS)
    INCLUDE EB.EQUS EB.COMMONS
    INCLUDE EB.EQUS EBCOM
    EQU CTRL.F TO CHAR(6), CTRL.L TO CHAR(12)
    dc = DCOUNT(ATTRS<1>, @SVM)
    FOR a = 1 TO dc
        attr = ATTRS<1, 1, a>
        IF INDEX(attr, CTRL.F, 1) THEN
            fname = FIELD(attr, CTRL.F, 2)
            attr = attr[COL1()-1]
            fattr = FIELD(fname, CTRL.L, 2)
            fname = fname[1, COL1()-1]
            CALL EB_OPEN('',fname,F.BP,@TRUE,POS)
            attr = attr:CTRL.F:POS:CTRL.L:fattr
        END
        ATTRS<1, 1, a> = attr
    NEXT a
    RETURN
