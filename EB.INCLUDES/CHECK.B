    jbc_suffixes = '.b':@AM:'.jabba'
    FOR jbcs = 1 TO 2
        jbc_suffix = jbc_suffixes<jbcs>
        IF ITNM[LEN(ITNM)-LEN(jbc_suffix), LEN(jbc_suffix)]#jbc_suffix THEN
            READV REC FROM FIL,ITNM:jbc_suffix,1 THEN
                LUK=FLNM:'*':EDIT.MODE:'*':ITNM
                IF LAST.EB<1,1> = LUK THEN
                    LUK:=jbc_suffix
                    LAST.EB<1,1> = LUK
                    WRITE LAST.EB ON FG_EB.CONTROL,FG_LOGNAME:'.LAST.EB'
                END
                ITNM:=jbc_suffix
                GO READ.ITEM
            END
        END
    NEXT jbcs
