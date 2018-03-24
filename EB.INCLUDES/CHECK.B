    IF ITNM 'R#2'#'.b' THEN
        READV REC FROM FIL,ITNM:'.b',1 THEN
            LUK=FLNM:'*':EDIT.MODE:'*':ITNM
            IF LAST.EB<1,1> = LUK THEN
                LUK:='.b'
                LAST.EB<1,1> = LUK
                WRITE LAST.EB ON FG$EB.CONTROL,FG$LOGNAME:'.LAST.EB'
            END
            ITNM:='.b'
            GO READ.ITEM
        END
    END
