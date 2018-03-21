    SUBROUTINE EB_TRIM(NEW,OLD,SPC,TYPE)
* @(#) EB_TRIM.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.TRIM Ported to jBASE 16:15:16  27 JUL 2001
! Trim trailing something
!
    SPCLEN=LEN(SPC)
    LENTH=LEN(OLD)-SPCLEN+1
    BEGIN CASE
    CASE TYPE='T'
        totfound = 0
        LOOP
            tmp = OLD[LENTH,SPCLEN]
            found = tmp EQ SPC
            totfound += found
        WHILE found AND LENTH DO LENTH-=1 REPEAT
        IF totfound THEN NEW=OLD[1,LENTH]
    CASE TYPE='L'
        I=1
        LOOP UNTIL OLD[I,SPCLEN]#SPC DO I+=1 REPEAT
        NEW=OLD[I,LENTH]
    CASE TYPE='L'
        NEW=CHANGE(OLD, SPC, '')
    END CASE
    RETURN
END
