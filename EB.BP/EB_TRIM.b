    SUBROUTINE EB_TRIM(NEW,OLD,SPC,TYPE)
!
! Trim trailing/leading/all something
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
            IF NOT(totfound) THEN LENTH=LEN(OLD)
            NEW=OLD[1,LENTH]
        CASE TYPE='L'
            I=1
            LOOP UNTIL OLD[I,SPCLEN] NE SPC DO I+=1 REPEAT
            NEW=OLD[I,LENTH]
        CASE TYPE='A'
            NEW=CHANGE(OLD, SPC, '')
    END CASE
    RETURN
