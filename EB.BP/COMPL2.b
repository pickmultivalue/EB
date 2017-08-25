* @(#) COMPL2.b Ported to jBASE 07:23:52  18 FEB 2010
* PROGRAM COMPL
    dir1 = SENTENCE(1)
    dir2 = SENTENCE(2)
    
    OPEN dir1 TO F.dir1 ELSE STOP 201, dir1
    OPEN dir2 TO F.dir2 ELSE STOP 201, dir2
    
    badsuffixes = 'o':@AM:'obj':@AM:'so':@AM:'log'
    
    SELECT F.dir1
    list = ''
    LOOP WHILE READNEXT key DO
        suffix=FIELD(key,'.',DCOUNT(key,'.'))
        LOCATE suffix IN badsuffixes SETTING pos ELSE
        READ rec1 FROM F.dir1,key THEN
        READ rec2 FROM F.dir2,key THEN
        rec1=TRIM(rec1,@AM,'T')
        rec2=TRIM(rec2,@AM,'T')
        Diff=0
        IF rec1=rec2 THEN
        AM1=DCOUNT(rec1,@AM)
        AM2=DCOUNT(rec2,@AM)
        IF AM1=AM2 THEN
        FOR a=1 TO AM1 UNTIL Diff
            line1=TRIM(rec1<a>,@VM,'T')
            line2=TRIM(rec2<a>,@VM,'T')
            IF line1=line2 THEN
            MV1=DCOUNT(line1,@VM)
            MV2=DCOUNT(line2,@VM)
            IF MV1=MV2 THEN
            FOR m=1 TO MV1 UNTIL Diff
                value1=TRIM(line1<1,m>,@SVM,'T')
                value2=TRIM(line2<1,m>,@SVM,'T')
                Diff=(value1#value2)
            NEXT m
        END ELSE Diff=1
    END ELSE Diff=1
NEXT a
END ELSE Diff=1
END ELSE Diff=1
IF Diff THEN list<-1>=key
END
END
END
REPEAT
IF LEN(list) THEN
EXECUTE 'COMPARE_ITEM ':dir1:' ':dir2 PASSLIST list
END ELSE CRT 'No differences'
