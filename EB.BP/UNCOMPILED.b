    PROGRAM UNCOMPILED
    INCLUDE JBC.h

    jelf = FIELD(GETENV('JELF'), ',', 1)
    fname = SYSTEM(1000)<2>

    OPEN 'DATA',fname:',OBJECT' TO F.object ELSE STOP 201,fname:',OBJECT'
    OPEN fname TO F.source ELSE STOP 201

    IF NOT(SYSTEM(11)) THEN SELECT F.source

    uncompiled = ''
    suffixes = 'b':@AM:'jabba'

    LOOP WHILE READNEXT id DO
        IF INDEX(id, '.v0', 1) THEN CONTINUE
        suffix = FIELD(id, '.', DCOUNT(id, '.'))
        obj_id = id
        LOCATE suffix IN suffixes SETTING jbc THEN
            obj_id = obj_id[1, COL1()-1]
        END ELSE
            jbc = @FALSE
        END
        IF jelf THEN
            obj_id := FILE_SUFFIX_SO
        END ELSE
            IF jbc THEN
                obj_id = FILE_SUFFIX_OBJ
            END ELSE
                obj_id = '$':obj_id
            END
        END
        READV temp FROM F.object, obj_id, 1 ELSE uncompiled<-1> = id
    REPEAT

    IF LEN(uncompiled) = 0 THEN
        CRT 'All compiled'
        STOP
    END ELSE
        dc = DCOUNT(uncompiled, @AM)
        CRT
        CRT dc:' item':(IF dc GT 1 THEN 's' ELSE ''):' selected'
        CRT
    END

    WRITELIST uncompiled ON ''
