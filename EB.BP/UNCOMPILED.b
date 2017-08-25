    PROGRAM UNCOMPILED
    INCLUDE JBC.h

    fname = SYSTEM(1000)<2>

    OPEN 'DATA',fname:',OBJECT' TO F.object ELSE STOP 201,fname:',OBJECT'
    OPEN fname TO F.source ELSE STOP 201

    IF NOT(SYSTEM(11)) THEN SELECT F.source

    uncompiled = ''

    LOOP WHILE READNEXT id DO
        IF INDEX(id, '.v0', 1) THEN CONTINUE
        obj_id = id
        IF obj_id[-2, 2] = '.b' THEN
            obj_id[-2, 2] = FILE_SUFFIX_OBJ
        END ELSE
            obj_id = '$':obj_id
        END
        READV temp FROM F.object, obj_id, 1 ELSE uncompiled<-1> = id
    REPEAT

    IF LEN(uncompiled) = 0 THEN
        CRT 'All compiled'
        STOP
    END

    list_name = 'uncompiled_':fname
    WRITELIST uncompiled ON list_name

    CHAIN 'GET-LIST ':list_name
