    PROGRAM UNCOMPILED

    fname = SYSTEM(1000)<2>

    OPEN 'DATA',fname:',OBJECT' TO F.object ELSE STOP 201,fname:',OBJECT'
    OPEN fname TO F.source ELSE STOP 201

    IF NOT(SYSTEM(11)) THEN SELECT F.source

    uncompiled = ''

    LOOP WHILE READNEXT id DO
        IF INDEX(id, '.v0', 1) THEN CONTINUE
        READV temp FROM F.object, '$':id, 1 ELSE uncompiled<-1> = id
    REPEAT

    list_name = 'uncompiled_':fname
    WRITELIST uncompiled ON list_name

    CHAIN 'GET-LIST ':list_name
