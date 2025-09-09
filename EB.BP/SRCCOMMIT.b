    PROGRAM SRCCOMMIT
    INCLUDE JBC.h
    $option jabba
    DEFFUN GETSRCTYPE()
    scType = GETSRCTYPE()
    SENT = DELETE(SYSTEM(1000), 1)
    items = ''
    for item in SENT
        if fnOPEN(item, f.dir) then
            EXECUTE 'git status ':item CAPTURING io
            CONVERT @TAB TO @AM IN io
            loc = 0
            LOOP
                REMOVE line FROM io AT loc SETTING delim
                line = TRIM(line)
                IF FIELD(line, ':', 1) EQ 'modified' THEN
                    items<-1> = TRIM(line[COL2()+1,999])
                END
            WHILE delim DO REPEAT
        end else
            items<-1> = item
        end
    next

    EXECUTE scType:'COMMIT ':CHANGE(items, @AM, ' ')
