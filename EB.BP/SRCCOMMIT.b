    PROGRAM SRCCOMMIT
    INCLUDE JBC.h
    $option jabba
    INCLUDE EB.EQUS EB.COMMON
    DEFFUN GETSRCTYPE()
    DEFFUN GIT_REPOSITORY(FilePath)
    MAT GEX=''; MAT EXTRAS=''; MAT OTHER.PARAMS=''
    githome = GIT_REPOSITORY('.')
    IF FIELD(githome, ':', 1) EQ 'fatal' THEN
        githome = GETENV('GITHOME')
    END
    IF LEN(githome) THEN
        rc = GETCWD(currpwd)
        rc = CHDIR(githome)
    END ELSE currpwd = ''
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
    IF LEN(currpwd) THEN rc = CHDIR(currpwd)
