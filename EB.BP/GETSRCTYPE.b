    FUNCTION GETSRCTYPE(...)
    $option jabba
    vargs = new object('$vararg')
    if vargs->size() then
        location = vargs->next()
    end else
        location = ''
    end
    COMMON /GETSRCTYPE/ processed, result
    DEFFUN EBJSHOW()
    result = ''
    IF UNASSIGNED(processed) OR NOT(processed) OR LEN(result) EQ 0 THEN
        processed = @TRUE
        git = (IF LEN(EBJSHOW('-c git')) THEN 'git' ELSE '')
        svn = (IF LEN(EBJSHOW('-c svn')) THEN 'svn' ELSE '')
        ksh = @IM:'k'
        cmd = ' status 2>&1'
        result = 'UNKNOWN'
        supported = ''
        supported<-1> = git
        supported<-1> = svn
        IF DCOUNT(supported, @AM) EQ 1 THEN RETURN supported
        reject = 'ot a git repository':@AM:'is not a working'
        dc = DCOUNT(supported, @AM)
        if location then location = 'cd ':location:' && '
        rc = PUTENV('HOME=/home/':SYSTEM(50))
        FOR d = 1 TO dc
            type = supported<1>
            EXECUTE ksh:location:type:cmd CAPTURING io
            io = DOWNCASE(io)
            IF NOT(INDEX(io,reject<d>,1)) THEN
                result = UPCASE(type)
                BREAK
            END
        NEXT d
    END
!
    IF NOT(location) AND NOT(result) THEN
        CRT
        CRT 'Please cd to the source repository directory'
        CRT
        STOP
    END
    RETURN result
