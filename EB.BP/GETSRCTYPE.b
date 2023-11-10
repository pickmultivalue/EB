    FUNCTION GETSRCTYPE
    COMMON /GETSRCTYPE/ processed, result
    DEFFUN EBJSHOW()
    IF UNASSIGNED(processed) OR NOT(processed) THEN
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
        FOR d = 1 TO dc
            type = supported<d>
            EXECUTE ksh:type:cmd CAPTURING io
            io = DOWNCASE(io)
            IF NOT(INDEX(io,reject<d>,1)) THEN
                result = UPCASE(type)
                BREAK
            END
        NEXT d
    END
!
    RETURN result
