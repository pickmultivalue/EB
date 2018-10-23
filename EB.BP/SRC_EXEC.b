    FUNCTION SRC_EXEC(Args, Capturing)
    DEFFUN GETSRCTYPE()
    DEFFUN GIT_EXEC()
    DEFFUN SVN_EXEC()
    scType = GETSRCTYPE()
    BEGIN CASE
        CASE scType = 'GIT'
            RETURN GIT_EXEC(Args, Capturing)
        CASE scType = 'SVN'
            RETURN SVN_EXEC(Args, Capturing)
    END CASE
