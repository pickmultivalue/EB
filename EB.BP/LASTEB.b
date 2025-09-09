! Initialisation
! ==============
    INCLUDE JBC.h
    DEFFUN EBGETHOME()
    path = EBGETHOME()
    opts = SENTENCE(1)
    IF opts = '-h' OR opts = '-?' OR opts = '--help' THEN
        CRT 'Syntax: LASTEB{ -[v,d]}'
        CRT '        -v full path'
        CRT '        -d containing directory'
        CRT
        STOP
    END
    verbose = INDEX(opts, '-v', 1)
    dir_only = INDEX(opts, '-d', 1)
    chg_dir = INDEX(opts, '-c', 1)
    OPEN path:'EB.CONTROL' TO FG_EB.CONTROL THEN
        READ LAST.EB FROM FG_EB.CONTROL,@LOGNAME:'.LAST.EB' ELSE LAST.EB=''
        LAST.EB = LAST.EB<1,1,1>
        dir = FIELD(FIELD(LAST.EB<1,1,1>,'*',1), @TAB, 1)
        LAST.EB = FIELD(LAST.EB<1,1,1>,@TAB,3,99)
        BEGIN CASE
        CASE verbose
            LAST.EB = dir:DIR_DELIM_CH:LAST.EB
        CASE dir_only
            LAST.EB = dir
        END CASE
        CRT LAST.EB
    END ELSE CRT 'EB.CONTROL not found'
