    FUNCTION SRC_SENTENCE(UseSRCLOCKS, Quiet)
!
! Command line helper for git,svn,etc utility commands
! Parses the sentence, detects select list
! or works off SRC.LOCKS to generate an array
! of complete paths.
!
! Returns an array of full_paths (i.e. dir and item)
!
    DEFFUN SRC_OPENLOCKS()
    DEFFUN SRC_EXEC()
    DEFFUN SRC_GETCATLID()
    DEFFUN GETFULLPATH()
    INCLUDE JBC.h
    EQU TRUE TO 1, FALSE TO 0
    EQU MAX TO 999999
    shell = CHAR(255):'k'
    shellend = ' 2>&1'
!
    IF UseSRCLOCKS THEN
        IF NOT(SRC_OPENLOCKS(F.Locks)) THEN STOP 201, 'SRC.LOCKS'
        Asterisk = '[*]'
    END ELSE
        Asterisk = '*'
    END
!
    CallStack = SYSTEM(1029)
    CallProg = CallStack<1, 1, 4>
!
    Syntax = ''
    Syntax<-1> = 'Syntax: ':CallProg:' filename item[ item...]'
    Syntax<-1> = ''
    Syntax<-1> = '                or'
    Syntax<-1> = ''
    Syntax<-1> = '        ':CallProg:' item[ item ...]'
    Syntax<-1> = ''
    Syntax<-1> = '                or'
    Syntax<-1> = ''
    Syntax<-1> = '        ':CallProg:' path/item[ path/item ...]'
    Syntax<-1> = ''
    Syntax<-1> = '                or'
    Syntax<-1> = ''
    Syntax<-1> = '        ':CallProg:' (from a select list)'
    Syntax<-1> = ''
    Syntax<-1> = '                or'
    Syntax<-1> = ''
    Syntax<-1> = '        ':CallProg:' ':Asterisk
!
    INCLUDE EB.INCLUDES SRCDBG
    INCLUDE EB.INCLUDES GET.HOME
!
    FullPaths = CHANGE(TRIM(FIELD(OCONV(@SENTENCE, 'G1 999'), '(', 1)), ' ', @AM)
!
    DisplaySyntax = TRUE
    BEGIN CASE
        CASE FullPaths = '?'
        CASE FullPaths = '--help'
        CASE FullPaths = '--syntax'
        CASE FullPaths = '--version'
        CASE 1
            DisplaySyntax = FALSE
            IF (UseSRCLOCKS AND LEN(FullPaths) = 0) OR FullPaths = '.' OR FullPaths[LEN(FullPaths), 1] = '*' OR SYSTEM(11) THEN
                IF SYSTEM(11) THEN
                    IF LEN(FullPaths) THEN
                        FilePath = GETFULLPATH(FullPaths):DIR_DELIM_CH
                        FullPaths = ''
                    END ELSE FilePath = ''
                    LOOP WHILE READNEXT ID DO
                        FullPaths<-1> = FilePath:ID
                    REPEAT
                END ELSE
                    IF FullPaths = '.' THEN
                        FullPaths = ''
                    END ELSE
                        IF FullPaths[LEN(FullPaths), 1] NE '*' THEN
                            READV FullPaths FROM F.Locks, user, 2 THEN
                                FullPaths = CHANGE(FullPaths, @VM, @AM)
                            END ELSE FullPaths = ''
                        END
                    END
                    IF LEN(FullPaths) = 0 OR FullPaths[LEN(FullPaths), 1] = '*' THEN
                        IF NOT(Quiet) THEN
                            CRT 'Generating list from current directory, please wait...':
                        END
                        io = SRC_EXEC('status ':FullPaths, TRUE)
                        io = CHANGE(io, ' + ',' ')
                        FullPaths = ''
                        loc = 0
                        LOOP
                            REMOVE FullPath FROM io AT loc SETTING delim
                            IF INDEX('AMD', FullPath[1,1], 1) THEN
                                FullPaths<-1> = TRIM(FullPath[2, MAX], ' ', 'L')
                            END
                        WHILE delim DO REPEAT
                        IF NOT(Quiet) THEN CRT
                    END
                END
            END ELSE
                FileName = FullPaths<1>
                ItemName = FullPaths<2>
                IF LEN(ItemName) THEN
                    OPEN FileName TO F.Source THEN
                        DEL FullPaths<1>
                        CLOSE F.Source
                        FilePath = GETFULLPATH(FileName)
                        NbrPaths = DCOUNT(FullPaths, @AM)
                        FOR F = 1 TO NbrPaths
                            FullPaths<F> = FilePath:DIR_DELIM_CH:FullPaths<F>
                        NEXT F
                    END
                END
            END
    END CASE
    IF DisplaySyntax THEN FullPaths = ''
!
    IF NOT(Quiet) AND LEN(FullPaths) = 0 THEN
        CRT
        CRT 'Nothing to do...'
        INCLUDE EB.INCLUDES DISPLAY.SYNTAX
    END
!
    NbrPaths = DCOUNT(FullPaths, @AM)
    FOR F = 1 TO NbrPaths
        FullPath = FullPaths<F>
        IF NOT(INDEX(FullPath, DIR_DELIM_CH, 1)) THEN
            FullPath = SRC_GETCATLID(FullPath)
            IF LEN(FullPath) THEN
                FullPaths<F> = FullPath
            END
        END
    NEXT F
!
    RETURN FullPaths
