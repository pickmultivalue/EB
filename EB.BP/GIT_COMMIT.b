    FUNCTION GIT_COMMIT(FullPaths)
!
! Utility to check in one or more items to GIT
!
! 03 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN GIT_EXEC()
    DEFFUN GIT_CHECKIN()
    DEFFUN GIT_GETROOT()
    DEFFUN GIT_GETPWD()
    DEFFUN SRC_OPENLOCKS()
    DEFFUN SRC_READHIST()
    DEFFUN SRC_HISTORY()
    DEFFUN GIT_GETHOMEPATH()
    DEFFUN GETFULLPATH()
    DEFFUN GETYN()
    INCLUDE JBC.h
    DEFFUN EBJSHOW()
    EQU TRUE TO 1, FALSE TO 0
    shell = @IM:'k'
    shellend = ' 2>&1'
!
    IF NOT(SRC_OPENLOCKS(F.Locks)) THEN STOP 201, 'SRC.LOCKS'
!
!    IF GETENV('JBCRELEASEDIR', jbcreleasedir) THEN
!        TempFile = jbcreleasedir:DIR_DELIM_CH:'tmp'
!    END ELSE
        TempFile = '.'
!
    OPEN TempFile TO F.Temp ELSE STOP 201, TempFile
    IF NOT(GETENV('SRC_EDITOR', Editor)) THEN 
        IF LEN(EBJSHOW('-c vi')) THEN
            Editor = 'vi'
        END ELSE
            Editor = 'EB'
        END
    END
!
    INCLUDE EB.INCLUDES SRC_DEBUG
    INCLUDE EB.INCLUDES GET.HOME
!
    CommitTemplate = '--This line, and those below, will be ignored--'
    CommitTemplate<-1> = '--DO NOT REMOVE THE FOLLOWING LINE--'
    RemoveTemplate = '--Remove any entities below you do not wish to commit now--'
!
    K.Template = '%':user:'_check_in%'
    Message = ''
    StartTemplate = ''
    Template = ''
    EndTemplate = ''
    nbr_items = DCOUNT(FullPaths, @AM)
    LastFilePath = ''
    RootList = ''
    CRT 'Building template, please wait...':
    DESCRIPTIONS = ''
    IF GETENV('pwd',homepath) ELSE
        IF GETENV('PWD',homepath) THEN NULL
    END
!    homepath = GIT_GETHOMEPATH(FullPaths<1>)
    homepath = CHANGE(homepath, DIR_DELIM_CH, '/')
    FOR i = 1 TO nbr_items
        oldPath = FullPaths<i,1>
        newPath = FullPaths<i,2>
        FullPath = homepath:DIR_DELIM_CH:(IF LEN(newPath) THEN newPath ELSE oldPath)
        status = GIT_EXEC('status ':FullPath, TRUE)
        IF LEN(status) = 0 THEN CONTINUE
        CALL SPLITFILEPATH(FullPath, FilePath, ItemName)
        Root = GIT_GETROOT(FilePath)
        LOCATE Root IN RootList SETTING POS ELSE
!            IF POS>1 THEN
!                CRT 'Error: ';!you cannot submit from 2 different GIT check out directories'
!                CRT Root
!                STOP
!            END
            INS Root BEFORE RootList<POS>
        END
        logs = SRC_HISTORY(FilePath, ItemName, '')
        log_datetime = logs<3,1>:logs<4,1>
        history = SRC_READHIST(1, FilePath, ItemName, '', '', '', '')
        description = history<1>
        dates = history<5>
        times = history<4>
        DESCRIPTION = ''
        hist_count = DCOUNT(dates, @VM)
        FOR v = 1 TO hist_count
            datetime =dates<1, v>:times<1, v>
            IF datetime > log_datetime THEN
                DESCRIPTION<1, -1> = description<1, v>
            END
        NEXT v
!
! Build a list of Descriptions. If they're all the same then
! we'll change the Template at the end of this loop
!
        LOCATE DESCRIPTION IN DESCRIPTIONS SETTING DPOS ELSE
            INS DESCRIPTION BEFORE DESCRIPTIONS<DPOS>
        END
        IF LEN(newPath) THEN
            DESCRIPTION<-1> = 'renamed to ':newPath
        END
        Template=DESCRIPTION
        CONVERT ';':@SVM:@VM TO @AM:@AM:@AM IN Template
        Template = CHANGE(Template, @AM:' ', @AM)
        IF nbr_items > 1 THEN
            DescTemplate = Template
            Template = CHANGE(FilePath, homepath, ''):DIR_DELIM_CH:ItemName:@AM:' - ':CHANGE(Template, @AM, @AM:' - ')
            Template = TRIM(Template, '/', 'L')
        END
        StartTemplate<-1> = Template
        EndTemplate<-1> = '    - ':FullPath
    NEXT i
!
! Do we need to consolidate the description?
!
    IF DCOUNT(DESCRIPTIONS, @AM) = 1 AND nbr_items > 1 THEN
        StartTemplate = DescTemplate
    END
    Template = StartTemplate
    Template<-1> = @AM:CommitTemplate
    Template<-1> = RemoveTemplate
    Template<-1> = @AM:EndTemplate
    WRITE Template ON F.Temp, K.Template
    TempDir = GETFULLPATH(TempFile)
    IF TempDir EQ '' THEN TempDir = '.'
    K.Message = TempDir:DIR_DELIM_CH:K.Template
    LOOP
        cmd = 'ls -l --time-style=full-iso ':K.Message:shellend
        EXECUTE shell:cmd CAPTURING before
        EXECUTE shell:Editor:' ':K.Message
        EXECUTE shell:cmd CAPTURING after
        READ Message FROM F.Temp, K.Template ELSE RETURN
        Abandon = (before = after)
        IF Abandon THEN
            Z=TRUE; L=1
            CRT @(0, SYSTEM(3)):@(-3)
            ANS = GETYN('Abandon commit', '', 1)
            IF ANS = 'Y' THEN Message = ''
        END ELSE
            ANS='Y'
            LOCATE RemoveTemplate IN Message SETTING pos THEN
                pos++
            END ELSE
                CRT 'Unable to vet commit message due to the following line missing:'
                CRT
                CRT RemoveTemplate
                CRT
                STOP
            END
            FullPaths = ''
            lastline = DCOUNT(Message, @AM)
            FOR i = pos TO lastline
                FullPath = TRIM(Message<i>)
                IF LEN(FullPath) THEN
                    FullPaths<-1> = TRIM(FullPath[2,999], ' ', 'L')
                END
            NEXT i
        END
    UNTIL ANS='Y' DO REPEAT
    IO = ''
    IF LEN(FullPaths) AND LEN(Message) THEN
        IO = GIT_CHECKIN(GIT_GETHOMEPATH(FullPaths<1>), FullPaths, Message)
    END
    CRT
    DELETE F.Temp, K.Template
    DELETE F.Temp, K.Template:'.BAK'
!
    RETURN CHANGE(IO, @AM, @CR:@LF)
