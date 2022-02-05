    FUNCTION SVN_COMMIT(FullPaths)
!
! Utility to check in one or more items to SVN
!
! 03 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN SVN_EXEC()
    DEFFUN SVN_CHECKIN()
    DEFFUN SVN_GETROOT()
    DEFFUN SVN_GETPWD()
    DEFFUN SRC_OPENLOCKS()
    DEFFUN SVN_READHIST()
    DEFFUN GETFULLPATH()
    DEFFUN GETYN()
    INCLUDE JBC.h
    EQU TRUE TO 1, FALSE TO 0
    shell = CHAR(255):'k'
    shellend = ' 2>&1'
!
    IF NOT(SRC_OPENLOCKS(F.Locks)) THEN STOP 201, 'SRC.LOCKS'
!
    IF GETENV('JBCRELEASEDIR', jbcreleasedir) THEN
        TempFile = jbcreleasedir:DIR_DELIM_CH:'tmp'
    END ELSE TempFile = '.'
!
    OPEN TempFile TO F.Temp ELSE STOP 201, TempFile
    IF NOT(GETENV('SVN_EDITOR', Editor)) THEN Editor = 'vi'
!
    INCLUDE EB.INCLUDES SRCDBG
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
    FOR i = 1 TO nbr_items
        FullPath = FullPaths<i>
        Type = FIELD(SVN_EXEC('status ':FullPath, TRUE), ' ', 1)
        IF LEN(Type) = 0 THEN CONTINUE
        CALL SPLITFILEPATH(FullPath, FilePath, ItemName)
        Root = SVN_GETROOT(FilePath)
        LOCATE Root IN RootList SETTING POS ELSE
            IF POS>1 THEN
                CRT 'Error: you cannot submit from 2 different SVN check out directories'
                STOP
            END
            INS Root BEFORE RootList<POS>
        END
        DESCRIPTION = SVN_READHIST(1, FilePath, ItemName, '', '', '', '')
        DESCRIPTION = DESCRIPTION<1>
!
! Build a list of Descriptions. If they're all the same then
! we'll change the Template at the end of this loop
!
        LOCATE DESCRIPTION IN DESCRIPTIONS SETTING DPOS ELSE
            INS DESCRIPTION BEFORE DESCRIPTIONS<DPOS>
        END
        Template = CONVERT(DESCRIPTION, ';':@SVM:@VM, @AM:@AM:@AM)
        Template = CHANGE(Template, @AM:' ', @AM)
        IF nbr_items > 1 THEN
            DescTemplate = Template
            Template = SVN_GETPWD(FilePath, DIR_DELIM_CH, TRUE):DIR_DELIM_CH:ItemName:@AM:' - ':CHANGE(Template, @AM, @AM:' - ')
        END
        StartTemplate<-1> = Template
        IF LEN(Type) THEN EndTemplate<-1> = Type:'    ':FullPath
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
    K.Message = TempDir:DIR_DELIM_CH:K.Template
    LOOP
        cmd = shell:'ls -l --time-style=full-iso ':K.Message:shellend
        EXECUTE cmd CAPTURING before
        EXECUTE Editor:' ':K.Message
        EXECUTE cmd CAPTURING after
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
                    FullPaths<-1> = TRIM(FullPath[2,-1], ' ', 'L')
                END
            NEXT i
        END
    UNTIL ANS='Y' DO REPEAT
    IO = ''
    IF LEN(FullPaths) AND LEN(Message) THEN
        IO = SVN_CHECKIN(FullPaths, Message)
    END
    CRT
    DELETE F.Temp, K.Template
    DELETE F.Temp, K.Template:'.BAK'
!
    RETURN IO
