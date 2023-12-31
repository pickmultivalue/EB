! PROGRAM GITREVERT
!
! Utility to revert one or more items to GIT
!
! 03 DEC 2009 Peter Falson (jBASE)
!
    DEFFUN GIT_REVERT()
    DEFFUN SRC_SENTENCE()
    DEFFUN GIT_SRC_STATUS()
    DEFFUN GETFULLPATH()
    DEFFUN GETYN()
    EQU TRUE TO 1, FALSE TO 0
    EQU MAX TO 999999
    INCLUDE JBC.h
    shell = @IM:'k'
    shellend = ' 2>&1'
!
    IF GETENV('JBCRELEASEDIR', jbcreleasedir) THEN
        TempFile = jbcreleasedir:DIR_DELIM_CH:'tmp'
    END ELSE TempFile = '.'
!
    OPEN TempFile TO F.Temp ELSE STOP 201, TempFile
!
    IF NOT(GETENV('SRC_EDITOR', Editor)) THEN Editor = 'vi'
    INCLUDE EB.INCLUDES SRCDBG
    INCLUDE EB.INCLUDES GET.HOME
!
    K.Template = '%':user:'_revert%'
    FullPaths = SRC_SENTENCE(TRUE, FALSE)
!
    IF LEN(FullPaths) THEN
        Template       = 'The following paths will be reverted'
        Template<-1>   = ' '
        Template<-1>   = 'Note: the character at the start of the'
        Template<-1>   = '      line denotes the status:'
        Template<-1>   = ' '
        Template<-1>   = ' U - unchanged'
        Template<-1>   = ' M - modified'
        Template<-1>   = ' A - added (new item)'
        Template<-1>   = ' ? - status unknown'
        Template<-1>   = ' '
        Template<-1>   = 'Remove any lines you do not wish to revert'
        RemoveTemplate = '==========DO NOT REMOVE THIS LINE==========='
        Template<-1>   = RemoveTemplate
        Template<-1>   = ' '
        loc = 0
        TABS = SPACE(5)
        LOOP
            REMOVE FullPath FROM FullPaths AT loc SETTING delim
            CALL SPLITFILEPATH(FullPath, FilePath, ItemName)
            Type = GIT_SRC_STATUS(FilePath, ItemName)[1,1]
            IF Type = ' ' THEN Type = 'U'
            Template<-1> = Type:TABS:FullPath
        WHILE delim DO REPEAT
!
        WRITE Template ON F.Temp, K.Template
        TempDir = GETFULLPATH(TempFile)
        K.Message = TempDir:DIR_DELIM_CH:K.Template
!
        EXECUTE shell:'ls -al ':K.Message:shellend CAPTURING before
        EXECUTE Editor:' ':K.Message
        READ Message FROM F.Temp, K.Template ELSE STOP
        DELETE F.Temp, K.Template
        CRT
        ANS = GETYN('Continue', 'N', 2)
        IF ANS = 'Y' THEN
            LOCATE RemoveTemplate IN Message SETTING pos THEN
                pos++
            END ELSE
                CRT 'Unable to vet revert message due to the following line missing:'
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
                    Type = FIELD(FullPath, ' ',1)
                    IF INDEX('?AMU', Type, 1) THEN
                        FullPath = FullPath[COL2()+1, MAX]
                    END
                    FullPaths<-1> = FullPath
                END
            NEXT i
            IF LEN(FullPaths) THEN
                CRT 'Reverting the following:'
                CRT
                loc = 0
                LOOP
                    REMOVE FullPath FROM FullPaths AT loc SETTING delim
                    CRT FullPath
                WHILE delim DO REPEAT
                CRT
                CRT GIT_REVERT(FullPaths)
                CRT
            END
        END
    END
