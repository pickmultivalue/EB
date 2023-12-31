    FUNCTION GIT_GETCATLID(ItemName)
    INCLUDE EB.EQUS EB.COMMON
!
! Function to return full source path of a catalogged program
!
    INCLUDE JBC.h
    DEFFUN GET_CATALOG_FILE()
    DEFFUN GETFULLPATH()
    DEFFUN GIT_SRC_STATUS()
    DEFFUN GIT_GETHOMEPATH()
    EQU TRUE TO 1, FALSE TO 0
!
    INCLUDE EB.INCLUDES SRCDBG
    FileName = GET_CATALOG_FILE(ItemName)
    FileName = GETFULLPATH(FileName)
!
    CALL EB_OPEN('',FileName,F.Source,0,ok)
    IF NOT(ok) THEN
        RETURN ''       ;! error
    END
!
    READV temp FROM F.Source, ItemName, 1 ELSE
        ItemName := '.b'
        READV temp FROM F.Source, ItemName, 1 ELSE
            RETURN ''       ;! error
        END
    END
!
    RETURN FileName:DIR_DELIM_CH:ItemName
