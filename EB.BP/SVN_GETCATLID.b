    FUNCTION SVN_GETCATLID(ItemName)
!
! Function to return full source path of a catalogged program
!
    INCLUDE JBC.h
    DEFFUN GET_CATALOG_FILE()
    DEFFUN GETFULLPATH()
    DEFFUN SVN_SRC_STATUS()
    DEFFUN SVN_GETHOMEPATH()
    EQU TRUE TO 1, FALSE TO 0
!
    INCLUDE EB.INCLUDES SRCDBG
    FileName = GET_CATALOG_FILE(ItemName)
    FileName = GETFULLPATH(FileName)
!
    OPEN FileName TO F.Source ELSE
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
