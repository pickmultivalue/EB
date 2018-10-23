    FUNCTION SVN_ADD(FullPaths)
!
! Function to add a program to the source control repository
!
! Function returns null if successful.
! Anything else is an error message
!
! 01 DEC 2009 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    DEFFUN SVN_CHECKOUT()
    shell = CHAR(255):'k'
    shellend = ' 2>&1'
    EQU TRUE TO 1, FALSE TO 0
    nbr_items = DCOUNT(FullPaths, @AM)
    INCLUDE EB.INCLUDES SRC_DEBUG
!
    IO = ''
    FOR i = 1 TO nbr_items
        FullPath = FullPaths<i>
        CALL SPLITFILEPATH(FullPath, FilePath, ItemName)
        IO<-1> = SVN_CHECKOUT(FALSE, FilePath, ItemName)
        FullPaths<i> = FilePath:DIR_DELIM_CH:ItemName
    NEXT i
    RETURN IO
