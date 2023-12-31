    FUNCTION SVN_REVERT(FullPaths)
!
! Function to add a program to the source control repository
!
! Function returns null if successful.
! Anything else is an error message
!
! 01 DEC 2009 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    DEFFUN SVN_EXEC()
    DEFFUN SVN_GETHOMEPATH()
    DEFFUN SVN_CLEAN()
    DEFFUN SRC_OPENLOCKS()
    DEFFUN SVN_SRC_STATUS()
    DEFFUN GET_CATALOG_FILE()
    EQU TRUE TO 1, FALSE TO 0
!
    IF NOT(SRC_OPENLOCKS(F.Locks)) THEN
        RETURN 'SRC.LOCKS is not a file name'
    END
!
    shell = CHAR(255):'k'
    shellend = ' 2>&1'
    INCLUDE EB.INCLUDES SRCDBG
!
    nbr_items = DCOUNT(FullPaths, @AM)
    FOR i = 1 TO nbr_items
        FullPath = FullPaths<i>
        EXECUTE shell:'ls -s ':FullPath:shellend CAPTURING IO
        IO = TRIM(IO)
        IF FIELD(IO, ' ', 1) MATCHES "1N0N" THEN
            CALL SPLITFILEPATH(FullPath, FilePath, ItemName)
            sFilePath = FilePath
            itm = sFilePath:DIR_DELIM_CH:ItemName
!
            IF FilePath = SVN_GETHOMEPATH(FilePath) THEN
                OPEN FilePath TO F.Source THEN
                    recatalog = FALSE
                    IF GET_CATALOG_FILE(ItemName) = FilePath THEN
                        EXECUTE 'jshow -c ':ItemName CAPTURING io
                        IF COUNT(io, 'source') > 1 THEN
                            EXECUTE 'DECATALOG ':FilePath:' ':ItemName CAPTURING io
                        END ELSE
                            recatalog = TRUE
                        END
                    END
                    IO = SVN_EXEC('revert ':itm, TRUE)
                    OrigPath = SVN_CLEAN(FilePath, F.Source, ItemName)
                    IF LEN(IO) = 0 THEN       ;! check to see if it was checked out but not changed
                        IO = SVN_SRC_STATUS(FilePath, ItemName)
                        IF LEN(IO) THEN IO = 'Reverted'
                    END
                    IF INDEX(IO, 'does not exist', 1) THEN
                        IO = 'Reverted'
                    END
                    IF recatalog THEN
                        EXECUTE 'CATALOG ':OrigPath:' ':ItemName CAPTURING io
                    END
!
! Change the FilePath to the original
! for the benefit of the calling program
!
                    FullPaths<i> = OrigPath:DIR_DELIM_CH:ItemName
                END
            END ELSE
                IO = SVN_EXEC('revert ':itm, TRUE)
            END
        END ELSE IO=''
    NEXT i
    RETURN IO
