    FUNCTION EBGETHOME
!
    INCLUDE JBC.h
!
    DEFFUN EBJSHOW()
!
    io = EBJSHOW('-c EB')
    FINDSTR DIR_DELIM_CH:'bin':DIR_DELIM_CH:'EB' IN io SETTING home THEN
        path = TRIM(io<home>)
        path = FIELD(path, ' ', DCOUNT(path, ' '))
        path = path[1, INDEX(path, DIR_DELIM_CH:'bin', 1)]
    END ELSE path = ''
!
    RETURN path
