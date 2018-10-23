    FUNCTION GETFLNM(filepath)
!
! Return the last portion of a file path
! (i.e. the filename)
!
! 03 DEC 2009 Peter Falson (jBASE)
!
    INCLUDE JBC.h
!
    dc = DCOUNT(filepath, DIR_DELIM_CH)
    IF filepath[LEN(filepath),1] EQ DIR_DELIM_CH THEN dc--
    RETURN FIELD(filepath, DIR_DELIM_CH, dc)
