    FUNCTION SRC_CASE(Value)
!
! Return upper case of string if Windows
!
    INCLUDE JBC.h
!
    IF DIR_DELIM_CH = '\' THEN rValue = UPCASE(Value) ELSE rValue = Value
!
    RETURN rValue
