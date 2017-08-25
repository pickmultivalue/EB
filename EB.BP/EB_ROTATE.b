    SUBROUTINE EB_ROTATE(DYNARRAY)
* @(#) EB_ROTATE.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.ROTATE Ported to jBASE 16:15:16  27 JUL 2001
! Subroutine to rotate dynamic array on an x-y axis
    EQU AM TO CHAR(254),VM TO CHAR(253)
    ACNT=DCOUNT(DYNARRAY,AM)
    VCNT=0
    FOR A=1 TO ACNT
        VMC=DCOUNT(DYNARRAY<A>,VM)
        IF VMC>VCNT THEN VCNT=VMC
    NEXT A
    NARRAY=''
    FOR A=1 TO ACNT
        VMC=DYNARRAY<A>
        FOR V=1 TO VCNT
            NARRAY<V,A>=VMC<1,V>
        NEXT V
    NEXT A
    DYNARRAY=NARRAY
    RETURN
