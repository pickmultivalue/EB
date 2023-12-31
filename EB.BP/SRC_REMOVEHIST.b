    FUNCTION SRC_REMOVEHIST(F.Hist, ItemName)
!
! Remove SRC.HISTORY items (after check in)
!
! Return 0 if any ON ERROR conditions occur
! Return 1 otherwise
!
    INCLUDE EB.INCLUDES SRCDBG
!
    flag = 1
    K.Hist_I = ItemName
    READ Dates FROM F.Hist, K.Hist_I THEN
    date_epos = DCOUNT(Dates, @VM)
    FOR d = 1 TO date_epos
        Date = Dates<1, d>
        K.Hist_ID = K.Hist_I:'@':Date
        READ Stimes FROM F.Hist, K.Hist_ID ELSE Stimes = ''
        stime_epos = DCOUNT(Stimes, @VM)
        FOR t = 1 TO stime_epos
            Stime = Stimes<1, t>
            K.Hist_IDT = K.Hist_ID:'@':Stime
            DELETE F.Hist, K.Hist_IDT ON ERROR GOSUB Set_Err_Flag
        NEXT t
        DELETE F.Hist, K.Hist_ID ON ERROR GOSUB Set_Err_Flag
    NEXT d
    DELETE F.Hist, K.Hist_I ON ERROR GOSUB Set_Err_Flag
    END
    RETURN flag
!
    Set_Err_Flag:
!
    flag = 0
    RETURN
