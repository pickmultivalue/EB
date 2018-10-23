    FUNCTION SRC_UPDATEHIST(FileName, ItemName, Date, Stime, Etime, Description)
!
! Update the relevant SRC.HISTORY item
!
! 27 JAN 2010 Peter Falson (jBASE)
!
! Returns 1 if succussful, 0 if not
!
    INCLUDE JBC.h
    DEFFUN SRC_OPENHIST()
    INCLUDE EB.INCLUDES GET.USER
    INCLUDE EB.INCLUDES SRC_DEBUG
!
    IF NOT(SRC_OPENHIST(F.Hist, FileName)) THEN
        RETURN 0
    END
    Hdescription=Description
    CONVERT @AM:@VM:@SVM TO ';;;' IN Hdescription
!
! Get primary history xref (of dates)
!
    K.Hist_I = ItemName
    READU Dates FROM F.Hist, K.Hist_I ELSE Dates = ''
    LOCATE Date IN Dates<1> BY 'AR' SETTING mvpos ELSE
    INS Date BEFORE Dates<1,mvpos>
    END
!
! Date history xref (of start times)
!
    K.Hist_ID = K.Hist_I:'@':Date
    READU Times FROM F.Hist, K.Hist_ID ELSE Times = ''
    LOCATE Stime IN Times<1> BY 'AR' SETTING mvpos ELSE
    INS Stime BEFORE Times<1,mvpos>
    END
!
! Now get/update the Time history
!
    K.Hist_IDT = K.Hist_ID:'@':Stime
    Hist = ''
    Hist<1> = Etime
    Hist<2> = user
    Hist<3> = Hdescription
    WRITE Hist ON F.Hist, K.Hist_IDT
    WRITE Times ON F.Hist, K.Hist_ID
    WRITE Dates ON F.Hist, K.Hist_I
!
    RETURN 1
