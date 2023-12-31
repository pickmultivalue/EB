    FUNCTION SRC_READHIST(Level, FileName, ItemName, Date, Stime, Etime, User)
!
! Return the details of an Item from SRC.HISTORY.
! As SRC.HISTORY has a global and temporary location
! (i.e. for checked out items) the Level variable
! determines which file(s) are used:
!
! 0. Main SRC.HISTORY file
! 1. Checked out filename,SRC.HISTORY
! 2. Both
!
! Depending on the other arguments passed in the result
! will vary as follows:
!
! 1. All args have values - Description{:@AM:User}
! 2. Etime is null        - Descriptions:@AM:{Users}:@AM:Etimes
! 3. Stime is (also) null - Descriptions:@AM:{Users}:@AM:Etimes:@AM:Stimes
! 4. Date is (also) null  - Descriptions:@AM:{Users}:@AM:Etimes:@AM:Stimes:@AM:Dates
!
! Additionally if the User arg is not null then only that user's
! change history will be returned (and the User portion will be null)
!
! 27 JAN 2010 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    DEFFUN SRC_OPENHIST()
    INCLUDE EB.INCLUDES GET.USER
    INCLUDE EB.INCLUDES SRCDBG
!
    History = ''
    mvpos = 1       ;! used for placement of values into History
!
! If Level is 0 or 2 then get global history...
!
    IF MOD(Level, 2) = 0 THEN
        IF SRC_OPENHIST(F.Hist, '') THEN
            GOSUB GET_HIST
        END
    END
!
! If it's 1 or 2 then get checked out history
!
    IF Level THEN
        IF SRC_OPENHIST(F.Hist, FileName) THEN
            GOSUB GET_HIST
        END
    END
!
    RETURN History
!
GET_HIST:
!
! Get primary history xref (of dates)
!
    K.Hist_I = ItemName
    READ Dates FROM F.Hist, K.Hist_I THEN
!
! Get the date range
!
        IF LEN(Date) THEN
            LOCATE Date IN Dates<1> BY 'AR' SETTING date_spos THEN
                date_epos = date_spos
            END ELSE date_epos = 0
        END ELSE
            date_spos = 1
            date_epos = DCOUNT(Dates, @VM)
        END
!
        FOR d = date_spos TO date_epos
!
! Date history xref (of start times)
!
            Date = Dates<1, d>
            K.Hist_ID = K.Hist_I:'@':Date
            READ Stimes FROM F.Hist, K.Hist_ID ELSE Stimes = ''
!
! Get the start time range
!
            IF LEN(Stime) THEN
                LOCATE Stime IN Stimes<1> BY 'AR' SETTING stime_spos THEN
                    stime_epos = stime_spos
                END ELSE stime_epos = 0
            END ELSE
                stime_spos = 1
                stime_epos = DCOUNT(Stimes, @VM)
            END
!
            FOR t = stime_spos TO stime_epos
                stime = Stimes<1, t>
!
! Now get the history
!
                K.Hist_IDT = K.Hist_ID:'@':stime
                READ Hist FROM F.Hist, K.Hist_IDT ELSE Hist = ''
                etime = Hist<1>
!
! Check if Etime criteria is there
!
                IF LEN(Etime) = 0 OR Etime = etime THEN
                    user = Hist<2>
                    IF LEN(User) THEN
                        IF User # user THEN CONTINUE
                        user = ''
                    END
                    desc = Hist<3>
!
                    History<1, mvpos> = desc
                    History<2, mvpos> = user
!
! Depending on how specific the request was...
!
                    IF LEN(Etime) = 0 THEN
                        History<3, mvpos> = etime
                        IF LEN(Stime) = 0 THEN
                            History<4, mvpos> = stime
                            IF LEN(Date) THEN
                                History<5, mvpos> = Date
                            END
                        END
                    END
                    mvpos ++
                END
            NEXT t
        NEXT d
    END
    RETURN
