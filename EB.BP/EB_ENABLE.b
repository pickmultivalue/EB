    OPEN 'MD' TO F.MD ELSE STOP 201,'MD'
    Files = 'EB.BP'
    Files<-1> = 'EB.CONTROL'
    Files<-1> = 'EB.EQUS'
    Files<-1> = 'EB.INCLUDES'
    Files<-1> = 'EB.OS.INCLUDES'
    Files<-1> = 'EB.PARAMS'
    Files<-1> = 'EB.SECURITY'
    Files<-1> = 'EB.TERM-TYPES'
    Files<-1> = 'JET.PASTE'
    Files<-1> = 'POINTER-FILE'
    Files<-1> = 'SVN.HISTORY'
    Files<-1> = 'SVN.LOCKS'
    Qptr = 'Q':@AM:'EB'
    dc = DCOUNT(Files, @AM)
    FOR f = 1 TO dc
        fname = Files<f>
        Qptr<3> = fname
        OPEN fname ELSE
            READ test FROM F.MD, fname THEN
                IF test NE Qptr THEN
                    CRT fname:' exists'
                END
            END ELSE
                WRITE Qptr ON F.MD, fname
                OPEN fname ELSE
                    CRT fname:' invalid'
                    DELETE F.MD, fname
                END
            END
        END
    NEXT f
