    SUBROUTINE EB_MAKEHIST(F.HISTFILE,ITNM,CHANGED.BY,DESCRIPTION)
* @(#) EB_MAKEHIST.b Ported to jBASE 07:23:52  18 FEB 2010
!
    READU HISTITEM FROM F.HISTFILE, ITNM ELSE HISTITEM = ''
    Change_Text = DESCRIPTION<1, 1>
    Start_Time = DESCRIPTION<1, 2>
    End_Time = DESCRIPTION<1, 3>
    LOCATE DATE() IN HISTITEM<1> BY 'AR' SETTING dpos ELSE
    INS DATE() BEFORE HISTITEM<1, dpos>
    INS '' BEFORE HISTITEM<2, dpos>
    INS '' BEFORE HISTITEM<3, dpos>
    INS '' BEFORE HISTITEM<4, dpos>
    INS '' BEFORE HISTITEM<5, dpos>
    END
    LOCATE Start_Time IN HISTITEM<2, dpos> BY 'AR' SETTING tpos ELSE
    INS Start_Time BEFORE HISTITEM<2, dpos, tpos>
    INS '' BEFORE HISTITEM<3, dpos, tpos>
    INS '' BEFORE HISTITEM<4, dpos, tpos>
    INS '' BEFORE HISTITEM<5, dpos, tpos>
    END
    HISTITEM<3, dpos, tpos> = Change_Text
    HISTITEM<4, dpos, tpos> = End_Time
    HISTITEM<5, dpos, tpos> = CHANGED.BY
    WRITE HISTITEM ON F.HISTFILE, ITNM
!
    RETURN
