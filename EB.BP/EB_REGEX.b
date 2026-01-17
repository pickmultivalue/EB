    FUNCTION EB_REGEX(rec, expr, all)
    INCLUDE JBC.h
    positions = ''
    lnpos = 0
    sstr = rec
    GOSUB get_rpos
    IF rpos THEN
        dc = DCOUNT(rec, @AM)
        FOR a = 1 TO dc
            line = rec<a>
            sstr = line
            GOSUB get_rpos
            IF rpos THEN
!                FOR c = 1 TO LEN(line)
!                    sstr = line[1, c]
!                    GOSUB get_rpos
!                    IF rpos THEN BREAK
!                NEXT c
!                FOR s = 1 TO c
!                    pos = c - s + 1
!                    sstr = line[pos, s]
!                    GOSUB get_rpos
!                    IF rpos THEN BREAK
!                NEXT c
                positions<-1> = (lnpos + rpos):@VM:LEN(sstr)
                IF NOT(all) THEN BREAK
            END
            lnpos += 1+LEN(line)
        NEXT a
    END
    RETURN(positions)
get_rpos:
    rpos = REGEXP(sstr, expr, REGEXP_EXTENDED)
    RETURN
