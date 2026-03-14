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
