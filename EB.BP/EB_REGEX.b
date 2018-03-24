  FUNCTION EB_REGEX(rec, expr, all)
  positions = ''
  lnpos = 0
  IF REGEXP(rec, expr) THEN
      dc = DCOUNT(rec, @AM)
      FOR a = 1 TO dc
          line = rec<a>
          IF REGEXP(line, expr) THEN
              FOR c = 1 TO LEN(line)
                  sstr = line[1, c]
                  IF REGEXP(sstr, expr) THEN BREAK
              NEXT c
              FOR s = 1 TO c
                  pos = c - s + 1
                  sstr = line[pos, s]
                  IF REGEXP(sstr, expr) THEN BREAK
              NEXT c
              positions<-1> = lnpos + pos
              IF NOT(all) THEN BREAK
          END
          lnpos += 1+LEN(line)
      NEXT a
  END
  RETURN(positions)
