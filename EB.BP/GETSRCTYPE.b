  FUNCTION GETSRCTYPE
  ksh = @IM:'k'
  cmd = ' status 2>&1'
  result = 'UNKNOWN'
  supported = 'git':@AM:'svn'
  reject = 'ot a git repository':@AM:'is not a working'
  dc = DCOUNT(supported, @AM)
  FOR d = 1 TO dc
      type = supported<d>
      EXECUTE ksh:type:cmd CAPTURING io
      io = DOWNCASE(io)
      IF NOT(INDEX(io,reject<d>,1)) THEN
          result = UPCASE(type)
          BREAK
      END
  NEXT d
!
  RETURN result
