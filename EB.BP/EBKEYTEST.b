! PROGRAM
!
    DEFFUN FNKEYTRANS()
    PROMPT ''
    match = ''
    CALL EBGETKEY(match, chars)
    CRT FNKEYTRANS(chars)
