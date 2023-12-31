    FUNCTION GETYN(Message, Dflt, Opt)
!
! Get a Y or N response and return the answer
!
! Opt determines display
! 0 - no display
! 1 - display no new line
! 2 - display with new line
!
    PROMPT ''
    ANS = Dflt      ;! initialise incase of timeout
    CRT Message:
    BEGIN CASE
        CASE Dflt = 'Y'; YN = '[Y]/N'
        CASE Dflt = 'N'; YN = 'Y/[N]'
        CASE 1         ; YN = 'Y/N'
    END CASE
    CRT ' (':YN:') ? ':ANS:STR(CHAR(8), LEN(ANS)):
    ECHO OFF
    LOOP
        INPUT ANS,1:
        ANS = UPCASE(ANS)
        IF LEN(ANS) = 0 THEN ANS = Dflt
    UNTIL ANS MATCHES "'Y'":@VM:"'N'" DO
        CRT CHAR(7):
    REPEAT
    ECHO ON
    IF Opt THEN
        CRT ANS:
        IF Opt = 2 THEN CRT
    END
    RETURN ANS
