    SUBROUTINE EB_TABS(CRTLN,PWIDTH)
* @(#) EB_TABS.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.TABS Ported to jBASE 16:15:16  27 JUL 2001
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    INCLUDE EB.EQUS EB.EQUS
!
    EQU TABCH TO CHAR(9), SPC TO ' ', TRUE TO 1, FALSE TO 0
    Indent=ITAB<ITABPOS>
    TABSPC=SPACE(Indent)
    Nbr.Tabs=DCOUNT(CRTLN,TABCH)
    IF CRTLN 'R#1'=TABCH THEN
        CRTLN:='.'
        ADD.DOT=TRUE
    END ELSE ADD.DOT=FALSE
!    IF Nbr.Tabs=1 THEN RETURN
    NEWLN=''
    FOR T=1 TO Nbr.Tabs
        TabField=FIELD(CRTLN,TABCH,T)
        TabLen=((LEN(TabField)/Indent+.5) "0")*Indent
        Hash='L#':TabLen
        NEWLN:=TabField Hash
    NEXT T
    CRTLN=TRIM(NEWLN,' ','T')
    IF ADD.DOT THEN CRTLN=CRTLN[1,LEN(CRTLN)-1]
    CRTLN=CRTLN[1,PWIDTH-5]
    RETURN
