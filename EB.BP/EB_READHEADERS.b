    SUBROUTINE EB_READHEADERS(TREC, HEADERS)
* @(#) EB_READHEADERS.b Ported to jBASE 07:23:52  18 FEB 2010
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(100),EB.FILE.LIST
    COM RDSP(100),CHANGES(100)
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS ACT.CODES
    MAIN$:!
!
    LOC=1
    LOOP
        idx=INDEX(TREC[LOC,LEN(TREC)],'#include',1)
        IF idx THEN
            LOOP UNTIL idx=LOC OR TREC[LOC+idx-1,1]=@AM DO idx-- REPEAT
            LINE=TREC[LOC+idx-1,100]
            LINE=LINE<1>
            idx+=LEN(LINE)
            CONVERT TAB TO SPC IN LINE
            LINE=TRIM(LINE)
            IF FIELD(LINE,SPC,1)='#include' THEN
                CALL EB_READINCL(HEADERS, LINE, HeaderId, HeaderCode, TRUE)
                IF HeaderId#'' THEN
                    CALL EB_READHEADERS(HeaderCode, HEADERS)
                    CONVERT CR:LF:TAB TO AM:AM:SPC IN HeaderCode
                    HEADERCODE<-1>=HeaderCode
                END
            END
        END
    WHILE idx DO
        LOC += idx
    REPEAT
    RETURN
END
