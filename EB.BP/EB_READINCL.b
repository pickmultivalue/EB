    SUBROUTINE EB_READINCL(HEADERS, CodeLine, IncludeString, Header, Recursive)
* @(#) EB_READINCL.b Ported to jBASE 07:23:52  18 FEB 2010
    INCLUDE EB.EQUS EB.COMMONS
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(100),EB.FILE.LIST
    COM RDSP(100),CHANGES(100)
    GO MAIN$
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS ACT.CODES
    EQU MAX TO 999
    MAIN$:!
#ifdef WIN32
    dir_sep=';'
    fromslash='/'; toslash='\'
#else
    dir_sep=':'
    fromslash='\'; toslash='/'
#endif
    CodeLine=TRIM(CodeLine)
    IF FIELD(CodeLine,SPC,1)='INCLUDE' THEN
        incid=FIELD(CodeLine,SPC,3)
        Header=''
        IF LEN(incid) THEN
            IncludeString=FIELD(CodeLine,SPC,2,2)
            CALL EB_OPEN('',FIELD(IncludeString,SPC,1),F.temp,1,ok)
            IF ok THEN
                READ Header FROM F.temp, incid ELSE NULL
            END
            RETURN
        END
    END
    incid=FIELD(CodeLine,SPC,2)
    CONVERT '<>' TO '' IN incid
    CONVERT fromslash TO toslash IN incid
    PathFlag=COUNT(incid,toslash)
    IF PathFlag THEN
        IF incid[1,1]='"' THEN
            incid=FIELD(incid,'"',2)
        END
        inclib=FIELD(incid,toslash,1,PathFlag)
!        inclib=FLNM:toslash:inclib
        incid=incid[COL2()+1,MAX]
    END ELSE
        CodeLine=''
        IF NOT(GETENV('JBCRELEASEDIR',jbcdir)) THEN jbcdir='/usr/jbc'
        IF NOT(GETENV('INCLUDE',inclib)) THEN inclib='.:':jbcdir:'/include'
        CONVERT dir_sep TO @AM IN inclib
        inclib<-1>=FLNM
    END
    IncludeString=''
    LOC=0
    IF incid[1,1]='"' THEN
        INS "." BEFORE inclib<1>
        incid=FIELD(incid,'"',2)
    END
    Header=''
    LOCATE incid IN HEADERS<1,vm_start> BY 'AL' SETTING POS THEN
        IF NOT(Recursive) THEN
            incdir=HEADERS<2,POS>
            Header=HEADERS<3,POS>
        END
    END ELSE
        LOOP
            REMOVE incdir FROM inclib AT LOC SETTING DELIM
            CALL EB_OPEN('',incdir,F.temp,1,ok)
            IF ok THEN
                READ Header FROM F.temp,incid THEN
                    CONVERT SPC TO BS IN incdir
                    CONVERT @AM TO @SVM IN Header
                    INS incid BEFORE HEADERS<1,POS>
                    INS incdir BEFORE HEADERS<2,POS>
                    INS Header BEFORE HEADERS<3,POS>
                    DELIM=0
                END
            END
        WHILE DELIM DO REPEAT
    END
    IF LEN(Header) THEN
        IncludeString = incdir:' ':incid:' (c'
        CONVERT @SVM TO @AM IN Header
    END
    RETURN