    FUNCTION GETFULLPATH(INFLNM)
    INCLUDE EB.EQUS EB.COMMONS
!
! Return full path for a given file
!
    DEFFUN GETFLNM()
    DEFFUN EBJSHOW()
    INCLUDE JBC.h
    CALL EB_OPEN('',INFLNM, DSCB,0,POS)
    IF POS THEN
        IF IOCTL(DSCB,JBC_COMMAND_GETFILENAME,FullPath) THEN
            RETURN(FullPath)
        END
    END
    shell = CHAR(255):'k'
    IF NOT(GETENV('pwd', pwd)) THEN
        IF NOT(GETENV('PWD', pwd)) THEN
            pwd = '.'         ;! hopefully this won't be the case
        END
    END
    FLNM = CHANGE(INFLNM, '/', DIR_DELIM_CH)
    dc = DCOUNT(FLNM, DIR_DELIM_CH)
    IF dc GT 1 THEN
        ITNM = FIELD(FLNM, DIR_DELIM_CH, dc)
        FLNM = FLNM[1, COL1()-1]
    END ELSE ITNM = ''
!
    FullPath = EBJSHOW('-f ':FLNM)
    CALL EB_OPEN('',FLNM, DSCB,0,POS)
    IF POS THEN
        rc = IOCTL(DSCB, JBC_COMMAND_GETFILENAME, FullPath)
    END
!
    IF FullPath='' THEN       ;! assume directory at current position
        temp = pwd:DIR_DELIM_CH:FLNM
        FullPath = EBJSHOW('-f ':temp)
        IF NOT(LEN(FullPath)) AND GETFLNM(pwd) = FLNM THEN
            temp = pwd
        END ELSE temp = FIELD(TRIM(FullPath), ' ', 2)
        FullPath=temp         ;! create a path with a dummy arg for FIELD below
    END ELSE
        FINDSTR 'File:' IN FullPath SETTING attr THEN
            FullPath = FullPath<attr>
        END ELSE
            FullPath = CHANGE(TRIM(FullPath), '---', @AM)
            IF DCOUNT(FullPath, @AM) GT 1 THEN
                FullPath = TRIM(FullPath<3>)
            END ELSE FullPath = FullPath<1>
        END
    END
!
    FullPath=OCONV(FullPath, 'MCP ')    ;! remove any dodgy characters
    FullPath=TRIM(FullPath)
!
! Get everything after the first word (i.e. result of jshow -f)
!
!    FullPath=FIELD(FullPath,' ',2,999)
    FullPath = FIELD(FullPath, '(', 1)
!
! If the path starts with ".", replace with pwd
!
    IF FIELD(FullPath, DIR_DELIM_CH, 1) = '.' THEN
        FullPath = pwd:FullPath[COL2(),999]
    END
    IF LEN(ITNM) THEN
        FullPath := DIR_DELIM_CH:ITNM
    END
!
    RETURN CHANGE(FullPath, DIR_DELIM_CH:'.':DIR_DELIM_CH, DIR_DELIM_CH)
