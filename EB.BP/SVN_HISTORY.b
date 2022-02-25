    FUNCTION SVN_HISTORY(FilePath, ItemName, Filters)
!
! Function to get the history/revsion of changes of a program from subversion
!
! Filters is an optional dynamic array
!
! attr 1 multi-valued strings containing any of the following:
!
! - Date       {used for exact match}
! - StartDate  {used for range}
! - EndDate
! - Time       {used for exact match}
! - StartTime  {used for range}
! - EndTime
! - User       {login name (* can be used at start/end)}
! - Word       {sub-valued list of words to search (case insensitive)}
!
! attr 2 multi-valued corresponding value
!
! 01 DEC 2009 Peter Falson (jBASE)
!
    INCLUDE JBC.h
    DEFFUN GETFLNM()
    DEFFUN SVN_EXEC()
    DEFFUN SVN_SUBVERSION()
    DEFFUN SVN_GET_REPOSITORY()
!
    DIM A_Filters(8)
    EQU F_Date       TO A_Filters(1)
    EQU F_StartDate  TO A_Filters(2)
    EQU F_EndDate    TO A_Filters(3)
    EQU F_Time       TO A_Filters(4)
    EQU F_StartTime  TO A_Filters(5)
    EQU F_EndTime    TO A_Filters(6)
    EQU F_User       TO A_Filters(7)
    EQU F_Word       TO A_Filters(8)
!
    shell = CHAR(255):'k'
    shellend = ' 2>&1'
    EQU TRUE TO 1, FALSE TO 0
!
! FileName may be the local one...it only makes sense to use the original
!
    INCLUDE EB.INCLUDES SRCDBG
    FileName = GETFLNM(FilePath)
    GOSUB Parse_Filters
!
    subversion = SVN_SUBVERSION(FilePath)
    Repository = SVN_GET_REPOSITORY(FileName)
    Repository := '/':FileName
    repo = subversion:'/':Repository
!
    IO = SVN_EXEC('log ':repo:'/':ItemName, TRUE)
!
    history = ''
    loc = 0
    REMOVE line FROM IO AT loc SETTING delim
    sep = line
    n = 1
    LOOP WHILE delim DO
        REMOVE line FROM IO AT loc SETTING delim
        line = CHANGE(line, ' | ', @VM)
        rev = line<1, 1>
        user = line<1, 2>
        timestamp = line<1, 3>
        odate = FIELD(timestamp, ' ', 1)
        date = ICONV(odate, 'D')
        IF LEN(date) = 0 THEN ;! format may need massaging
            year = FIELD(odate, '-', 1)
            month = FIELD(odate, '-', 2)
            day = FIELD(odate, '-', 3)
            date = ICONV(day:'/':month:'/':year, 'D4/E')
            END
            time = ICONV(FIELD(timestamp, ' ', 2), 'MTS')
            linecnt = OCONV(line<1, 4>, 'MCN')
            REMOVE line FROM IO AT loc SETTING delim
            desc = ''
            line_id = ''
            FOR L = 1 TO linecnt
                REMOVE line FROM IO AT loc SETTING delim
                IF LEN(line) THEN
                IF LEN(line_id) = 0 AND LEN(desc) = 0 THEN  ;! check if description is for multiple items
                    line_id = CONVERT(line, '\', '/')
                    line_id = line_id[1, LEN(Repository)]
                    IF line_id NE Repository THEN line_id = ''
                    END
                    desc<1, 1, -1> = line
                    END
                NEXT L
!
! If line_id is set then we need to
! extract the description for ItemName
!
                IF LEN(line_id) THEN
                desc_id = CONVERT(Repository:'/':ItemName, '/', DIR_DELIM_CH)
                LOCATE desc_id IN desc<1, 1> SETTING loc THEN
                linecnd = DCOUNT(desc<1, 1>, @SVM)
                loc++
                ndesc = ''
                FOR L = loc TO linecnt
                    line = desc<1, 1, L>
                    line_id = CONVERT(line, '\', '/')
                    line_id = line_id[1, LEN(Repository)]
                    IF line_id = Repository THEN BREAK
                    ndesc<1, 1, -1> = line
                NEXT L
                desc = ndesc
                END
                END
                IF LEN(Filters) THEN
                GOSUB Match_Filter
            END ELSE OK = TRUE
            IF OK THEN
            history<1, n> = OCONV(rev,'MCN')
            history<2, n> = user
            history<3, n> = date
            history<4, n> = time
            history<5, n> = CONVERT(desc, @SVM, ' ')
            n++
            END
            REMOVE line FROM IO AT loc SETTING delim
            REPEAT
!
            RETURN history
!
            Match_Filter:
!
            OK = TRUE
            IF F_Date THEN
            IF date NE F_Date THEN OK=FALSE; RETURN
            END
            IF F_StartDate THEN
            IF date < F_StartDate THEN OK=FALSE; RETURN
            END
            IF F_EndDate THEN
            IF date > F_EndDate THEN OK=FALSE; RETURN
            END
            IF F_Time THEN
            IF time NE F_Time THEN OK=FALSE; RETURN
            END
            IF F_StartTime THEN
            IF time < F_StartTime THEN OK=FALSE; RETURN
            END
            IF F_EndTime THEN
            IF time > F_EndTime THEN OK=FALSE; RETURN
            END
            IF F_User THEN
            IF NOT(F_User MATCHES M_User) THEN OK=FALSE; RETURN
            END
            IF F_Word THEN
            OK = FALSE
            FOR W = 1 TO NbrWords UNTIL OK
                OK = INDEX(desc, F_Word<1, 1, W>, 1)
            NEXT W
            END
            RETURN
!
            Parse_Filters:
!
            MAT A_Filters = ''
            Nbr_Filters = DCOUNT(Filters<1>, @VM)
            FOR F = 1 TO Nbr_Filters
                Filter = Filters<1, F>
                Value = Filters<2, F>
                BEGIN CASE
            CASE Filter = 'Date'      ; F_Date      = Value
            CASE Filter = 'StartDate' ; F_StartDate = Value
            CASE Filter = 'EndDate'   ; F_EndDate   = Value
            CASE Filter = 'Time'      ; F_Time      = Value
            CASE Filter = 'StartTime' ; F_StartTime = Value
            CASE Filter = 'EndTime'   ; F_EndTime   = Value
            CASE Filter = 'Word'
                F_Word = UPCASE(Value)
                NbrWords = DCOUNT(F_Word, @SVM)
            CASE Filter = 'User'
                F_User = Value
                L_User = LEN(CONVERT(F_User, '*', ''))
                BEGIN CASE
            CASE F_User[1,1] = '*' AND F_User[-1, 1] = '*'
                M_User = "0X'":F_User[2, -2]:"'0X"
            CASE F_User[-1, 1] = '*'
                M_User = "'":F_User[1, -2]:"'0X"
            CASE F_User[1, 1] = '*'
                M_User = "0X'":F_User[2, -1]:"'"
            CASE 1
                M_User = "'":F_User:"'"
            END CASE
        END CASE
    NEXT F
    RETURN
