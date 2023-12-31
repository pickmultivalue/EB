    FUNCTION GIT_HISTORY(FilePath, ItemName, Filters)
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
    DEFFUN GIT_EXEC()
    DEFFUN GIT_REPOSITORY()
    DEFFUN GIT_GET_REPOSITORY()
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
    shellend = ' 2>&1'
    EQU TRUE TO 1, FALSE TO 0
    EQU MAX TO 999999
!
! FileName may be the local one...it only makes sense to use the original
!
    INCLUDE EB.INCLUDES SRCDBG
    FileName = GETFLNM(FilePath)
    GOSUB Parse_Filters
!
    Repository = GIT_REPOSITORY(FilePath)
!    Repository = GIT_GET_REPOSITORY(FileName)
!    Repository := '/':FileName
!    repo = Repository
    repo = FilePath
!
    Stash = ItemName<2> EQ 'S'
    ItemName = ItemName<1>
    matchKey = CHANGE(repo:DIR_DELIM_CH:ItemName,DIR_DELIM_CH:'.':DIR_DELIM_CH, DIR_DELIM_CH)
    IF Stash THEN
        IO = GIT_EXEC('reflog show --format="medium" stash', TRUE)
        stashstr = 'stash@{'
        IF 1 THEN
        matchKey = CHANGE(matchKey, Repository:DIR_DELIM_CH, '')
        occ = 1
        dc = DCOUNT(IO, @AM)
        LOOP
            FINDSTR stashstr IN IO,occ SETTING lnbr ELSE BREAK
            stashid = FIELD(IO<lnbr>,' ',2)
            stash = GIT_EXEC('show --name-only --oneline ':stashid, TRUE)
            IF INDEX(stash, matchKey, 1) THEN
                occ++
            END ELSE
                --lnbr
                LOOP
                    DEL IO<lnbr>
                    --dc
                    stash = FIELD(IO<lnbr>, ' ', 1)
                UNTIL lnbr GT dc OR stash EQ 'commit' DO REPEAT
            END
        REPEAT
        END
    END ELSE
        IO = GIT_EXEC('log ':matchKey, TRUE)
    END
!
    history = ''
    loc = 0
    n = 1
    REMOVE line FROM IO AT loc SETTING delim
    IO := @AM:'commit'
    IF FIELD(line, ' ', 1) = 'commit' THEN
        LOOP
            rev = line[COL2()+1, 99]
            ll = 1
            LOOP
                REMOVE user FROM IO AT loc SETTING delim
                IF Stash AND INDEX(user, stashstr, 1) THEN
                    rev = FIELD(user, ' ', 2)
                END
            UNTIL FIELD(user, ':', 1) EQ 'Author' OR ll GT 4 DO ll++ REPEAT
            user = TRIM(FIELD(user, ':', 2))
            REMOVE timestamp FROM IO AT loc SETTING delim
            timestamp = TRIM(timestamp)
            time = ICONV(FIELD(timestamp, ' ', 5), 'MTS')
            odate = FIELD(timestamp, ' ', 3, 2):' ':FIELD(timestamp, ' ', 6)
            date = ICONV(odate, 'D')
            desc = ''
            line_id = ''
            LOOP
                REMOVE line FROM IO AT loc SETTING delim
            UNTIL FIELD(line, ' ', 1) = 'commit' DO
                IF LEN(line) THEN
                    desc<1, 1, -1> = TRIM(line)
                END
            REPEAT
            IF LEN(Filters) THEN
                GOSUB Match_Filter
            END ELSE OK = TRUE
            IF OK THEN
                history<1, n> = rev
                history<2, n> = user
                history<3, n> = date
                history<4, n> = time
                history<5, n> = CHANGE(desc, @SVM, ' ')
            END
        WHILE line NE 'commit' DO
            n++
        REPEAT
    END ELSE
        sep = line
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
                        line_id = CHANGE(line, '\', '/')
                        line_id = line_id[1, LEN(Repository)]
                        IF line_id NE Repository THEN line_id = ''
                    END
                    desc<1, 1, -1> = TRIM(line)
                END
            NEXT L
!
! If line_id is set then we need to
! extract the description for ItemName
!
            IF LEN(line_id) THEN
                desc_id = CHANGE(Repository:'/':ItemName, '/', DIR_DELIM_CH)
                LOCATE desc_id IN desc<1, 1> SETTING loc THEN
                    linecnd = DCOUNT(desc<1, 1>, @SVM)
                    loc++
                    ndesc = ''
                    FOR L = loc TO linecnt
                        line = desc<1, 1, L>
                        line_id = CHANGE(line, '\', '/')
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
                history<5, n> = CHANGE(desc, @SVM, ' ')
                n++
            END
            REMOVE line FROM IO AT loc SETTING delim
        REPEAT
    END
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
                L_User = LEN(CHANGE(F_User, '*', ''))
                BEGIN CASE
                    CASE F_User[1,1] = '*' AND F_User[LEN(F_User), 1] = '*'
                        M_User = "0X'": F_User[2, LEN(F_User)-1]:"'0X"
                    CASE F_User[-1, 1] = '*'
                        M_User = "'": F_User[1, LEN(F_User)-1]:"'0X"
                    CASE F_User[1, 1] = '*'
                        M_User = "0X'":F_User[2, MAX]:"'"
                    CASE 1
                        M_User = "'":F_User:"'"
                END CASE
        END CASE
    NEXT F
    RETURN
