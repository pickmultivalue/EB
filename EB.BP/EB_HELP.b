    SUBROUTINE EB_HELP(WORD,OS.HELP)
    INCLUDE EB.EQUS EB.COMMONS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS ACT.CODES
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(100),EB.FILE.LIST
    COM RDSP(100),CHANGES(100)
    GO MAIN$
    EQU TRUE TO 1, FALSE TO 0, ESC TO CHAR(27)
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE JBC.h
    DEFFUN FNKEYTRANS()
MAIN$:!
!
    accuterm = @FALSE
    IF NOT(GETENV('EBACCUTERM',accuterm)) THEN accuterm = @TRUE
    IF accuterm THEN CRT ESC:CHAR(2):0:
    ksh = @IM:'k'
    INCLUDE EB.OS.INCLUDES OS.REL
    INDENT = SPACE(4)
    BEGIN CASE
        CASE WORD='EBOPTS'
            dc = COUNT(FG_SENTENCE, ' ') - 1
            CRT @(-1):'Options Help'
            CRT
!        IF WCNT GT dc THEN
!            CRT 'You are at ':WCNT:' of ':dc:' items'
!            CRT
!        END
            CRT INDENT:'. - prompt for members of a variable structure (non-basic code)'
            CRT INDENT:'A - insert date/time stamp'
            CRT INDENT:'B - Show errors from last compile'
            CRT INDENT:'C - Compare current source with another program'
            CRT INDENT:'D - Duplicate line above'
            CRT INDENT:'E - EDit record (using ED)'
            CRT INDENT:'F - Format (indent)'
            CRT INDENT:'G - Toggle whether TABs are used or SPACEs'
            CRT INDENT:'H - Hex mode toggle'
            CRT INDENT:'I - (Perforce Integration specific)'
            CRT INDENT:'K - Insert key of current record'
            CRT INDENT:'M - Merge'
            CRT INDENT:'N - Not Used'
            CRT INDENT:'O - Move to file (warning drops you out)'
            CRT INDENT:'P - Print'
            CRT INDENT:'R - Reset record to initial edit state'
            CRT INDENT:'S - Swap/Convert current line'
            CRT INDENT:'T - Rotate AM/VM layout'
            CRT INDENT:'U - Unindent'
            CRT INDENT:'V - Edit Values'
            CRT INDENT:'W - Save and keep editing'
            CRT INDENT:'    (e.g. LOCATE->INS; READ<->WRITE; FOR...->FOR...STEP-1;...and many more)'
            CRT INDENT:'X - Toggle 80/132 view'
            CRT INDENT:'Z - Record size'
            CRT
            CRT
            CRT INDENT:'Press any key...':
            CALL EB_GET_INPUT(CHR, CHR.NBR)
            IF FG_ACT.CODE = FG_HLP.CODE THEN GOSUB DisplayEBcmds
            FG_ACT.CODE=FALSE
            OS.HELP=TRUE
        CASE WORD='EBREPLACE'
            CRT @(-1):'Replace Help'
            CRT
            CRT INDENT:'General syntax:'
            CRT INDENT:'R{opts}/<old>/<new>'
            CRT INDENT:'S{opts}/<new>/<old>'
            CRT
            CRT INDENT:'The / delimiter can be any non alphanumeric character'
            CRT
            CRT INDENT:'opts:'
            CRT
            CRT INDENT:'U or A - all occurrences'
            CRT INDENT:'V      - replace matching variable names only'
            CRT INDENT:'C      - confirm each replacement'
            CRT
            CRT INDENT:'E      - end of record'
            CRT INDENT:'         (this is typically used in a range e.g. R5-E/old/new replaces from line 5 to the end)'
            CRT
            CRT INDENT:'A single number "n" means replace n lines from the current line'
            CRT INDENT:'Alternatively you can enter line number ranges (e.g. R10-20/old/new)'
            CRT
            CRT INDENT:'A complex example:'
            CRT
            CRT INDENT:'R1-EVU/I/ILOOP wil replace all occurrences of I with ILOOP only if I is a variable'
            CRT
            CRT INDENT:'Wildcards'
            CRT
            CRT INDENT:'^nnn can be used to represent CHAR(nnn) (must be 3 numerics)'
            CRT
            CRT INDENT:'You can use @n (i.e. @1, @2, etc) to mask any number of characters in both the search and replace'
            CRT
            CRT INDENT:'e.g. R/READ @1 FROM @2,@3 @4/CALL READSUB(@2, @3, @1)'
            CRT
            CRT INDENT:'A special @ replace is  which will replace a with a sequential number'
            CRT INDENT:'e.g. R99/@1/EQU A.@1 TO A.ARRAY(1) is an easy way to generate equates'
            CRT
            CRT INDENT:'The R is optional for readability but is useful for reversing the last replace thus:'
            CRT
            CRT INDENT:'- as each search/replace is held in a history file, by default the next time'
            CRT INDENT:'  you do a search/replace it defaults to the one just executed.'
            CRT INDENT:'  If you press <backspace> at the first character position this will prevent'
            CRT INDENT:'  the input routine from clearing the entry (as is normal when entering a value)'
            CRT INDENT:'  You can then overtype the R with an S (think of it as (S)witch) which then'
            CRT INDENT:'  treats the 2 strings in reverse'
            CRT
            CRT
            CRT
            CRT INDENT:'Press any key...':
            CALL EB_GET_INPUT(CHR, CHR.NBR)
            IF FG_ACT.CODE = FG_HLP.CODE THEN GOSUB DisplayEBcmds
            FG_ACT.CODE=FALSE
            OS.HELP=TRUE
        CASE WORD='EBCUT'
            CRT @(-1):'Cut/Paste Help'
            CRT
            CRT INDENT:'Enter a number for quick reusable paste items'
            CRT INDENT:'Enter a name for more permanent paste items'
            CRT INDENT:'Special commands:'
            CRT
            CRT INDENT:'! - comment out block'
            CRT INDENT:'> - indent block'
            CRT INDENT:'< - unindent block'
            CRT INDENT:'^ - rotate block'
            CRT
            CRT INDENT:'Press any key...':
            CALL EB_GET_INPUT(CHR, CHR.NBR)
            IF FG_ACT.CODE = FG_HLP.CODE THEN GOSUB DisplayEBcmds
            FG_ACT.CODE=FALSE
            OS.HELP=TRUE
        CASE 1
            OS.HELP=FALSE
            WORD = TRIM(WORD)
            EXECUTE ksh:'man ':WORD:' 2>&1' CAPTURING list
            IF LEN(list) EQ 0 THEN
                EXECUTE ksh:'man -k ':WORD:' 2>&1' CAPTURING list
            END
            loc=0
            manpages=''
            FWORD=UPCASE(WORD)    ;!:'()'
            LOOP
                REMOVE line FROM list AT loc SETTING delim
                line=SWAP(line,', ',@VM)
                FINDSTR FWORD IN line<1,vm_start> SETTING POS ELSE
                    FINDSTR WORD IN line<1,vm_start> SETTING POS ELSE POS = @FALSE
                END
                IF POS THEN
                    vol=OCONV(FIELD(line,'(',2),'MCN')
                    IF vol#'' THEN
                        POS=INDEX(line,vol,1)
                        vol=FIELD(line[POS,9],')',1)
                        LOCATE vol IN manpages<am_start> BY 'AR' SETTING pos ELSE
                            INS vol BEFORE manpages<pos>
                        END
                    END
                    CRT @(-1):
                    EXECUTE ksh:'man ':vol:' ':WORD:' 2>&1'
                    OS.HELP=TRUE
                    delim = @FALSE
                END
            WHILE delim DO REPEAT
            IF NOT(OS.HELP) THEN
                CRT @(-1):
                IF DIR_DELIM_CH = '/' THEN
                    mandir = '-M $JBCRELEASEDIR/man '
                END ELSE mandir = ''
                EXECUTE ksh:'man ':mandir:WORD:' 2>&1' CAPTURING list
                notfound = INDEX(list, 'o manual entry', 1) OR INDEX(list, 'hat manual page', 1)
                IF notfound THEN
                    CRT list
                    CRT
                END ELSE
                    EXECUTE ksh:'man ':mandir:WORD:' 2>&1' CAPTURING help
                    K.HELP = '%EB_HELP*':WORD:'%'
                    WRITE help ON JET.PASTE,K.HELP
                    EXECUTE 'EB JET.PASTE ':K.HELP
                    DELETE JET.PASTE,K.HELP
                END
                CRT
                CRT 'Press return or F1 for EB help':
                CALL EB_GET_INPUT(CHR, CHR.NBR)
                IF FG_ACT.CODE = FG_HLP.CODE THEN GOSUB DisplayEBcmds
                FG_ACT.CODE=FALSE
                OS.HELP=TRUE
            END
    END CASE
!    BEGIN CASE
!    CASE OS.REL='UDT'
!        EXECUTE 'HELP UNIBASIC ':WORD
!        OS.HELP=TRUE
!    CASE OS.REL='JB'
!        EXECUTE ksh:'%JBCGLOBALDIR%\man\manhtml\jbc2_':WORD:'.html'
!    CASE 1; OS.HELP=FALSE
!    END CASE
    IF accuterm THEN CRT ESC:CHAR(2):1:
    RETURN
DisplayEBcmds:
    CRT @(-1):'EB commands...'
    CRT
    hash = 'L#30 ':
    keyboard = ''
    keyboard<-1> = 'Save/accept' hash:FNKEYTRANS(EB_CHARS(2))
    keyboard<-1> = 'Exit/cancel' hash:FNKEYTRANS(EB_CHARS(3))
    keyboard<-1> = 'Refresh' hash:FNKEYTRANS(EB_CHARS(6))
    keyboard<-1> = 'Search' hash:FNKEYTRANS(EB_CHARS(5))
    keyboard<-1> = 'Shell' hash:FNKEYTRANS(EB_CHARS(8))
    keyboard<-1> = 'Help' hash:FNKEYTRANS(EB_CHARS(11))
    keyboard<-1> = 'Next record' hash:FNKEYTRANS(EB_CHARS(56))
    keyboard<-1> = 'Options' hash:FNKEYTRANS(EB_CHARS(54))
    keyboard<-1> = 'Prev record' hash:FNKEYTRANS(EB_CHARS(69))
    keyboard<-1> = 'Reverse search' hash:FNKEYTRANS(EB_CHARS(99))
    general = SORT(keyboard)
    keyboard = ''
    keyboard<-1> = 'Bottom of screen/record' hash:FNKEYTRANS(EB_CHARS(75))
    keyboard<-1> = 'Zoom' hash:FNKEYTRANS(EB_CHARS(55))
    keyboard<-1> = 'Previous field' hash:FNKEYTRANS(EB_CHARS(4))
    keyboard<-1> = 'Page Up' hash:FNKEYTRANS(EB_CHARS(51))
    keyboard<-1> = 'Page Down' hash:FNKEYTRANS(EB_CHARS(52))
    keyboard<-1> = 'Bookmark' hash:FNKEYTRANS(EB_CHARS(7))
!    keyboard<-1> = 'Next field' hash:FNKEYTRANS(EB_CHARS(9))
    keyboard<-1> = 'Prev word' hash:FNKEYTRANS(EB_CHARS(28))
    keyboard<-1> = 'End of line' hash:FNKEYTRANS(EB_CHARS(29))
    keyboard<-1> = 'Next word' hash:FNKEYTRANS(EB_CHARS(30))
    keyboard<-1> = 'Leftarrow' hash:FNKEYTRANS(EB_CHARS(31))
    keyboard<-1> = 'Downarrow' hash:FNKEYTRANS(EB_CHARS(33))
    keyboard<-1> = 'Uparrow' hash:FNKEYTRANS(EB_CHARS(34))
    keyboard<-1> = 'Rightarrow' hash:FNKEYTRANS(EB_CHARS(35))
    keyboard<-1> = 'Next occurrence' hash:FNKEYTRANS(EB_CHARS(36))
    keyboard<-1> = 'Top of screen/record' hash:FNKEYTRANS(EB_CHARS(38))
    keyboard<-1> = 'Start of line' hash:FNKEYTRANS(EB_CHARS(41))
    keyboard<-1> = 'Back tab' hash:FNKEYTRANS(EB_CHARS(45))
    keyboard<-1> = 'Goto line/label' hash:FNKEYTRANS(EB_CHARS(70))
    navigation = SORT(keyboard)
    keyboard = ''
    keyboard<-1> = 'Start/end block' hash:FNKEYTRANS(EB_CHARS(10))
    keyboard<-1> = 'Lower case' hash:FNKEYTRANS(EB_CHARS(26))
    keyboard<-1> = 'Toggle case' hash:FNKEYTRANS(EB_CHARS(27))
    keyboard<-1> = 'Tab' hash:FNKEYTRANS(EB_CHARS(32))
    keyboard<-1> = 'Paste' hash:FNKEYTRANS(EB_CHARS(37))
    keyboard<-1> = 'Toggle INS/OVR' hash:FNKEYTRANS(EB_CHARS(39))
!    keyboard<-1> = 'SPELL.CH' hash:FNKEYTRANS(EB_CHARS(40))
    keyboard<-1> = 'Truncate/cut' hash:FNKEYTRANS(EB_CHARS(42))
    keyboard<-1> = 'Undo' hash:FNKEYTRANS(EB_CHARS(43))
    keyboard<-1> = 'Redo' hash:FNKEYTRANS(EB_CHARS(12))
    keyboard<-1> = 'Insert line' hash:FNKEYTRANS(EB_CHARS(44))
    keyboard<-1> = 'Insert space' hash:FNKEYTRANS(EB_CHARS(46))
    keyboard<-1> = 'Delete line' hash:FNKEYTRANS(EB_CHARS(47))
    keyboard<-1> = 'Delete char' hash:FNKEYTRANS(EB_CHARS(48))
    keyboard<-1> = 'Delete word' hash:FNKEYTRANS(EB_CHARS(49))
    keyboard<-1> = 'Comment/select toggle' hash:FNKEYTRANS(EB_CHARS(58))
!    keyboard<-1> = 'SUS.CH' hash:FNKEYTRANS(EB_CHARS(50))
!    keyboard<-1> = 'MERGE.CH' hash:FNKEYTRANS(EB_CHARS(53))
!    keyboard<-1> = 'EXIT.LN' hash:FNKEYTRANS(EB_CHARS(59))
!    keyboard<-1> = 'TAG.CMD' hash:FNKEYTRANS(EB_CHARS(60))
!    keyboard<-1> = 'HOT.KEYS' hash:FNKEYTRANS(EB_CHARS(61))
!    keyboard<-1> = 'QUICK.CH' hash:FNKEYTRANS(EB_CHARS(62))
!    keyboard<-1> = 'TUT.CH' hash:FNKEYTRANS(EB_CHARS(63))
!    keyboard<-1> = 'Mouse click' hash:FNKEYTRANS(EB_CHARS(64))
!    keyboard<-1> = 'EXPECT.CR' hash:FNKEYTRANS(EB_CHARS(65))
    keyboard<-1> = 'Backspace' hash:FNKEYTRANS(EB_CHARS(66))
!    keyboard<-1> = 'ALT.CH' hash:FNKEYTRANS(EB_CHARS(67))
!    keyboard<-1> = 'CLR.CMD' hash:FNKEYTRANS(EB_CHARS(68))
!    keyboard<-1> = 'MENU.CH' hash:FNKEYTRANS(EB_CHARS(71))
!    keyboard<-1> = 'ADD.CH' hash:FNKEYTRANS(EB_CHARS(72))
!    keyboard<-1> = 'APP.CH' hash:FNKEYTRANS(EB_CHARS(73))
!    keyboard<-1> = 'AMD.CH' hash:FNKEYTRANS(EB_CHARS(74))
!    keyboard<-1> = 'FUNC.CHARS' hash:FNKEYTRANS(EB_CHARS(97))
!    keyboard<-1> = 'FUNC.VALS' hash:FNKEYTRANS(EB_CHARS(98))
!    keyboard<-1> = 'MNENOMICS' hash:FNKEYTRANS(EB_CHARS(100))
    editing = SORT(keyboard)
    uline = STR('=',50)
    keyboard = ''
    keyboard<-1> = uline:@AM:'General':@AM:uline:@AM:general
    keyboard<-1> = uline:@AM:'Navigation':@AM:uline:@AM:navigation
    keyboard<-1> = uline:@AM:'Editing':@AM:uline:@AM:editing
    nbr_keys = DCOUNT(keyboard, @AM)
    FOR i = 1 TO nbr_keys
        CRT INDENT:keyboard<i>
        IF i = nbr_keys OR (MOD(i, PDEPTH-3) = 0) THEN
            CRT
            CALL EB_GET_INPUT(CHR, CHR.NBR)
            IF FG_ACT.CODE = FG_ABT.CODE THEN RETURN
        END
    NEXT i
    RETURN
