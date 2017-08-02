    SUBROUTINE EB_HELP(WORD,OS.HELP)
* @(#) EB_HELP.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.HELP Ported to jBASE 16:15:14  27 JUL 2001
    INCLUDE EB.EQUS EB.COMMONS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS ACT.CODES
    COM GEX(50),EXTRAS(50)
    COM EB.FILES(100),EB.FILE.LIST
    COM RDSP(100),CHANGES(100)
    GO MAIN$
    EQU TRUE TO 1, FALSE TO 0
    INCLUDE EB.EQUS EB.EQUS
    MAIN$:!
!
    INCLUDE EB.OS.INCLUDES OS.REL
    INDENT = SPACE(4)
    IF WORD='EBOPTS' THEN
        CRT @(-1):'Options Help'
        CRT
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
        CRT INDENT:'S - Save and keep editing'
        CRT INDENT:'T - Rotate AM/VM layout'
        CRT INDENT:'U - Unindent'
        CRT INDENT:'V - Edit Values'
        CRT INDENT:'W - Swap/Convert current line'
        CRT INDENT:'    (e.g. LOCATE->INS; READ<->WRITE; FOR...->FOR...STEP-1;...and many more)'
        CRT INDENT:'X - Toggle 80/132 view'
        CRT INDENT:'Z - Record size'
        CRT
        CRT
        CRT
        CRT
        CRT INDENT:'Press any key...':
        CALL EB_GET_INPUT(CHR, CHR.NBR)
        IF FG$ACT.CODE = FG$HLP.CODE THEN GOSUB DisplayEBcmds
        FG$ACT.CODE=FALSE
        OS.HELP=TRUE
        RETURN
    END
    IF WORD='EBREPLACE' THEN
        CRT @(-1):'Replace Help'
        CRT
        CRT INDENT:'General syntax:'
        CRT INDENT:'R{opts}/<old>/<new>'
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
        IF FG$ACT.CODE = FG$HLP.CODE THEN GOSUB DisplayEBcmds
        FG$ACT.CODE=FALSE
        OS.HELP=TRUE
        RETURN
    END
    OS.HELP=FALSE
    WORD = TRIM(WORD)
    EXECUTE CHAR(255):'kman -k ':WORD:' 2>&1' CAPTURING list
    loc=0
    manpages=''
    FWORD=WORD:'()'
    LOOP
        REMOVE line FROM list AT loc SETTING delim
        line=SWAP(line,', ',@VM)
        LOCATE FWORD IN line<1,vm_start> SETTING POS THEN
            vol=OCONV(line,'MCN')
            IF vol#'' THEN
                POS=INDEX(line,vol,1)
                vol=FIELD(line[POS,9],')',1)
                LOCATE vol IN manpages<am_start> BY 'AR' SETTING pos ELSE
                    INS vol BEFORE manpages<pos>
                END
            END
            EXECUTE CHAR(255):'kman ':vol:' ':WORD:' 2>&1'
            OS.HELP=TRUE
        END
    WHILE delim DO REPEAT
    IF NOT(OS.HELP) THEN
        CRT @(-1):
        EXECUTE CHAR(255):'kman -M $JBCRELEASEDIR/man ':WORD:' 2>&1' CAPTURING list
        notfound = INDEX(list, 'o manual entry', 1) OR INDEX(list, 'hat manual page', 1)
        IF notfound THEN
            CRT list
            CRT
        END ELSE
            EXECUTE CHAR(255):'kman -M $JBCRELEASEDIR/man ':WORD:' 2>&1' CAPTURING help
            K.HELP = '%EB_HELP*':WORD:'%'
            WRITE help ON JET.PASTE,K.HELP
            EXECUTE 'EB JET.PASTE ':K.HELP
            DELETE JET.PASTE,K.HELP
        END
        CRT
        CRT 'Press return or F1 for EB help':
        CALL EB_GET_INPUT(CHR, CHR.NBR)
        IF FG$ACT.CODE = FG$HLP.CODE THEN GOSUB DisplayEBcmds
        FG$ACT.CODE=FALSE
        OS.HELP=TRUE
    END
!    BEGIN CASE
!    CASE OS.REL='UDT'
!        EXECUTE 'HELP UNIBASIC ':WORD
!        OS.HELP=TRUE
!    CASE OS.REL='JB'
!        EXECUTE CHAR(255):'k%JBCGLOBALDIR%\man\manhtml\jbc2_':WORD:'.html'
!    CASE 1; OS.HELP=FALSE
!    END CASE
    RETURN
DisplayEBcmds:
    CRT @(-1):'EB commands...'
    CRT
    hash = 'L#20 ':
    keyboard = ''
    keyboard<-1> = 'END.CH' hash:OCONV(EB$CHARS(2),'ESC')
    keyboard<-1> = 'ABT.CH' hash:OCONV(EB$CHARS(3),'ESC')
    keyboard<-1> = 'BCK.CH' hash:OCONV(EB$CHARS(4),'ESC')
    keyboard<-1> = 'SRCH.CH' hash:OCONV(EB$CHARS(5),'ESC')
    keyboard<-1> = 'RFR.CH' hash:OCONV(EB$CHARS(6),'ESC')
    keyboard<-1> = 'LST.CH' hash:OCONV(EB$CHARS(7),'ESC')
    keyboard<-1> = 'TCL.CH' hash:OCONV(EB$CHARS(8),'ESC')
    keyboard<-1> = 'SKP.CH' hash:OCONV(EB$CHARS(9),'ESC')
    keyboard<-1> = 'SEL.CH' hash:OCONV(EB$CHARS(10),'ESC')
    keyboard<-1> = 'HLP.CH' hash:OCONV(EB$CHARS(11),'ESC')
!    keyboard<-1> = 'FUNK.CH' hash:OCONV(EB$CHARS(12),'ESC')
!    keyboard<-1> = 'DEL.CMD' hash:OCONV(EB$CHARS(13),'ESC')
!    keyboard<-1> = 'AMD.CMD' hash:OCONV(EB$CHARS(14),'ESC')
!    keyboard<-1> = 'PRT.CMD' hash:OCONV(EB$CHARS(15),'ESC')
!    keyboard<-1> = 'IND.CMD' hash:OCONV(EB$CHARS(16),'ESC')
!    keyboard<-1> = 'LN.CMD' hash:OCONV(EB$CHARS(17),'ESC')
!    keyboard<-1> = 'ULN.CMD' hash:OCONV(EB$CHARS(18),'ESC')
!    keyboard<-1> = 'DLN.CMD' hash:OCONV(EB$CHARS(19),'ESC')
!    keyboard<-1> = 'INS.CMD' hash:OCONV(EB$CHARS(20),'ESC')
!    keyboard<-1> = 'BCK.CMD' hash:OCONV(EB$CHARS(21),'ESC')
!    keyboard<-1> = 'FWD.CMD' hash:OCONV(EB$CHARS(22),'ESC')
!    keyboard<-1> = 'REP.CMD' hash:OCONV(EB$CHARS(23),'ESC')
!    keyboard<-1> = 'CP.CMD' hash:OCONV(EB$CHARS(24),'ESC')
!    keyboard<-1> = 'MV.CMD' hash:OCONV(EB$CHARS(25),'ESC')
    keyboard<-1> = 'L.CASE.CH' hash:OCONV(EB$CHARS(26),'ESC')
    keyboard<-1> = 'CASE.CH' hash:OCONV(EB$CHARS(27),'ESC')
    keyboard<-1> = 'BWORD.CH' hash:OCONV(EB$CHARS(28),'ESC')
    keyboard<-1> = 'EOL.CH' hash:OCONV(EB$CHARS(29),'ESC')
    keyboard<-1> = 'FWORD.CH' hash:OCONV(EB$CHARS(30),'ESC')
    keyboard<-1> = 'LEFT.CH' hash:OCONV(EB$CHARS(31),'ESC')
    keyboard<-1> = 'TAB.CH' hash:OCONV(EB$CHARS(32),'ESC')
    keyboard<-1> = 'DOWN.CH' hash:OCONV(EB$CHARS(33),'ESC')
    keyboard<-1> = 'UP.CH' hash:OCONV(EB$CHARS(34),'ESC')
    keyboard<-1> = 'SKIP.CH' hash:OCONV(EB$CHARS(35),'ESC')
    keyboard<-1> = 'MULTI.CH' hash:OCONV(EB$CHARS(36),'ESC')
    keyboard<-1> = 'PASTE.CH' hash:OCONV(EB$CHARS(37),'ESC')
    keyboard<-1> = 'TOP.CH' hash:OCONV(EB$CHARS(38),'ESC')
    keyboard<-1> = 'INSMOD.CH' hash:OCONV(EB$CHARS(39),'ESC')
    keyboard<-1> = 'SPELL.CH' hash:OCONV(EB$CHARS(40),'ESC')
    keyboard<-1> = 'SOL.CH' hash:OCONV(EB$CHARS(41),'ESC')
    keyboard<-1> = 'CUT.CH' hash:OCONV(EB$CHARS(42),'ESC')
    keyboard<-1> = 'UNDEL.CH' hash:OCONV(EB$CHARS(43),'ESC')
    keyboard<-1> = 'INSLN.CH' hash:OCONV(EB$CHARS(44),'ESC')
    keyboard<-1> = 'BTAB.CH' hash:OCONV(EB$CHARS(45),'ESC')
    keyboard<-1> = 'INS.CH' hash:OCONV(EB$CHARS(46),'ESC')
    keyboard<-1> = 'DELLN.CH' hash:OCONV(EB$CHARS(47),'ESC')
    keyboard<-1> = 'DEL.CH' hash:OCONV(EB$CHARS(48),'ESC')
    keyboard<-1> = 'DELW.CH' hash:OCONV(EB$CHARS(49),'ESC')
    keyboard<-1> = 'SUS.CH' hash:OCONV(EB$CHARS(50),'ESC')
    keyboard<-1> = 'PRV.CH' hash:OCONV(EB$CHARS(51),'ESC')
    keyboard<-1> = 'NXT.CH' hash:OCONV(EB$CHARS(52),'ESC')
    keyboard<-1> = 'MERGE.CH' hash:OCONV(EB$CHARS(53),'ESC')
    keyboard<-1> = 'OPT.CH' hash:OCONV(EB$CHARS(54),'ESC')
    keyboard<-1> = 'JMP.CH' hash:OCONV(EB$CHARS(55),'ESC')
    keyboard<-1> = 'NXTKEY.CH' hash:OCONV(EB$CHARS(56),'ESC')
    keyboard<-1> = 'SCRN.SEL' hash:OCONV(EB$CHARS(57),'ESC')
    keyboard<-1> = 'TAG.CH' hash:OCONV(EB$CHARS(58),'ESC')
    keyboard<-1> = 'EXIT.LN' hash:OCONV(EB$CHARS(59),'ESC')
!    keyboard<-1> = 'TAG.CMD' hash:OCONV(EB$CHARS(60),'ESC')
    keyboard<-1> = 'HOT.KEYS' hash:OCONV(EB$CHARS(61),'ESC')
    keyboard<-1> = 'QUICK.CH' hash:OCONV(EB$CHARS(62),'ESC')
    keyboard<-1> = 'TUT.CH' hash:OCONV(EB$CHARS(63),'ESC')
    keyboard<-1> = 'MOUSE.CH' hash:OCONV(EB$CHARS(64),'ESC')
!    keyboard<-1> = 'EXPECT.CR' hash:OCONV(EB$CHARS(65),'ESC')
    keyboard<-1> = 'BS.CH' hash:OCONV(EB$CHARS(66),'ESC')
    keyboard<-1> = 'ALT.CH' hash:OCONV(EB$CHARS(67),'ESC')
!    keyboard<-1> = 'CLR.CMD' hash:OCONV(EB$CHARS(68),'ESC')
    keyboard<-1> = 'PRVKEY.CH' hash:OCONV(EB$CHARS(69),'ESC')
    keyboard<-1> = 'GOTO.CH' hash:OCONV(EB$CHARS(70),'ESC')
    keyboard<-1> = 'MENU.CH' hash:OCONV(EB$CHARS(71),'ESC')
    keyboard<-1> = 'ADD.CH' hash:OCONV(EB$CHARS(72),'ESC')
    keyboard<-1> = 'APP.CH' hash:OCONV(EB$CHARS(73),'ESC')
    keyboard<-1> = 'AMD.CH' hash:OCONV(EB$CHARS(74),'ESC')
    keyboard<-1> = 'BOT.CH' hash:OCONV(EB$CHARS(75),'ESC')
!    keyboard<-1> = 'FUNC.CHARS' hash:OCONV(EB$CHARS(97),'ESC')
!    keyboard<-1> = 'FUNC.VALS' hash:OCONV(EB$CHARS(98),'ESC')
!    keyboard<-1> = 'LAST.CHR' hash:OCONV(EB$CHARS(99),'ESC')
!    keyboard<-1> = 'MNENOMICS' hash:OCONV(EB$CHARS(100),'ESC')
    nbr_keys = DCOUNT(keyboard, @AM)
    keyboard = SORT(keyboard)
    FOR i = 1 TO nbr_keys
        CRT INDENT:keyboard<i>
        IF i = nbr_keys OR (MOD(i, PDEPTH-3) = 0) THEN
            CRT
            CALL EB_GET_INPUT(CHR, CHR.NBR)
            IF FG$ACT.CODE = FG$ABT.CODE THEN RETURN
        END
    NEXT i
    RETURN
