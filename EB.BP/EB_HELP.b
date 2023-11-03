    SUBROUTINE EB_HELP(WORD,OS.HELP)
    INCLUDE EB.EQUS EB.COMMON
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS ACT.CODES
    GO MAIN$
    EQU TRUE TO 1, FALSE TO 0, ESC TO CHAR(27)
    INCLUDE EB.EQUS EB.EQUS
    INCLUDE JBC.h
    DEFFUN EBJSHOW()
    DEFFUN FNKEYTRANS()
MAIN$:!
!
    accuterm = @FALSE
    hash = 'L#25 ':
    minihash = 'L#6 ':
    uline = STR('=',78)
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
            hkey = 'EB_opts.txt'
            READ helptext FROM FG_EB.PARAMS,hkey THEN
                GOSUB parse_help
            END ELSE
                helptext = 'Missing ':hkey:' from EB.PARAMS'
            END
            GOSUB showhelp
        CASE WORD='EBSEARCH'
            CRT @(-1):'Search Help'
            CRT
            hkey = 'EB_search.txt'
            READ helptext FROM FG_EB.PARAMS,hkey THEN
                GOSUB parse_help
            END ELSE
                helptext = 'Missing ':hkey:' from EB.PARAMS'
            END
            GOSUB showhelp
        CASE WORD='EBREPLACE'
            CRT @(-1):'Replace Help'
            CRT
            hkey = 'EB_replace.txt'
            READ helptext FROM FG_EB.PARAMS,hkey THEN
                GOSUB parse_help
            END ELSE
                helptext = 'Missing ':hkey:' from EB.PARAMS'
            END
            GOSUB showhelp
        CASE WORD='EBCUT'
            CRT @(-1):'Cut/Paste Help'
            CRT
            hkey = 'EB_cut.txt'
            READ helptext FROM FG_EB.PARAMS,hkey THEN
                GOSUB parse_help
            END ELSE
                helptext = 'Missing ':hkey:' from EB.PARAMS'
            END
            GOSUB showhelp
        CASE 1
            OS.HELP=FALSE
            WORD = TRIM(WORD)
            IF accuterm THEN
                IF @FALSE THEN
                    lword = DOWNCASE(WORD)
                    READV help_url FROM FG_EB.CONTROL,'jbasedoc_url',1 ELSE
                        help_url = 'https://docs.zumasys.com/jbase/jbc'
                    END
                    READV word_url FROM FG_EB.CONTROL,'jbc_':lword,1 ELSE word_url = lword:'/#':lword
                    URL = help_url:'/':word_url
                    EXECUTE @IM:'kcurl ':URL:' 2>&1' CAPTURING io
                    FG_ACT.CODE=FALSE
                    IF NOT(INDEX(io, 'Phil Collins', 1)) THEN
                        CRT @ESC:CHAR(2):'<':URL:@CR:
                        OS.HELP = @TRUE
                    END
                END
            END ELSE
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
                        IF LEN(vol) THEN
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
            END
            IF NOT(OS.HELP) THEN
                IF LEN(EBJSHOW('-c man')) AND LEN(WORD) THEN
                    CRT @(-1):
                    IF DIR_DELIM_CH = '/' THEN
                        mandir = '-M $JBCRELEASEDIR/man '
                    END ELSE mandir = ''
                    EXECUTE ksh:'man ':mandir:WORD:' 2>&1' CAPTURING list
                    notfound = INDEX(list, 'o manual entry', 1) OR INDEX(list, 'hat manual page', 1)
                    IF NOT(notfound) THEN
                        EXECUTE ksh:'man ':mandir:WORD:' 2>&1' CAPTURING help
                        K.HELP = '%EB_HELP*':WORD:'%'
                        WRITE help ON JET.PASTE,K.HELP
                        rc = IOCTL(JET.PASTE, JBC_COMMAND_GETFILENAME, OS.HELP)
                        EXECUTE 'EB ':OS.HELP:' ':K.HELP
                        DELETE JET.PASTE,K.HELP
                        CRT
                        CRT 'Press return or F1 for EB help':
                        CALL EB_GET_INPUT(CHR, CHR.NBR)
                    END
                END ELSE FG_ACT.CODE = FG_HLP.CODE
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
    CRT @(-1):'EB Help'
    CRT
    READ helptext FROM FG_EB.PARAMS, 'EB_help.txt' THEN
        GOSUB parse_help
    END ELSE
        helptext = 'Editing programs items with EB can be initiated as follows:'
        helptext<-1> = \\
        helptext<-1> = \EB\ hash:\Popup-list of previous sessions will display\
        helptext<-1> = \\ hash:\ - The [F8] key can be used to filter\
        helptext<-1> = \EB\ hash:\Edit last file/program\
        helptext<-1> = \EB\ hash:\Edit previous to last file (pops last file)\
        helptext<-1> = \EB\ hash:\Similar to ED/JED, can be run from a list\
        helptext<-1> = \EB\ hash:\Enter one or more cataloged programs/subroutines \
        helptext<-1> = \EBFIND\ hash:\Search for items with matching text and invoke EB\
        helptext<-1> = \\
        helptext<-1> = \Editing tips:\
        helptext<-1> = \- pressing <enter> in the middle of a line will split the line\
        helptext<-1> = \- pressing <del> at the end of a line will joine the line below\
    END
    mainhelp = helptext

    READ helptext FROM FG_EB.PARAMS,'EB_common.txt' THEN
        GOSUB parse_help
    END ELSE
        helptext = ''
        helptext<-1> = 'Help' hash:FNKEYTRANS(EB_CHARS(11)) minihash:\(available for [F2], [F6], [F8] and ctrl-R)\
        helptext<-1> = 'Save/accept' hash:FNKEYTRANS(EB_CHARS(2))
        helptext<-1> = 'Exit/cancel' hash:FNKEYTRANS(EB_CHARS(3)) minihash:\(press twice to exit)\
        helptext<-1> = 'Options' hash:FNKEYTRANS(EB_CHARS(54))
        helptext<-1> = 'Refresh' hash:FNKEYTRANS(EB_CHARS(6))
        helptext<-1> = 'Search' hash:FNKEYTRANS(EB_CHARS(5))
        helptext<-1> = 'Reverse search' hash:FNKEYTRANS(EB_CHARS(99))
        helptext<-1> = 'Shell' hash:FNKEYTRANS(EB_CHARS(8))
        helptext<-1> = 'Next record' hash:FNKEYTRANS(EB_CHARS(56))
        helptext<-1> = 'Prev record' hash:FNKEYTRANS(EB_CHARS(69))
    END
    general = helptext
    READ helptext FROM FG_EB.PARAMS,'EB_navigation.txt' THEN
        GOSUB parse_help
    END ELSE
        helptext = ''
        helptext<-1> = 'Bottom of screen/record' hash:FNKEYTRANS(EB_CHARS(75))
        helptext<-1> = 'Zoom' hash:FNKEYTRANS(EB_CHARS(55))
        helptext<-1> = 'Previous field' hash:FNKEYTRANS(EB_CHARS(4))
        helptext<-1> = 'Page Up' hash:FNKEYTRANS(EB_CHARS(51))
        helptext<-1> = 'Page Down' hash:FNKEYTRANS(EB_CHARS(52))
        helptext<-1> = 'Bookmark' hash:FNKEYTRANS(EB_CHARS(7))
        helptext<-1> = 'Prev word' hash:FNKEYTRANS(EB_CHARS(28))
        helptext<-1> = 'End of line' hash:FNKEYTRANS(EB_CHARS(29))
        helptext<-1> = 'Next word' hash:FNKEYTRANS(EB_CHARS(30))
        helptext<-1> = 'Leftarrow' hash:FNKEYTRANS(EB_CHARS(31))
        helptext<-1> = 'Downarrow' hash:FNKEYTRANS(EB_CHARS(33))
        helptext<-1> = 'Uparrow' hash:FNKEYTRANS(EB_CHARS(34))
        helptext<-1> = 'Rightarrow' hash:FNKEYTRANS(EB_CHARS(35))
        helptext<-1> = 'Next occurrence' hash:FNKEYTRANS(EB_CHARS(36))
        helptext<-1> = 'Top of screen/record' hash:FNKEYTRANS(EB_CHARS(38))
        helptext<-1> = 'Start of line' hash:FNKEYTRANS(EB_CHARS(41))
        helptext<-1> = 'Back tab' hash:FNKEYTRANS(EB_CHARS(45))
        helptext<-1> = 'Goto line/label' hash:FNKEYTRANS(EB_CHARS(70))
    END
    navigation = helptext
    READ helptext FROM FG_EB.PARAMS,'EB_editing.txt' THEN
        GOSUB parse_help
    END ELSE
        helptext = ''
        helptext<-1> = 'Start/end block' hash:FNKEYTRANS(EB_CHARS(10)) minihash:\(press twice to select current line)\
        helptext<-1> = 'Lower case' hash:FNKEYTRANS(EB_CHARS(26))
        helptext<-1> = 'Toggle case' hash:FNKEYTRANS(EB_CHARS(27))
        helptext<-1> = 'Tab' hash:FNKEYTRANS(EB_CHARS(32))
        helptext<-1> = 'Paste' hash:FNKEYTRANS(EB_CHARS(37))
        helptext<-1> = 'Toggle INS/OVR' hash:FNKEYTRANS(EB_CHARS(39))
        helptext<-1> = 'Truncate/cut' hash:FNKEYTRANS(EB_CHARS(42))
        helptext<-1> = 'Undo' hash:FNKEYTRANS(EB_CHARS(43))
        helptext<-1> = 'Redo' hash:FNKEYTRANS(EB_CHARS(12))
        helptext<-1> = 'Insert line' hash:FNKEYTRANS(EB_CHARS(44))
        helptext<-1> = 'Insert space' hash:FNKEYTRANS(EB_CHARS(46))
        helptext<-1> = 'Delete line' hash:FNKEYTRANS(EB_CHARS(47))
        helptext<-1> = 'Delete char' hash:FNKEYTRANS(EB_CHARS(48))
        helptext<-1> = 'Delete word' hash:FNKEYTRANS(EB_CHARS(49))
        helptext<-1> = 'Comment/select toggle' hash:FNKEYTRANS(EB_CHARS(58))
        helptext<-1> = 'Backspace' hash:FNKEYTRANS(EB_CHARS(66))
    END
    editing = helptext
    helptext = ''
    helptext<-1> = ''
    helptext<-1> = uline:@AM:'Main':@AM:uline:@AM:mainhelp
    helptext<-1> = ''
    helptext<-1> = uline:@AM:'General':@AM:uline:@AM:general
    helptext<-1> = ''
    helptext<-1> = uline:@AM:'Navigation':@AM:uline:@AM:navigation
    helptext<-1> = ''
    helptext<-1> = uline:@AM:'Editing':@AM:uline:@AM:editing
    GOSUB showhelp
showhelp:
    nbr_lines = DCOUNT(helptext, @AM)
    FOR i = 1 TO nbr_lines
        CRT INDENT:helptext<i>
        IF MOD(i, PDEPTH-3) EQ 0 THEN
            CRT
            CALL EB_GET_INPUT(CHR, CHR.NBR)
            IF FG_ACT.CODE = FG_ABT.CODE THEN RETURN
        END
    NEXT i
    CRT
    CRT INDENT:'Press any key...':
    CALL EB_GET_INPUT(CHR, CHR.NBR)
    IF FG_ACT.CODE = FG_HLP.CODE THEN GOSUB DisplayEBcmds
    SCR.LR=1
    CALL EB_REFRESH
    FG_ACT.CODE=FALSE
    OS.HELP=TRUE
    RETURN
parse_help:
    dc = DCOUNT(helptext, @AM)
    ebcmatch = "'ebc_'1N0N"
    FOR L = dc TO 1 STEP -1
        line = helptext<L>
        fkey = line<1,2,1>
        IF fkey MATCHES ebcmatch THEN
            line<1,2,1> = FNKEYTRANS(EB_CHARS(OCONV(line<1,2,1>, 'MCN')))
        END
        IF LEN(line<1,2,2>) THEN
            hlen = FIELD(line<1,2,1>, TAB, 2)
            IF hlen THEN
                hlen = 'L#':hlen
                line<1,2,1> = line<1,2,1>[1,COL1()-1]
            END ELSE
                hlen = minihash
            END

            line<1,2,1> = line<1,2,1> hlen
        END
        IF LEN(line<1,2,1>) THEN line<1,1> = line<1,1> hash
        tabopt = FIELD(line<1,1>, TAB, 2)
        underline = @FALSE
        IF tabopt THEN
            line<1,1> = line<1,1>[1, COL1()-1]
            BEGIN CASE
                CASE tabopt EQ 'u'
                    underline = @TRUE
            END CASE
        END
        helptext<L> = line<1,1>:line<1,2,1>:line<1,2,2>
        IF underline THEN
            INS STR('-', LEN(line<1,1>)) BEFORE helptext<L+1>
        END
    NEXT L
    RETURN
