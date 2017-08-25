    SUBROUTINE EB_CONVCODE(KeyCode, CHR, FG$ACT.CODE)
* @(#) EB_CONVCODE.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.CONVCODE Ported to jBASE 16:15:14  27 JUL 2001
!
! Converts jBASE standard input codes to EB's codes
!
    CRT @(0,40):KeyCode:@(-4):' ':SEQ(CHR):
    INCLUDE jCmdKeys.h
    INCLUDE EB.EQUS ACT.CODES
    BEGIN CASE
    CASE KeyCode=cmd_insert_space           ; FG$ACT.CODE=FG$INS.CODE ;! Insert space at current pos
    CASE KeyCode=cmd_start_line             ; FG$ACT.CODE=FG$SOL.CODE ;! Move to start of line
    CASE KeyCode=cmd_change_case            ; FG$ACT.CODE=FG$CASE.CODE          ;! Change case of character
    CASE KeyCode=cmd_end_line               ; FG$ACT.CODE=FG$EOL.CODE ;! Move to end of line
    CASE KeyCode=cmd_mark_line              ; FG$ACT.CODE=FG$GOTO.CODE          ;! Mark line ready for block cmd
    CASE KeyCode=cmd_forward_tab            ; FG$ACT.CODE=FG$TAB.CODE ;! Normal forward tab
    CASE KeyCode=cmd_clear_end_line         ; FG$ACT.CODE=FG$DEL.LINE.CODE      ;! Clear to end of line
    CASE KeyCode=cmd_insert_line            ; FG$ACT.CODE=FG$INS.LINE.CODE      ;! Insert a blank line
    CASE KeyCode=cmd_carriage_return        ; RETURN        ;! Normal carriage return char
    CASE KeyCode=cmd_next_locate            ; FG$ACT.CODE=FG$MULTI.CODE         ;! Look next occurence of str
    CASE KeyCode=cmd_key_f13                ; FG$ACT.CODE=FG$PASTE.CODE         ;!FG$PRV.KEY.CODE ;! Look previous occurence of str
    CASE KeyCode=cmd_prev_locate            ; FG$ACT.CODE=FG$BSEARCH.CODE
    CASE KeyCode=cmd_toggle_insert          ; FG$ACT.CODE=FG$INSERT.CODE        ;! Toggle insert and over-write
    CASE KeyCode=cmd_repaint                ; RETURN ; FG$ACT.CODE=FG$RFR.CODE  ;! Repaint screen lose recent upd
    CASE KeyCode=cmd_copy_character         ; FG$ACT.CODE=FG$TAG.CODE ;! Copy character on previous line
    CASE KeyCode=cmd_indent_screen          ; FG$ACT.CODE=0 ;! Indent current screen
    CASE KeyCode=cmd_delete_word            ; FG$ACT.CODE=FG$DEL.WORD.CODE      ;! Delete next word
    CASE KeyCode=cmd_exit_record            ; FG$ACT.CODE=FG$ABT.CODE ;! Exit the record
    CASE KeyCode=cmd_back_tab               ; FG$ACT.CODE=FG$APP.CODE ;! Backwards tab position
    CASE KeyCode=cmd_escape                 ; FG$ACT.CODE=FG$ABT.CODE ;! Normal escape character
    CASE KeyCode=cmd_insert_sub_value       ; FG$ACT.CODE=0 ;! Insert a char(252)
    CASE KeyCode=cmd_insert_value           ; FG$ACT.CODE=FG$CUT.CODE ;! Insert a char(253)
    CASE KeyCode=cmd_cursor_up              ; FG$ACT.CODE=FG$BCK.CODE ;! Move cursor up a line
    CASE KeyCode=cmd_cursor_down            ; FG$ACT.CODE=FG$SKP.CODE ;! Move cursor down a line
    CASE KeyCode=cmd_cursor_right           ; FG$ACT.CODE=FG$RIGHT.CODE         ;! Move cursor to the right
    CASE KeyCode=cmd_scroll_bottom          ; FG$ACT.CODE=FG$BOT.CODE ;! Move to end of record
    CASE KeyCode=cmd_cursor_left OR SEQ(CHR)=21           ; FG$ACT.CODE=FG$LEFT.CODE      ;! Move cursor to the left
    CASE KeyCode=cmd_scroll_up_line         ; FG$ACT.CODE=FG$PRV.KEY.CODE       ;! Scroll screen up a single line
    CASE KeyCode=cmd_scroll_down_line       ; FG$ACT.CODE=FG$NXT.KEY.CODE       ;! Scroll screen down single line
    CASE KeyCode=cmd_scroll_up_half         ; FG$ACT.CODE=0 ;! Scroll screen up a half page
    CASE KeyCode=cmd_scroll_down_half       ; FG$ACT.CODE=0 ;! Scroll screen down a half page
    CASE KeyCode=cmd_scroll_up_page         ; FG$ACT.CODE=FG$PRVS.CODE          ;! Scroll screen up a full page
    CASE KeyCode=cmd_scroll_down_page       ; FG$ACT.CODE=FG$NXTS.CODE          ;! Scroll screen down a full page
    CASE KeyCode=cmd_scroll_top             ; FG$ACT.CODE=FG$TOP.CODE ;! Move to top of record
    CASE KeyCode=cmd_scroll_bottom          ; FG$ACT.CODE=FG$BOT.CODE ;! Move to end of record
    CASE KeyCode=cmd_next_indent            ; FG$ACT.CODE=FG$FWORD.CODE         ;! Find matching next indent
    CASE KeyCode=cmd_prev_indent            ; FG$ACT.CODE=FG$BWORD.CODE         ;! Find matching previous indent
    CASE KeyCode=cmd_backspace              ; RETURN        ;! Normal backspace character
    CASE KeyCode=cmd_delete_char            ; FG$ACT.CODE=FG$DEL.CHAR.CODE      ;! Delete a character
    CASE KeyCode=cmd_delete_line            ; FG$ACT.CODE=FG$DEL.LINE.CODE      ;! Delete a line
*
* cmd_key_f1, to cmd_key_f10 used to be equated to the functions that jed/jsh performed for these
*	keys (eg cmd_scroll_up_line). This meant you could not distinguish between a function key and
*	a page down for instance.
*
    CASE KeyCode=cmd_key_f1		  ; FG$ACT.CODE=FG$HLP.CODE    ;! Function Key
    CASE KeyCode=cmd_key_f2		  ; FG$ACT.CODE=FG$OPT.CODE    ;! Function Key
    CASE KeyCode=cmd_key_f3		  ; FG$ACT.CODE=FG$JMP.CODE    ;! Function Key
    CASE KeyCode=cmd_key_f4		  ; FG$ACT.CODE=FG$END.CODE    ;! Function Key
    CASE KeyCode=cmd_key_f5		  ; FG$ACT.CODE=FG$RFR.CODE    ;! Function Key
    CASE KeyCode=cmd_key_f6		  ; FG$ACT.CODE=FG$SEL.CODE    ;! Function Key
    CASE KeyCode=cmd_key_f7		  ; FG$ACT.CODE=FG$LST.CODE    ;! Function Key
    CASE KeyCode=cmd_key_f8		  ; FG$ACT.CODE=FG$SEARCH.CODE ;! Function Key
    CASE KeyCode=cmd_key_f9		  ; FG$ACT.CODE=FG$INSERT.CODE ;! Function Key
    CASE KeyCode=cmd_key_f10		  ; FG$ACT.CODE=FG$TCL.CODE   ;! Function Key

    CASE KeyCode=cmd_key_f11		  ; FG$ACT.CODE=FG$CASE.CODE  ;! Function key
    CASE KeyCode=cmd_key_f12		  ; FG$ACT.CODE=FG$L.CASE.CODE          ;! Function key
    CASE KeyCode=cmd_key_f13		  ; FG$ACT.CODE=FG$SEARCH.CODE          ;! Function key
    CASE KeyCode=cmd_key_f14		  ; FG$ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f15		  ; FG$ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f16		  ; FG$ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f17		  ; FG$ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f18		  ; FG$ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f19		  ; FG$ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f20		  ; FG$ACT.CODE=0   ;! Function key
*
    CASE KeyCode=cmd_key_f21		  ; FG$ACT.CODE=0   ;! Function key shifted
    CASE KeyCode=cmd_key_f22		  ; FG$ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f23		  ; FG$ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f24		  ; FG$ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f25		  ; FG$ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f26		  ; FG$ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f27		  ; FG$ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f28		  ; FG$ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f29		  ; FG$ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f30		  ; FG$ACT.CODE=FG$MOUSE.CODE ;! Function key
    CASE KeyCode=cmd_key_f31		  ; FG$ACT.CODE=FG$ADD.CODE   ;! Function key
    CASE KeyCode=cmd_key_f32		  ; FG$ACT.CODE=FG$APP.CODE
*
    CASE KeyCode=cmd_error                  ; FG$ACT.CODE=FG$UNDEL.CODE         ;! Unknown command string
    CASE KeyCode=cmd_alpha_numeric          ; FG$ACT.CODE=0 ;! Simple alpha-numeric
    CASE KeyCode=cmd_timeout                ; FG$TIMEDOUT=1 ;! The input timed out
    CASE KeyCode=cmd_winsize                ; FG$ACT.CODE=0 ;! Size of the 0 window changed.
    CASE 1
        CRT @(-1):KeyCode
        CRT OCONV(CHR,'MX')
        STOP
    END CASE
    RETURN
