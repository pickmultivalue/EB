    SUBROUTINE EB_CONVCODE(KeyCode, CHR, FG_ACT.CODE)
!
! Converts jBASE standard input codes to EB's codes
!
    CRT @(0,40):KeyCode:@(-4):' ':SEQ(CHR):
    INCLUDE jCmdKeys.h
    INCLUDE EB.EQUS ACT.CODES
    BEGIN CASE
    CASE KeyCode=cmd_insert_space           ; FG_ACT.CODE=FG_INS.CODE ;! Insert space at current pos
    CASE KeyCode=cmd_start_line             ; FG_ACT.CODE=FG_SOL.CODE ;! Move to start of line
    CASE KeyCode=cmd_change_case            ; FG_ACT.CODE=FG_CASE.CODE          ;! Change case of character
    CASE KeyCode=cmd_end_line               ; FG_ACT.CODE=FG_EOL.CODE ;! Move to end of line
    CASE KeyCode=cmd_mark_line              ; FG_ACT.CODE=FG_GOTO.CODE          ;! Mark line ready for block cmd
    CASE KeyCode=cmd_forward_tab            ; FG_ACT.CODE=FG_TAB.CODE ;! Normal forward tab
    CASE KeyCode=cmd_clear_end_line         ; FG_ACT.CODE=FG_DEL.LINE.CODE      ;! Clear to end of line
    CASE KeyCode=cmd_insert_line            ; FG_ACT.CODE=FG_INS.LINE.CODE      ;! Insert a blank line
    CASE KeyCode=cmd_carriage_return        ; RETURN        ;! Normal carriage return char
    CASE KeyCode=cmd_next_locate            ; FG_ACT.CODE=FG_MULTI.CODE         ;! Look next occurence of str
    CASE KeyCode=cmd_key_f13                ; FG_ACT.CODE=FG_PASTE.CODE         ;!FG_PRV.KEY.CODE ;! Look previous occurence of str
    CASE KeyCode=cmd_prev_locate            ; FG_ACT.CODE=FG_BSEARCH.CODE
    CASE KeyCode=cmd_toggle_insert          ; FG_ACT.CODE=FG_INSERT.CODE        ;! Toggle insert and over-write
    CASE KeyCode=cmd_repaint                ; RETURN ; FG_ACT.CODE=FG_RFR.CODE  ;! Repaint screen lose recent upd
    CASE KeyCode=cmd_copy_character         ; FG_ACT.CODE=FG_TAG.CODE ;! Copy character on previous line
    CASE KeyCode=cmd_indent_screen          ; FG_ACT.CODE=0 ;! Indent current screen
    CASE KeyCode=cmd_delete_word            ; FG_ACT.CODE=FG_DEL.WORD.CODE      ;! Delete next word
    CASE KeyCode=cmd_exit_record            ; FG_ACT.CODE=FG_ABT.CODE ;! Exit the record
    CASE KeyCode=cmd_back_tab               ; FG_ACT.CODE=FG_APP.CODE ;! Backwards tab position
    CASE KeyCode=cmd_escape                 ; FG_ACT.CODE=FG_ABT.CODE ;! Normal escape character
    CASE KeyCode=cmd_insert_sub_value       ; FG_ACT.CODE=0 ;! Insert a char(252)
    CASE KeyCode=cmd_insert_value           ; FG_ACT.CODE=FG_CUT.CODE ;! Insert a char(253)
    CASE KeyCode=cmd_cursor_up              ; FG_ACT.CODE=FG_BCK.CODE ;! Move cursor up a line
    CASE KeyCode=cmd_cursor_down            ; FG_ACT.CODE=FG_SKP.CODE ;! Move cursor down a line
    CASE KeyCode=cmd_cursor_right           ; FG_ACT.CODE=FG_RIGHT.CODE         ;! Move cursor to the right
    CASE KeyCode=cmd_scroll_bottom          ; FG_ACT.CODE=FG_BOT.CODE ;! Move to end of record
    CASE KeyCode=cmd_cursor_left OR SEQ(CHR)=21           ; FG_ACT.CODE=FG_LEFT.CODE      ;! Move cursor to the left
    CASE KeyCode=cmd_scroll_up_line         ; FG_ACT.CODE=FG_PRV.KEY.CODE       ;! Scroll screen up a single line
    CASE KeyCode=cmd_scroll_down_line       ; FG_ACT.CODE=FG_NXT.KEY.CODE       ;! Scroll screen down single line
    CASE KeyCode=cmd_scroll_up_half         ; FG_ACT.CODE=0 ;! Scroll screen up a half page
    CASE KeyCode=cmd_scroll_down_half       ; FG_ACT.CODE=0 ;! Scroll screen down a half page
    CASE KeyCode=cmd_scroll_up_page         ; FG_ACT.CODE=FG_PRVS.CODE          ;! Scroll screen up a full page
    CASE KeyCode=cmd_scroll_down_page       ; FG_ACT.CODE=FG_NXTS.CODE          ;! Scroll screen down a full page
    CASE KeyCode=cmd_scroll_top             ; FG_ACT.CODE=FG_TOP.CODE ;! Move to top of record
    CASE KeyCode=cmd_scroll_bottom          ; FG_ACT.CODE=FG_BOT.CODE ;! Move to end of record
    CASE KeyCode=cmd_next_indent            ; FG_ACT.CODE=FG_FWORD.CODE         ;! Find matching next indent
    CASE KeyCode=cmd_prev_indent            ; FG_ACT.CODE=FG_BWORD.CODE         ;! Find matching previous indent
    CASE KeyCode=cmd_backspace              ; RETURN        ;! Normal backspace character
    CASE KeyCode=cmd_delete_char            ; FG_ACT.CODE=FG_DEL.CHAR.CODE      ;! Delete a character
    CASE KeyCode=cmd_delete_line            ; FG_ACT.CODE=FG_DEL.LINE.CODE      ;! Delete a line
*
* cmd_key_f1, to cmd_key_f10 used to be equated to the functions that jed/jsh performed for these
*	keys (eg cmd_scroll_up_line). This meant you could not distinguish between a function key and
*	a page down for instance.
*
    CASE KeyCode=cmd_key_f1		  ; FG_ACT.CODE=FG_HLP.CODE    ;! Function Key
    CASE KeyCode=cmd_key_f2		  ; FG_ACT.CODE=FG_OPT.CODE    ;! Function Key
    CASE KeyCode=cmd_key_f3		  ; FG_ACT.CODE=FG_JMP.CODE    ;! Function Key
    CASE KeyCode=cmd_key_f4		  ; FG_ACT.CODE=FG_END.CODE    ;! Function Key
    CASE KeyCode=cmd_key_f5		  ; FG_ACT.CODE=FG_RFR.CODE    ;! Function Key
    CASE KeyCode=cmd_key_f6		  ; FG_ACT.CODE=FG_SEL.CODE    ;! Function Key
    CASE KeyCode=cmd_key_f7		  ; FG_ACT.CODE=FG_LST.CODE    ;! Function Key
    CASE KeyCode=cmd_key_f8		  ; FG_ACT.CODE=FG_SEARCH.CODE ;! Function Key
    CASE KeyCode=cmd_key_f9		  ; FG_ACT.CODE=FG_INSERT.CODE ;! Function Key
    CASE KeyCode=cmd_key_f10		  ; FG_ACT.CODE=FG_TCL.CODE   ;! Function Key

    CASE KeyCode=cmd_key_f11		  ; FG_ACT.CODE=FG_CASE.CODE  ;! Function key
    CASE KeyCode=cmd_key_f12		  ; FG_ACT.CODE=FG_L.CASE.CODE          ;! Function key
    CASE KeyCode=cmd_key_f13		  ; FG_ACT.CODE=FG_SEARCH.CODE          ;! Function key
    CASE KeyCode=cmd_key_f14		  ; FG_ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f15		  ; FG_ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f16		  ; FG_ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f17		  ; FG_ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f18		  ; FG_ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f19		  ; FG_ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f20		  ; FG_ACT.CODE=0   ;! Function key
*
    CASE KeyCode=cmd_key_f21		  ; FG_ACT.CODE=0   ;! Function key shifted
    CASE KeyCode=cmd_key_f22		  ; FG_ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f23		  ; FG_ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f24		  ; FG_ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f25		  ; FG_ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f26		  ; FG_ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f27		  ; FG_ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f28		  ; FG_ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f29		  ; FG_ACT.CODE=0   ;! Function key
    CASE KeyCode=cmd_key_f30		  ; FG_ACT.CODE=FG_LMOUSE.CODE ;! Function key
    CASE KeyCode=cmd_key_f31		  ; FG_ACT.CODE=FG_ADD.CODE   ;! Function key
    CASE KeyCode=cmd_key_f32		  ; FG_ACT.CODE=FG_APP.CODE
*
    CASE KeyCode=cmd_error                  ; FG_ACT.CODE=FG_UNDEL.CODE         ;! Unknown command string
    CASE KeyCode=cmd_alpha_numeric          ; FG_ACT.CODE=0 ;! Simple alpha-numeric
    CASE KeyCode=cmd_timeout                ; FG_TIMEDOUT=1 ;! The input timed out
    CASE KeyCode=cmd_winsize                ; FG_ACT.CODE=0 ;! Size of the 0 window changed.
    CASE 1
        CRT @(-1):KeyCode
        CRT OCONV(CHR,'MX')
        STOP
    END CASE
    RETURN
