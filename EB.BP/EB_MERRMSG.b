    SUBROUTINE EB_MERRMSG(MSG.PRE,MSG,MSG.POST,RESP,VALID.RESP)
* @(#) EB_MERRMSG.b Ported to jBASE 07:23:52  18 FEB 2010
    INCLUDE EB.EQUS EB.COMMONS
    GO MAIN$
!
!=============================================
! GALA 4GL
!
! Copyright (C) GENERAL AUTOMATION AUSTRALASIA Pty. Ltd.
!
! Written by Peter Falson - March 1989
!
!=========== Program's Purpose ===============
!
! Display error message
! If MSG.PRE<2> is not null then a bitmap is displayed
!  CASE BITMAP=1; BITMAP='INFO'
!  CASE BITMAP=2; BITMAP='QUES'
!  CASE BITMAP=3; BITMAP='EXCL'
!  CASE BITMAP=4; BITMAP='STOP'
!  CASE 1       ; bitmap name has been explicitly named
!
!=============================================
!
    INCLUDE EB.EQUS STD.EQUS
    INCLUDE EB.EQUS SCR.PARAMS
    INCLUDE EB.EQUS CRT.PARAMS
    INCLUDE EB.EQUS OTHER.PARAMS
    INCLUDE EB.EQUS SCREEN.PARAMS
    INCLUDE EB.EQUS COLOURS
    EQU SQ TO "'", DQ TO '"'
    EQU MAX TO 9999
    MAIN$:!
!
    DEFC INT JBASEEmulateGETINT(INT, INT)
    IF_COMPILED_PRIME=JBASEEmulateGETINT(30,2)
    am_start=IF_COMPILED_PRIME
    vm_start=IF_COMPILED_PRIME
    CRLF=CR:LF
    WINTEGRATE=FG$STERM=3
    GUI=FG$STERM=6
    INCLUDE EB.EQUS WIN.ARGS
    IF UNASSIGNED(RESP) THEN RESP=''
    IF RESP='' THEN RESP=VALID.RESP<2>
    VALID.RESP=VALID.RESP<1>
    IF RESP#'' AND VALID.RESP#'' THEN
        LOCATE RESP IN VALID.RESP<1,vm_start> SETTING VPOS ELSE RESP=''
    END
    IF MSG[1,2]='E$' OR MSG[1,2]='D$' THEN CALL EB_BLD_MSG(MSG)
    IF GUI THEN
        BEGIN CASE
        CASE VALID.RESP='Y':VM:'N'
            VALID.RESP=4
        CASE VALID.RESP='Y':VM:'N':VM:'C'
            VALID.RESP=3
        CASE VALID.RESP='OK'
            VALID.RESP=0
        CASE VALID.RESP=''
        END CASE
        IF VALID.RESP='' THEN MSG.TYPE='M' ELSE MSG.TYPE='m'
        MSG=SWAP(MSG,SVM,CRLF)
        CALL EB_GUI_SEND(MSG.TYPE,BELL:MSG:AM:VALID.RESP)
        CALL EB_GUI_SEND('r','')
        ECHO OFF
        LOOP
            INPUT RESP
            RESP=FIELD(FIELD(RESP,STX,2),';',2)
        UNTIL NUM(RESP) OR VALID.RESP='' DO REPEAT
        ECHO ON
        IF VALID.RESP#'' THEN CONVERT 7654321 TO 'NYITACO' IN RESP
        RETURN
    END
    IF FG$STERM THEN
        CONVERT BELL TO '' IN MSG
        MSG=SWAP(MSG,DQ,'":':SQ:DQ:SQ:':"')
        IF WINTEGRATE THEN
            CONVERT SVM TO AM IN MSG
            BEGIN CASE
            CASE VALID.RESP='Y':VM:'N'
                CALL EB_WIN_MSGBOX(MSG,'','MB_YESNO|MB_ICONEXCLAMAT|MB_DEFBUTTON1',RESP)
                RESP=RESP[1,1]
            CASE VALID.RESP='Y':VM:'N':VM:'C'
                CALL EB_WIN_MSGBOX(MSG<1,1>,'','MB_YESNOCANCEL|MB_ICONEXCLAMAT|MB_DEFBUTTON1',RESP)
                RESP=RESP[1,1]
            CASE VALID.RESP='OK'
                SCRIPT='MessageBox "':MSG:'"'
                CALL EB_WIN_COMSUB(SCRIPT)
                RESP=''
            CASE VALID.RESP=''
                LABEL=MSG<1,1>
                LENTH=40
                BOX.WIDTH=(LEN(LABEL)+LENTH)*4+30
                BOX.DEPTH=40
                FONT.SIZE=8
                FONT.TYPE='helv'
                COL.ADJ=4.17
                ROW.ADJ=9
                INP.HGT=9
                ROW.OFF=6
                CALL EB_WIN_WP(RESP,LABEL,100,100,70,20,'U',40,'L','Message',MAT WIN.ARGS)
                RETURN
            CASE 1
                NBR.VALS=DCOUNT(VALID.RESP,VM)
                VALID=TRUE
                BUTTONS=''
                DEF.BUTTON=''
                ICON=''
                FOR V=1 TO NBR.VALS WHILE VALID
                    RESP=VALID.RESP<1,V>[1,1]
                    OPT='(':RESP:')'
                    DFLT='<':RESP:'>'
                    OPOS=INDEX(MSG,OPT,1)
                    IF OPOS THEN
                        VPOS=OPOS
                        DPOS=FALSE
                    END ELSE
                        DPOS=INDEX(MSG,DFLT,1)
                        IF DPOS THEN VPOS=DPOS ELSE VPOS=FALSE
                    END
                    IF VPOS THEN
                        SOP=VPOS+3
                        LOOP
                            CHR=MSG[SOP,1]
                        WHILE TRIM(OCONV(CHR,'MCA'))#'' DO SOP+=1 REPEAT
                        BUTTON=MSG[VPOS,SOP-VPOS]
                        CONVERT '<>()' TO '' IN BUTTON
                        BEGIN CASE
                        CASE CHR=','; SOP+=1
                        CASE CHR='/'; SOP+=1
                        CASE MSG[SOP,2]=', '; SOP+=2
                        CASE MSG[SOP,4]=' or '; SOP+=4
                        END CASE
                        MSG=MSG[1,VPOS-1]:MSG[SOP,MAX]
                        BUTTONS<1,-1>='&':BUTTON
                        IF DPOS THEN DEF.BUTTON=OCONV(BUTTON,'MCU')
                    END ELSE VALID=''
                NEXT V
                IF VALID THEN
                    CALL EB_MESSAGE_BOX(0,MSG<1,1>,BUTTONS,'',DEF.BUTTON,ICON,RESP)
                END ELSE RESP=''
            END CASE
            RETURN
        END
        INCLUDE EB.INCLUDES MORE.CHARS
        IF MORE.CHARS THEN
            IF MORE.CHARS=1 AND VALID.RESP#'' THEN
                ECHO OFF
                INPUT RESP,1:
                ECHO ON
                IF RESP#'' THEN
                    LOCATE RESP IN VALID.RESP<1,vm_start> SETTING OK THEN RETURN
                END
            END ELSE
                INCLUDE EB.INCLUDES REMOVE.TYPEAHEAD
            END
        END
        LOCATE RESP IN VALID.RESP<1,vm_start> SETTING DPOS ELSE DPOS=DCOUNT(VALID.RESP,VM)
        MSG=SWAP(MSG,SVM,'|M')
        BEGIN CASE
        CASE VALID.RESP='Y':VM:'N'
            CALL EB_AT_MESSAGE.BOX(5,4,DPOS,'',MSG<1,1>,'','|M',RESP)
            CONVERT 64 TO 'YN' IN RESP
        CASE VALID.RESP='Y':VM:'N':VM:'C'
            CALL EB_AT_MESSAGE.BOX(6,4,DPOS,'',MSG<1,1>,'','|M',RESP)
            CONVERT 640 TO 'YNC' IN RESP
        CASE VALID.RESP='OK'
            CALL EB_AT_MESSAGE.BOX(1,4,3,'',MSG<1,1>,'','|M',RESP)
            RESP=''
            RETURN
        CASE VALID.RESP=''
            CALL EB_AT_WINDOWS.INPUT(300,0,5,0,'','|M','',RESP:'',MSG,10,10,RESP)
            RETURN
        CASE 1
            NBR.VALS=DCOUNT(VALID.RESP,VM)
            VALID=TRUE
            BUTTONS=''
            DEF.BUTTON=''
            ICON=''
            FOR V=1 TO NBR.VALS WHILE VALID
                RESP=VALID.RESP<1,V>[1,1]
                OPT='(':RESP:')'
                DFLT='<':RESP:'>'
                OPOS=INDEX(MSG,OPT,1)
                IF OPOS THEN
                    VPOS=OPOS
                    DPOS=FALSE
                END ELSE
                    DPOS=INDEX(MSG,DFLT,1)
                    IF DPOS THEN VPOS=DPOS ELSE VPOS=FALSE
                END
                IF VPOS THEN
                    SOP=VPOS+3
                    LOOP
                        CHR=MSG[SOP,1]
                    WHILE TRIM(OCONV(CHR,'MCA'))#'' DO SOP+=1 REPEAT
                    BUTTON=MSG[VPOS,SOP-VPOS]
                    CONVERT '<>()' TO '' IN BUTTON
                    BEGIN CASE
                    CASE CHR=','; SOP+=1
                    CASE CHR='/'; SOP+=1
                    CASE MSG[SOP,2]=', '; SOP+=2
                    CASE MSG[SOP,4]=' or '; SOP+=4
                    END CASE
                    MSG=MSG[1,VPOS-1]:MSG[SOP,MAX]
                    BUTTONS<1,-1>='&':BUTTON
                    IF DPOS THEN DEF.BUTTON=OCONV(BUTTON,'MCU')
                END ELSE VALID=''
            NEXT V
            IF VALID THEN
                CALL EB_MESSAGE_BOX(0,MSG<1,1>,BUTTONS,'',DEF.BUTTON,ICON,RESP)
            END ELSE RESP=''
        END CASE
        IF RESP#'' THEN RETURN
    END
    CONVERT VM TO '' IN MSG
    IF NOT(INDEX(CLEOL,TRIM(MSG),1)) THEN
        IF FG$POPUP AND FG$STERM THEN
            CC=0; RR=FG$SCR.CO.ORDS<1,4>
            PRFX=STATUS.LINE<1,1>
            CRT PRFX:MSG:STATUS.LINE<1,2>:
        END ELSE
            CC=LEN(MSG)+1; RR=23
            FLD.COLOUR=FG$COLOURS<1,2,1>
            BACK.COLOUR=FG$COLOURS<1,2,2>
!
            SAVE.COLOURS=FG$CURR.COLOURS:AM:FG$PREV.COLOURS
            CRT @(0,23):FG:CLEOL:
            IF WHITE<1,1>#'' THEN CALL EB_CH_COLOUR(FLD.COLOUR,BACK.COLOUR)
            FG$CURR.COLOURS=SAVE.COLOURS<1>; FG$PREV.COLOURS=SAVE.COLOURS<2>
!
            CRT @(0,23):FG:MSG.PRE:MSG:MSG.POST:
        END
        IF VALID.RESP='' THEN L=0 ELSE
            IF VALID.RESP='OK' THEN VALID.RESP=''
            NBR.RESP=DCOUNT(VALID.RESP,VM)
            L=1
            FOR I=1 TO NBR.RESP WHILE L=1
                L=LEN(VALID.RESP<1,I>)
            NEXT I
        END
        RESP.CHK=VALID.RESP
        CONVERT VM TO '' IN RESP.CHK
        BEGIN CASE
        CASE VALID.RESP=''; INP.TYPE='AN'
        CASE VALID.RESP=OCONV(RESP.CHK,'MCU'); INP.TYPE='U'
        CASE VALID.RESP=OCONV(RESP.CHK,'MCN'); INP.TYPE='N'
        CASE 1; INP.TYPE='AN'
        END CASE
        LOOP
            IF L=1 THEN
                CALL EB_INPUT_ZERO(RESP,CC,RR,FG$INPUT.CODES,VALID.RESP:AM:AM:RESP,1)
            END ELSE
                CALL EB_WP(RESP,INP.TYPE,CC,RR,10,0)
            END
        UNTIL INDEX(VALID.RESP,RESP,1) OR VALID.RESP='' OR FG$ACT.CODE DO REPEAT
        IF FG$ACT.CODE THEN RESP=''
        FG$ACT.CODE=''
        IF FG$POPUP AND FG$STERM THEN
            PRFX=STATUS.LINE<1,1>
            CRT PRFX:STATUS.LINE<1,2>:
        END ELSE CRT @(0,RR):CLEOL:
        CRT FG$CURR.COLOURS:
    END
!
    RETURN
END
