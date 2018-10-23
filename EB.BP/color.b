    EQU ESC TO CHAR(27)
    SGR1=ESC:'[#;'
    SGR2=';#m'
    LOOP
        INPUT ANS
    UNTIL ANS = '' DO
    CRT SGR1:CHAR(ANS):SGR2:'test'
    REPEAT
