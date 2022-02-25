! PROGRAM EBKEYS
    DIM EB_CHARS(100)
    INCLUDE EB.EQUS EB.CHARS
    cmd = SYSTEM(1000)

    OPEN 'EB.PARAMS' TO f.params ELSE STOP 201, 'EB.PARAMS'
    OPEN 'EB.EQUS' TO f.equs ELSE STOP 201, 'EB.EQUS'

    READ equs FROM f.equs, 'EB.CHARS' ELSE STOP 202, 'EB.CHARS'

    DEFFUN FNKEYTRANS()

    ttype = cmd<2>
    specific_setting = cmd<3>

    IF (specific_setting 'R#1') = '*' THEN
        specific_setting = specific_setting[1, LEN(specific_setting)-1]
        wild_card = @TRUE
    END ELSE wild_card = @FALSE

    key = 'EB.CHARS@':ttype

    MATREAD EB_CHARS FROM f.params, key THEN
        status = 'updated'
    END ELSE
        MAT EB_CHARS =''
        CRT 'New term type...';RQM
        status = 'created'
    END

    match = ''
    changed = @FALSE
    settings = ''

    loc = 0
    LOOP
        REMOVE line FROM equs AT loc SETTING delim
        setting = TRIM(FIELD(line, '!', 2))
        attr = OCONV(line, 'MCN')
        IF LEN(attr) AND LEN(setting) THEN
            settings<attr> = setting
        END
    WHILE delim DO REPEAT

    dc = DCOUNT(settings, @AM)

    FOR attr = 1 TO dc
        setting = settings<attr>
        IF LEN(setting) = 0 THEN CONTINUE
        IF wild_card THEN
            match_setting = setting[1, LEN(specific_setting)]
        END ELSE match_setting = setting

        IF NOT(LEN(specific_setting)) OR match_setting = specific_setting THEN
            chars = EB_CHARS(attr)
            save_chars = chars
            GOSUB get_key

            IF LEN(chars) > 0 THEN
                IF chars = ' ' THEN chars = ''

                FOR i = 2 TO 100
                    existing = EB_CHARS(i)
                    IF existing = chars AND i NE attr THEN
                        CRT 'Sequence already in use in ':settings<i>:' (':i:')'
                        BREAK
                    END
                NEXT i

                IF chars NE save_chars THEN
                    IF i > 100 THEN
                        changed = @TRUE
                        EB_CHARS(attr) = chars
                        CRT OCONV(chars, 'MX')
                        CRT OCONV(chars, 'MCP')
                    END
                END
            END
        END

    NEXT attr

    IF changed THEN
        len = 0
        FOR attr = 2 TO 100
            l = LEN(EB_CHARS(attr))
            IF l > len THEN len = l
        NEXT attr

        EB_CHARS(1) = len

        attr = ''
        setting = 'Update ':key
        chars = 'N'; match='Y':@VM:'N'
        GOSUB get_key

        IF OCONV(chars, 'MCU') = 'Y' THEN
            MATWRITE EB_CHARS ON f.params, key
            CRT key:' ':status
        END
    END

    STOP

get_key:
    PROMPT ''
    newchars = FNKEYTRANS(chars)
    CRT attr,setting:' (':newchars:') ? ':
    CALL EBGETKEY(match, chars)
    RETURN
