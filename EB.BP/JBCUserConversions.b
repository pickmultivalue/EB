    SUBROUTINE JBCUserConversions(result, source, code, type, error)
* Changed by PORTBAS ->     SUBROUTINE JBCUserConversions (result, source, code, type, error)
* @(#) JBCUserConversion.b Ported to jBASE 07:23:52  18 FEB 2010
    BEGIN CASE
        CASE code MATCHES "'VDOTL#'0N" OR code MATCHES "'VDOTR#'0N" OR code MATCHES "'VDOTC#'0N"
            DispLen = OCONV(code,'MCN')
            result = DispLen
            BEGIN CASE
                CASE DispLen = 0
                    result = ''
                CASE LEN(source) LE DispLen
                    result = source
                CASE code[1,5] = 'VDOTL'
                    result = source[1,DispLen-3]:'...'
                CASE code[1,5] = 'VDOTR'
                    result = '...':source[-(DispLen-3),-1]
                CASE code[1,5] = 'VDOTC'
                    FrontLen = OCONV((DispLen - 3) / 2 + .3,'MR0')
                    BackLen = OCONV((DispLen - 3) / 2 - .3, 'MR0')
                    result = source[1,FrontLen]:'...':source[-BackLen,-1]
            END CASE
        CASE code = 'ESC'
            EscTable = ''
            EscTable<27> = '<ESC>'
            l = LEN(source)
            result = ''
            FOR i = 1 TO l
                c = source[i,1]
                s = SEQ(c)
                IF s < 32 THEN
                    escval = EscTable<s>
                    IF LEN(escval) = 0 THEN
                        IF s > 0 AND s < 27 THEN
                            escval = 'ctrl-[':CHAR(64+s):']'
                        END ELSE escval = 'char(':s:')'
                    END
                END ELSE
                    escval = c
                END
                result := escval
            NEXT i
    END CASE
    RETURN
