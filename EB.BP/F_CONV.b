    FUNCTION F_CONV(type)
    
    sent = SYSTEM(1000)
    
    value = sent<2>
    conv = sent<3>

    IF type = 'O' THEN
        RETURN OCONV(value, conv)
    END ELSE
        RETURN ICONV(value, conv)
    END
