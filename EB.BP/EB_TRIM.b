    SUBROUTINE EB_TRIM(NEW,OLD,SPC,TYPE)
* @(#) EB_TRIM.b Ported to jBASE 07:23:52  18 FEB 2010
* @(#) EB.TRIM Ported to jBASE 16:15:16  27 JUL 2001
! Trim trailing blanks
    LENTH=LEN(OLD)
    BEGIN CASE
    CASE TYPE='T'
        LOOP WHILE OLD[LENTH,1]=SPC AND LENTH DO LENTH-=1 REPEAT
        NEW=OLD[1,LENTH]
    CASE TYPE='L'
        I=1
        LOOP UNTIL OLD[I,1]#SPC DO I+=1 REPEAT
        NEW=OLD[I,LENTH]
    CASE TYPE='L'
        NEW=OLD; CONVERT SPC TO '' IN NEW
    END CASE
    RETURN
END