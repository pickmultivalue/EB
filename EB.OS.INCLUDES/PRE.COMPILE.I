!
! SAVE SOURCE
!
  IF INDEX(NEW.PROGRAM,'@CALL',1) THEN CONV=1 ELSE CONV=0
  IF CONV OR ENCRYPTED THEN
    CRT 'Converting...':
    WRITE PGM ON F.BP,'%':PROG:'%'
!
    LOOP
      LNBR=INDEX(NEW.PROGRAM,'@CALL',1)
    WHILE LNBR DO
      LNBR=DCOUNT(NEW.PROGRAM[1,LNBR],CHAR(254))
      LINE=NEW.PROGRAM<LNBR>
      LOOP
        POS=INDEX(LINE,'@CALL',1)
      WHILE POS DO
        PRIOR=LINE[1,POS-1]; LINE=LINE[POS+6,999]
        CNT=1
        LOOP
          SOP=INDEX(LINE,')',CNT)
          AFTER=LINE[1,SOP]
          OLD="'(')"; GOSUB REMOVE.IT
          OLD="'(',"; GOSUB REMOVE.IT
          OLD="')')"; GOSUB REMOVE.IT
          OLD="')',"; GOSUB REMOVE.IT
        UNTIL COUNT(AFTER,'(')=COUNT(AFTER,')') AND LINE[SOP+1,1]#"'" DO CNT=CNT+1 REPEAT
        AFTER=LINE[SOP+1,999]; LINE=LINE[1,SOP]
        STMT=FIELD(LINE,'(',1); LINE=LINE[COL2()+1,999]
        W=1; WORD=LINE; GOSUB GET.WORD
        MVAR=WORD
        OTHER.WORDS=LINE[CNT,999]
        OTHER.WORDS=OTHER.WORDS[1,LEN(OTHER.WORDS)-1]
        W=1; WORD=OTHER.WORDS; GOSUB GET.WORD
        WORD.TWO=WORD
        W=2; WORD=OTHER.WORDS; GOSUB GET.WORD
        WORD.THREE=WORD
        W=3; WORD=OTHER.WORDS; GOSUB GET.WORD
        WORD.FOUR=WORD
        W=4; WORD=OTHER.WORDS; GOSUB GET.WORD
        WORD.FIVE=WORD
        IF STMT='SSORT' THEN STMT='SORT' ;! GALA 4 fix
        BEGIN CASE
          CASE STMT='ASSIGNED'
            STMT='NOT(UNASSIGNED(':MVAR:'))'
          CASE STMT='CLOSE'
            STMT='* CLOSE ':MVAR
          CASE STMT='SWAP' AND 0
            IF WORD.TWO='' THEN OTHER.WORDS=MVAR:',':OTHER.WORDS
            IF WORD.FOUR#'' THEN
!              STMT=MVAR:'=':WORD.TWO:'; CHANGE ':WORD.THREE:' TO ':WORD.FOUR:' IN ':MVAR
              STMT=MVAR:'=SWAP(':WORD.TWO:',':WORD.THREE:',':WORD.FOUR:')'
            END ELSE
              STMT='CHANGE ':WORD.TWO:' TO ':WORD.THREE:' IN ':MVAR
            END
          CASE STMT='CONVERT' OR STMT='SWAP'
            IF WORD.TWO='' THEN OTHER.WORDS=MVAR:',':OTHER.WORDS
            IF WORD.FOUR#'' THEN
              STMT=MVAR:'=':STMT:'(':OTHER.WORDS:')'
            END ELSE
              STMT=MVAR:'=':STMT:'(':MVAR:',':OTHER.WORDS:')'
            END
!          CASE STMT='EREPLACE'
!            STMT=MVAR:'=EREPLACE(':MVAR:',':OTHER.WORDS:')'
          CASE STMT='EXECUTE'
            STMT=STMT:LINE
          CASE STMT='FOLD'
            IF WORD.THREE='' THEN
              WORD.THREE=WORD.TWO
              WORD.TWO=''
            END
            IF WORD.TWO='' THEN WORD.TWO=MVAR
            IF WORD.FOUR='' THEN WORD.FOUR='AM'
!            STMT='CALL GALA.FOLD(':WORD.TWO:',':WORD.THREE:')'
            STMT=WORD.TWO:'=FOLD(':WORD.TWO:',':WORD.THREE:')'
            IF WORD.TWO#MVAR AND WORD.FOUR='AM' THEN
              STMT:='; ':MVAR:'=':WORD.TWO
            END ELSE
              IF WORD.FOUR#'AM' THEN STMT:='; ':MVAR:'=CONVERT(':WORD.TWO:',AM,':WORD.FOUR:')'
            END
          CASE STMT='GROUP'
            STMT=MVAR:'=FIELD(':WORD.TWO:',':WORD.THREE:',':WORD.FOUR:',':WORD.FIVE:')'
          CASE STMT='INPUT.ZERO'
            STMT='INPUT ':MVAR:',0:'
          CASE STMT='MATBUILD' OR STMT='MATPARSE'
            SVAR=WORD.THREE
            IF STMT='MATPARSE' THEN
              IF WORD.FOUR='' THEN WORD.FOUR='SAC$TEMP$VAR'
            END ELSE
!              IF WORD.FOUR#'' AND WORD.FIVE#'' THEN
!                WORD.TWO:=',':WORD.FOUR:',':WORD.FIVE
!              END
            END
            IF STMT='MATPARSE' AND WORD.FOUR#'' THEN WORD.THREE:=' SETTING ':WORD.FOUR
            STMT=STMT:' ':MVAR:' FROM ':WORD.TWO:' USING ':WORD.THREE
*                R91 bug. MATPARSE sometimes fails on first attempt
            IF FIELD(STMT,' ',1)='MATPARSE' THEN
              STMT=STMT:'; IF ':WORD.FOUR:' # DCOUNT(':WORD.TWO:',':SVAR:') THEN ':STMT
            END
          CASE STMT='SWAP.ASSIGN'
            STMT=MVAR:'=':WORD.TWO:'; CONVERT ':WORD.THREE:' TO ':WORD.FOUR:' IN ':MVAR
          CASE STMT='SOUNDEX'
            IF WORD.TWO='' THEN WORD.TWO=MVAR
            STMT=FIELD(MVAR,')',1):'=':STMT:'(':WORD.TWO:')'
          CASE STMT='SUMMATION' OR STMT='SUM'
            IF WORD.TWO='' THEN WORD.TWO=MVAR
            STMT='CALL GALA.SUMMATION(':FIELD(MVAR,')',1):',':WORD.TWO:')'
          CASE STMT[1,4]='TRIM'
            IF WORD.TWO='' THEN WORD.TWO=MVAR
            IF WORD.THREE='' THEN WORD.THREE="' '"
            STMT=MVAR:'=TRIM(':WORD.TWO:',':WORD.THREE:',"':STMT[5,1]:'")'
          CASE STMT='UNASSIGNED'
            STMT='UNASSIGNED(':MVAR:')'
          CASE 1
            CHANGE ',,' TO ',':MVAR:":''" IN LINE
            STMT='CALL GALA.':STMT:'(':LINE
        END CASE
        LINE=PRIOR:STMT:AFTER
      REPEAT
      NEW.PROGRAM<LNBR>=LINE
    REPEAT
    BREAK OFF
    WRITE NEW.PROGRAM ON F.BP,PROG
    EXECUTE 'PORTBAS ':BP.FILE:' ':PROG:' (OH' CAPTURING OUTPUT
    CRT
  END
