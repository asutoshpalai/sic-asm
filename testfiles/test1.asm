       START 1000
       LDA FOUR
       STCH DATA
INLOOP TD INDEV
       JEQ INLOOP
       RD INDEV
       STCH DATA

OUTLP  TD OUTDEV
       JEQ OUTLP
       LDCH DATA
       WD OUTDEV

       LDA FOUR
       MUL NINE
       STA DEST

INDEV  BYTE X'F1'
OUTDEV BYTE X'05'
DATA   RESB 1
DEST   RESW 1
FOUR   WORD 4
NINE   WORD 9
       END INLOOP