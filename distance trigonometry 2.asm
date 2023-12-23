; calculate distance between two coordinates using pythagoras theorem
; manually set y and x-positions are subtracted from each other to get the yd and xd (y-delta and x-delta). these are then processed using pythagoras' theorem to get the length of the hypotenuse (ie the distance between the two points). currently p1y and p1x need to be smaller than e1y and e1x for the calculation to work.

define p1y $00
define p1x $01
define e1y $10
define e1x $11
define i1y $20
define i1x $21
define squarey $30
define squarex $31
define tempy $40
define tempx $41

lda #$44
sta p1y
lda #$43
sta p1x
lda #$47
sta e1y
lda #$47
sta e1x

; load p1x
; load e1x
; load p1y
; load e1y

lda p1y
cmp e1y
bcs p1ylarger

e1ylarger:
lda e1y
sec
sbc p1y
sta i1y
jmp yend

p1ylarger:
lda p1y
sec
sbc e1y
sta i1y

yend:

lda p1x
cmp e1x
bcs p1xlarger

e1xlarger:
lda e1x
sec
sbc p1x
sta i1x
jmp xend

p1xlarger:
lda p1x
sec
sbc e1x
sta i1x

xend:
; square i1y
; square i1x


clc
ldx #$00
loopy:
lda $30    ; load result
adc $20   ; add factor 1
sta $30    ; store as result
inx            ; increment x
cpx $20       ; compare x against factor 2
bne loopy   ; loop if x is less than factor 2

clc
ldx #$00
loopx:
lda $31
adc $21
sta $31
inx
cpx $21
bne loopx

; a^2 + b^2 = c^2

clc
lda $30
adc $31
sta $32

; √(c^2)

define ROOT $00
define REM $01
define NUMH $11
define NUML $10

lda #$19
sta NUML


LDA #$00
STA ROOT
STA REM
LDX #$8
L1:
SEC
LDA NUMH
SBC #$40
TAY
LDA REM
SBC ROOT
BCC L2
STY NUMH
STA REM
L2:
ROL ROOT
ASL NUML
ROL NUMH
ROL REM
ASL NUML
ROL NUMH
ROL REM
DEX
BNE L1

brk

