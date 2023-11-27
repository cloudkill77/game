define y1 $0
define x1 $1
define y2 $10
define x2 $11
define ycollision $50
define xcollision $51
sec

store:
lda #$4
sta y1 ; (4-220)
lda #$1
sta x1 ; (4-251)
lda #$1
sta y2 ; (0-255)
lda #$1
sta x2 ; (0-255)

calc_y:
lda y1
sbc y2
sta $20
cmp #$2
bcc yless2

lda y2
sbc y1
sta $30
cmp #$2
bcc yless2

lda y1
cmp y2
beq yless2 

jmp calc_x ; perform x calc regardless if no collision on y

yless2:
lda #$1
sta ycollision

calc_x:
lda x1
sbc x2
sta $21
cmp #$2
bcc xless2

lda x2
sbc x1
sta $31
cmp #$2
bcc xless2

lda x1
cmp x2
beq xless2

jmp end

xless2:
lda #$1
sta xcollision

end:
clc
lda ycollision
adc xcollision
cmp #$2
beq collision

nocollision:
brk

collision:
brk
