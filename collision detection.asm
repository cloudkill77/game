define y1 $0
define x1 $1
define y2 $10
define x2 $11
define ycollision $50
define xcollision $51
define p 3

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
sec
lda y1
sbc y2
sta $20
cmp #p
bcc ylessp

sec
lda y2
sbc y1
sta $30
cmp #p
bcc ylessp

lda y1
cmp y2
beq ylessp

jmp calc_x ; perform x calc regardless if no collision on y

ylessp:
lda #$1
sta ycollision

calc_x:
sec
lda x1
sbc x2
sta $21
cmp #p
bcc xlessp

sec
lda x2
sbc x1
sta $31
cmp #p
bcc xlessp

lda x1
cmp x2
beq xlessp

jmp end

xlessp:
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
