define y1 $0
define x1 $1
define y2 $10
define x2 $11
define ycollision $50
define xcollision $51

store:
lda #$1
sta y1 ; (1-224)
lda #$1
sta x1 ; (1-255)
lda #$2
sta y2 ; (1-224)
lda #$2
sta x2 ; (1-255)

calc_y:
clc
lda y1
sbc y2
sta $20
cmp #$2
bcc yless2

clc
lda y2
sbc y1
sta $30
cmp #$2
bcc yless2

clc
lda y1
cmp y2
beq yless2 

jmp calc_x

yless2:
lda #$1
sta ycollision

calc_x:
clc
lda x1
sbc x2
sta $21
cmp #$2
bcc xless2

clc
lda x2
sbc x1
sta $31
cmp #$2
bcc xless2

clc
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
