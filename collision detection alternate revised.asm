define y1 $0
define x1 $1
define y2 $10
define x2 $11
define yd $20
define xd $21


store:
lda #1
sta y1 ; (4-236)
lda #$e0
sta x1 ; (4-251)
lda #2
sta y2 ; (0-240)
lda #7
sta x2 ; (0-255)

lda y1
cmp y2  
beq ysequal
bcc y1small
bcs y2small
error0:
brk

ysequal:
lda #$1
sta ysequal
jmp calc_x 

y1small:
lda #$1
sta y1small
jmp calc_x 

y2small:
lda #$1
sta y2small
jmp calc_x 

calc_x:
lda x1
cmp x2
beq xsequal
bcc x1small
bcs x2small
error1:
brk

xsequal:
lda #$1
sta xsequal
jmp end

x1small:
lda #$1
sta x1small
jmp end

x2small:
lda #$1
sta x2small
jmp end

end:
lda y1small
cmp #$1
beq y2y1

lda y2small
cmp #$1
beq y1y2

lda ysequal 
cmp #$1
beq ynop
error2:
brk

y2y1:
sec
lda y2
sbc y1
sta yd
jmp end2

y1y2:
sec
lda y1
sbc y2
sta yd
jmp end2 

ynop:
lda #$0
sta yd
jmp end2

end2:
lda x1small
cmp #$1
beq x2x1

lda x2small
cmp #$1
beq x1x2

lda xsequal 
cmp #$1
beq xnop
error3:
brk

x2x1:
sec
lda x2
sbc x1
sta xd
jmp end3

x1x2:
sec
lda x1
sbc x2
sta xd
jmp end3 

xnop:
lda #$0
sta xd
jmp end3

end3:
brk

