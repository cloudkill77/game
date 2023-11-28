define y1 $0      ; player y
define x1 $1      ; player x
define y2 $10     ; enemy y
define x2 $11     ; enemy x
define yd $20     ; y-delta
define xd $21     ; x-delta


ysequal .rs 1
y1small .rs 1
y2small .rs 1
xsequal .rs 1
x1small .rs 1
x2small .rs 1
p .rs 1         ; collision limit


lda #$5
sta p           ; store collision limit


; store
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
beq l_ysequal
bcc l_y1small
bcs l_y2small
error0:
brk

l_ysequal:
lda #$1
sta ysequal
jmp calc_x 

l_y1small:
lda #$1
sta y1small
jmp calc_x 

l_y2small:
lda #$1
sta y2small
jmp calc_x 

calc_x:
lda x1
cmp x2
beq l_xsequal
bcc l_x1small
bcs l_x2small
error1:
brk

l_xsequal:
lda #$1
sta xsequal
jmp end

l_x1small:
lda #$1
sta x1small
jmp end

l_x2small:
lda #$1
sta x2small
jmp end

end:
lda y1small
cmp #$1
beq l_y2y1

lda y2small
cmp #$1
beq l_y1y2

lda ysequal 
cmp #$1
beq l_ynop
error2:
brk

l_y2y1:
sec
lda y2
sbc y1
sta yd
jmp end2

l_y1y2:
sec
lda y1
sbc y2
sta yd
jmp end2 

l_ynop:
lda #$0
sta yd
jmp end2

end2:
lda x1small
cmp #$1
beq l_x2x1

lda x2small
cmp #$1
beq l_x1x2

lda xsequal 
cmp #$1
beq l_xnop
error3:
brk

l_x2x1:
sec
lda x2
sbc x1
sta xd
jmp end3

l_x1x2:
sec
lda x1
sbc x2
sta xd
jmp end3 

l_xnop:
lda #$0
sta xd
jmp end3

end3:
lda yd
cmp p
bcc clear

lda xd
cmp p
bcc clear

collision:
brk

clear:
brk