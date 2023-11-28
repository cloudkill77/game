cld
define y1 $0      ; player y
define x1 $1      ; player x
define y2 $10     ; enemy y
define x2 $11     ; enemy x
define yd $20     ; y-delta
define xd $21     ; x-delta
define prox 9     ; collision limit

lda #$0
sta ysequal
sta y1small
sta y2small
sta xsequal
sta x1small
sta x2small


; store
lda #$90
sta y1 ; (4-236)
lda #$90
sta x1 ; (4-251)
lda #$10
sta y2 ; (0-240)
lda #$10
sta x2 ; (0-255)


lda y1
cmp y2  ; accumulator (y1) less than y2?
beq l_ysequal
bcc l_y1small  ; yes, y1 (accumulator) less than y2
bcs l_y2small  ; no, y2 is less than y1 (accumulator)
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
cmp x2 ; accumulator (x1) less than x2?
beq l_xsequal
bcc l_x1small ; yes, x1 (accumulator) less than x2
bcs l_x2small ; no, x2 is less than x1 (accumulator)
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
cmp prox  ; accumulator less than prox?
bcs clear ; no, accumulator is not less than prox

lda xd
cmp prox  ; accumulator less than prox?
bcs clear ; no, accumulator is not less than prox

collision:
brk

clear:
brk