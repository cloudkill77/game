;; works - tested 27 November 2023
define y1 $0               ; y1 coordinate
define x1 $1               ; x1 coordinate
define y2 $10              ; y2 coordinate
define x2 $11              ; x2 coordinate
define ycollision $50      ; records detected y-collision
define xcollision $51      ; records detected x-collision
define p 3                 ; proximity limit for collision 

store:
lda #$4
sta y1 ; (4-220) player 1 field boundaries y-pos
lda #$4
sta x1 ; (4-251) player 1 field boundaries x-pos
lda #$6
sta y2 ; (0-255) enemy 1 field boundaries y-pos
lda #$6
sta x2 ; (0-255) enemy 1 field boundaries x-pos

calc_y:
lda y1
cmp y2
beq ylessp ; check and branch if y1 is equal to y2

sec
lda y1
sbc y2
sta $20
cmp #p
bcc ylessp ; check and branch if y1-y2 is less than p

sec
lda y2
sbc y1
sta $30
cmp #p
bcc ylessp ; check and branch if y2-y1 is less than p

jmp calc_x ; perform x calc regardless if no collision on y

ylessp:
lda #$1
sta ycollision ; "collision" on y-plane

calc_x:
lda x1
cmp x2
beq xlessp ; check and branch if x1 is equal to x2

sec
lda x1
sbc x2
sta $21
cmp #p
bcc xlessp ; check and branch if x1-x2 is less than p

sec
lda x2
sbc x1
sta $31
cmp #p
bcc xlessp ; check and branch if x2-x1 is less than p

jmp end

xlessp:
lda #$1
sta xcollision ; "collision" on x-plane

end:
clc
lda ycollision
adc xcollision
cmp #$2
beq collision ; collision on both y and x-plane

nocollision:
brk

collision:
brk
