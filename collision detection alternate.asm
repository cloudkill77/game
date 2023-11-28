;; works - tested 27 November 2023
define y1 $0        ; y1 coordinate
define x1 $1        ; x1 coordinate
define y2 $10       ; y2 coordinate
define x2 $11       ; x2 coordinate
define yd $20       ; y-delta / distance
define xd $21       ; x-delta / distance

lda #5
sta y1 ; (4-220) player 1 field boundaries y-pos
lda #9
sta x1 ; (4-251) player 1 field boundaries x-pos
lda #9
sta y2 ; (0-255) enemy 1 field boundaries y-pos
lda #5
sta x2 ; (0-255) enemy 1 field boundaries x-pos

checky:
lda y1
cmp y2  
beq ysequal ; check and branch if y1 is equal to y2
bcc y1small ; check and branch if y1 is smaller than y2
bcs y2small ; check and branch if y2 is smaller than y1
error:
brk

ysequal:
lda #$1
sta ysequal ; record y1=y2
jmp checkx 

y1small:
lda #$1
sta y1small ; record y1<y2
jmp checkx 

y2small:
lda #$1
sta y2small ; record y2<y1
jmp checkx 

checkx:
lda x1
cmp x2
beq xsequal ; check and branch if x1 is equal to x2
bcc x1small ; check and branch if x1 is smaller than x2
bcs x2small ; check and branch if x2 is smaller than x1
error:
brk

xsequal:
lda #$1
sta xsequal ; record x1=x2
jmp check1end

x1small:
lda #$1
sta x1small ; record x1<x2
jmp check1end

x2small:
lda #$1
sta x2small ; record x2<x1
jmp check1end

check1end:
lda y1small
cmp #$1
beq y2y1 ; branch to y2-y1 calculation

lda y2small
cmp #$1
beq y1y2 ; branch to y1-y2 calculation

lda ysequal
cmp #$1
beq ynop
error2:
brk

y2y1:
lda y2
sbc y1
sta yd ; record y-delta
jmp calcx

y1y2:
lda y1
sbc y2
sta yd ; record y-delta
jmp calcx

ynop:
lda #$0 
sta yd ; record y-delta of 0
jmp calcx

calcx:
lda x1small
cmp #$1
beq x2x1 ; branch to x2-x1 calculation

lda x2small
cmp #$1
beq x1x2 ; branch to x1-x2 calculation

lda xsequal
cmp #$1
beq xnop
error3:
brk

x2x1:
lda x2
sbc x1
sta xd ; store x-delta
jmp end3

x1x2:
lda x1
sbc x2
sta xd ; store x-delta
jmp end3 

xnop:
lda #$0
sta xd ; store x-delta of 0

end3:
brk

