lda #$d
sta $0
sta $10
lda #$4
sta $1
define temp $5f

loop1:
inx
sec
lda $10
sbc $1
sta $10
beq zero
cmp $1
bcc less
jmp loop1

loop2:
iny
sec
sbc $1
sta $11
beq zero2
cmp $1
bcc less2
jmp loop2

loop3:
iny
sec
sbc $1
sta $12
beq zero3
cmp $1
bcc less3
jmp loop3

zero:
stx $10
brk

zero2:
sty $11
brk

zero3:
sty $12
brk

less:
stx $10
asl ;x2
sta temp
asl ;x4
asl ;x8
clc
adc temp
sta $11
jmp loop2

less2:
sty $11
asl ; x2
sta temp
asl ;x4
asl ;x8
clc
adc temp
sta $12
ldy #$0
jmp loop3

less3:
sty $12
brk