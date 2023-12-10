define fpy $0
define fpx $1

lda #$50
sta fpy
lda #$ff
sta fpx
ldx #$00
loop1:
dec fpx
inc fpy
inx
cpx #$8
bne loop1
loop2:
dec fpx
dec fpy
dex
bne loop2
jmp loop1


