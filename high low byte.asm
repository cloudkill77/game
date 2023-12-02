cld
lda #$00   ; low byte
sta $0
lda #$02   ; high byte
sta $1

start:
lda #$5
sta ($0,x)
inc $0
lda $0
cmp #$00
beq next
jmp start

next:
inc $1
brk
jmp start
