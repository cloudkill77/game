cld
new:
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
lda $1
cmp #$6
beq new
jmp start
