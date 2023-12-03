lda #$00   ; low byte
sta $0
lda #$02   ; high byte
sta $1

start:
lda #$5
sta ($0),y   
inc $0
jmp start


