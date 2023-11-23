lda #$0
sta $0
start:
ldx $0
txa
;and #$7
clc
adc #$1
and #$7
tax
stx $0


jmp start
