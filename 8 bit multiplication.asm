lda #$ff
sta $0     ; factor 1
lda #$3
sta $1     ; factor 2

clc
ldx #$0
loop:
lda $11    ; load low byte result
adc $0     ; add factor 1
sta $11    ; store as low byte result
inx        ; increment x
lda #$0    ; load 0
adc $10    ; add carry to high byte
sta $10    ; store as high byte result
cpx $1     ; compare x against factor 2
bne loop   ; loop if x is less than factor 2

brk
