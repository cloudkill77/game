; multiplication of two 8 bit numbers ($00 and $01), storing result in a 16 bit number stored as high and low bytes ($10 and $11 respectively)
; multiplication is performed by adding factor 1 to itself factor 2 number of times. ie 4x3 = 0+4+4+4 = 12
; if an overflow occurs on the low byte, a carry of value 1 is set, it then gets added to the high byte. if no overflow occured, no carry is added.

lda #$ff
sta $00    ; factor 1
lda #$3
sta $01    ; factor 2

clc
ldx #$00
loop:
lda $11    ; load low byte result
adc $00    ; add factor 1
sta $11    ; store as low byte result
inx        ; increment x
lda #$00   ; load 0
adc $10    ; add carry (if present) to high byte
sta $10    ; store as high byte result
cpx $01    ; compare x against factor 2
bne loop   ; loop if x is less than factor 2

brk
