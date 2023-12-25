define xvar $00     ; x variable
define yvar $01     ; y variable
define temp1 $10   ; temporary variable 1
define temp2 $20 ; temporary variable 2
lda #$04
sta xvar



ldx #$00
loop:            ; square variable x
lda temp1
clc
adc xvar
sta temp1
inx
cpx xvar
bne loop

; divide by 4
lda temp1
lsr
lsr
sta temp2

; add 4x
lda xvar
asl
asl
sta temp1

; subtract first
lda temp1
sec
sbc temp2


; store result
sta yvar



