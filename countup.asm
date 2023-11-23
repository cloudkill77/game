lda #$70
start:
adc #$1
sta $220
bmi minus
jmp start

minus:
sta $221
brk

; counts up to 7f, then on 80 it sets the negative & overflow flags and stops
; -127 to 0 to 127
; -7f to 0 to 7f