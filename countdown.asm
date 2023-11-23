lda #$20
start:
sbc #$1
sta $220
bmi minus
jmp start

minus:
sta $221
brk

; counts down from 20, then at ff it sets the negative & overflow flags and stops
; -127 to 0 to 127
; -7f to 0 to 7f

