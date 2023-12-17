define sp1y $0 ; adjusted y-pos
define sp1x $1 ; adjusted x-pos
define my $10 ;  source y-pos
define mx $11 ; source x-pos
define sp1counter $20 ; loop counter for increasing / decreasing spiral
lda #$60 ; starting y-pos
sta sp1y
sta my  ; store source position
lda #$60 ; starting x-pos
sta sp1x
sta mx ; store source position


LDX #$00 
STX sp1counter ; set loop counter to 0

loop1:
ldx sp1counter
cpx #$36  ; loop counter value check
; cpx #$4 ; testing value
bcs loop2 ; if its equal to or greater than #$36, branch to loop2

clc
lda y1, x
adc my
sta sp1y
clc
lda x1, x
adc mx
sta sp1x
inx
stx sp1counter
jmp loop1
brk

loop2:
ldx sp1counter
cpx #$1b   ; loop counter value check
; cpx #$8 ; testing value
bcs reset  ; if its equal to or greater than #$1b, branch to reset

clc
lda y2, x
adc my
sta sp1y
clc
lda x2, x
adc mx
sta sp1x
inx
stx sp1counter
jmp loop2
brk

reset:
ldx #$00
stx sp1counter
jmp loop1




y1: ; grow spiral
DCB $00,$ff,$ff,$00,$ff,$ff,$00,$ff,$fe,$fe,$ff,$00,$01,$02,$02,$02,$ff,$00,$ff,$fe,$fd,$fd,$fd,$fd,$fe,$ff,$00,$01,$02,$03,$03,$03,$02,$01,$00,$ff,$fe,$fd,$fc,$fc,$fc,$fd,$fe,$ff,$00,$01,$02,$03,$03,$03,$02,$01,$00,$ff
y2: ; shrink spiral
dcb $fe,$fd,$fd,$fd,$fd,$fe,$ff,$00,$01,$02,$02,$02,$02,$01,$00,$ff,$fe,$fe,$ff,$00,$01,$01,$00,$ff,$ff,$00,$00

x1: ; grow spiral
DCB $00,$00,$01,$01,$01,$00,$ff,$ff,$00,$01,$02,$02,$02,$01,$00,$ff,$fe,$fe,$fe,$fe,$ff,$00,$01,$02,$03,$03,$03,$03,$02,$01,$00,$ff,$fe,$fd,$fd,$fd,$fd,$fe,$ff,$00,$01,$02,$03,$03,$03,$03,$02,$01,$00,$ff,$fe,$fd,$fd,$fd
x2: ; shrink spiral
dcb $fe,$ff,$00,$01,$02,$03,$03,$03,$03,$02,$01,$00,$ff,$fe,$fe,$ff,$00,$01,$02,$02,$01,$00,$ff,$00,$01,$01,$00
