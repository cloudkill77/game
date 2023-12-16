
define ypos $0
define xpos $1
define fwd $10
lda #$0
sta fwd
lda #$64
sta ypos
sta xpos

down:
inc fwd
clc
lda ypos
adc fwd
sta ypos
dec xpos

; f1,5,9
; o1
; x-1
; y+1
; y+5
; y+9

right:
inc fwd
clc
lda xpos
adc fwd
sta xpos
inc ypos

; f2,6,10
; o1
; y+1
; x+2
; x+6
; x+10

up:
inc fwd
sec
lda ypos
sbc fwd
sta ypos
inc xpos

; f3,7,11
; o1
; x+1
; y-3
; y-7
; y-11

left:
inc fwd
sec
lda xpos
sbc fwd
sta xpos
dec ypos

; f4,8,12
; o1
; y-1
; x-4
; x-8
; x-12


jmp down