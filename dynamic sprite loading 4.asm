define oamL $0
define oamH $1
define e1oamYL $10
define e1oamYH $11
define e1oamTL $12
define e1oamTH $13
define e1oamAL $14
define e1oamAH $15
define e1oamXL $16
define e1oamXH $17
define key $2
define frame $3

lda #$00
sta oamL
lda #$02
sta oamH
sta e1oamYH
sta e1oamTH
sta e1oamAH
sta e1oamXH

jsr oamstore
lda #$30 ; y-pos
sta (e1oamYL), y
lda #$fc ; tile #
sta (e1oamTL), y
lda #$0  ; attribute
sta (e1oamAL), y
lda #$50 ; x-pos
sta (e1oamXL), y

loop:

lda frame
cmp #$4
beq nmi
inc frame

jmp loop

nmi:

lda #$0
sta frame
ldx key
lda anim_player,x
sta (e1oamTL),y
inc key
jmp loop

oamstore:
clc
lda oamL         ; eg 0
sta e1oamYL ; store
adc #$1          ; eg 0+1
sta e1oamTL ; store
adc #$1          ; eg 0+1+1
sta e1oamAL ; store
adc #$1          ; eg 0+1+1+1
sta e1oamXL ; store
rts

anim_player:
dcb $e0, $e0, $e0, $e1, $e1, $e1, $e1, $e2, $e2, $e2, $e3, $e3, $e3, $e4, $e4, $e4, $e4, $e5, $e5, $e5, $e5, $e6, $e6, $e6, $e6, $e6, $e6

oamslotY:
dcb $00,$04,$08,$0c,$10,$14,$18,$1c,$20,$24,$28,$2c,$30,$34,$38,$3c,$40,$44,$48,$4c,$50,$54,$58,$5c,$60,$64,$68,$6c,$70,$74,$78,$7c,$80,$84,$88,$8c,$90,$94,$98,$9c,$a0,$a4,$a8,$ac,$b0,$b4,$b8,$bc,$c0,$c4,$c8,$cc,$d0,$d4,$d8,$dc,$e0,$e4,$e8,$ec,$f0,$f4,$f8,$fc

oamslotT:
dcb $01,$05,$09,$0d,$11,$15,$19,$1d,$21,$25,$29,$2d,$31,$35,$39,$3d,$41,$45,$49,$4d,$51,$55

oamslotA:
dcb $02,$06,$0a,$0e,$12,$16,$1a,$1e

oamslotX:
dcb $03,$07,$0b,$0f,$13,$17,$1b,

other:
; dcb $1,$4,$9,$16,$25,$36,$49,$64,$81,$100,$121,$144,$169,$196,$225,$256
dcb $1,$4,$9,$10,$19,$24,$31,$40,$51,$64,$79,$90,$a9,$c4,$e1,$ff+1
