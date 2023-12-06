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
jsr store
lda #$fc ; tile #
jsr store
lda #$0  ; attribute
jsr store
lda #$50 ; x-pos
jsr store
brk


store:
sta (oamL), y
inc oamL
rts


oamstore:
clc
lda oamL         ; eg 0
sta e1oamYL      ; store
adc #$1          ; eg 0+1
sta e1oamTL      ; store
adc #$1          ; eg 0+1+1
sta e1oamAL      ; store
adc #$1          ; eg 0+1+1+1
sta e1oamXL      ; store
rts

anim_player:
dcb $e0, $e0, $e0, $e1, $e1, $e1, $e1, $e2, $e2, $e2, $e3, $e3, $e3, $e4, $e4, $e4, $e4, $e5, $e5, $e5, $e5, $e6, $e6, $e6, $e6, $e6, $e6
