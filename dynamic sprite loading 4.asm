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
