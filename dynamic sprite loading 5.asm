;; tested 8.12.2023, tested and works with 1 sprite

define p1 $10   ; oam address eg #$04: $0204
define p1y $11 ; player 1 y-pos
define p1t $12 ; tile number
define p1a $14 ; attribute
define p1x $15 ; x-pos
define p1e $16 ; extended attributes
; 76543210
; 1,0 : flicker group 0,1,2,3
; 2 : onscreen
; 3 : 
; 4 : 
; 5 : retreating
; 6 : homing
; 7 : firing
define p1h $17
; 76543210
; 5,4,3,2,1,0 ; health 0-63
; 6 : damaged
; 7 : dead
define e1 $18
define e1y $19  ; enemy 1
define e1t $1a
define e1a $1b
define e1x $1c
define e1e $1d
define e1h $1e
define m1 $1f
define m1y $20 ; missile 1
define m1t $21
define m1a $22
define m1x $23
define m1e $24
define m1h $25
define l1 $26
define l1y $27  ; laser 1
define l1t $28
define l1a $29
define l1x $2a
define l1e $2b
define l1h $2c
define l2 $2d
define l2y $2e ; laser 2
define l2t $2f
define l2a $30
define l2x $31
define l2e $32
define l2h $33

; flicker groups:
; 0 - dont flicker (draw first)
; 1 - flicker on odd frames
; 2 - flicker on even frames
; 3 - low priority (draw last)

define oamL $0; starts at 00, up to FF
define oamH $1 ; fixed at 02
lda #$02
sta oamH





nmi:
;oam
lda oamL ; check oam usage
; if offscreen, dont load into oam


;render all frames

lda l1 ; load oam address of l1 into a
sta oamL ; store current l1 oam address
ldx #$c ; start at 12 (laser1)
loop:
lda sprites,x ; load y-pos (for example) into a
jsr store
cpx #$10 ; at 16 stop looping
bne loop

;render odd frames


;render even frames


jmp nmi


rti


store:

sta (oamL),y
inc oamL
inx
rts


sprites:
dcb $ff,$e0,$0,$60,$1,$3f  ; sprite1: y,t,a,x,e,h
dcb $ff, $e1,$0,$60,$2,$3f ; sprite2
dcb $ff,$a0,$0,$60,$1,$0   ; laser1
dcb $ff,$a1,$0,$60,$1,$0   ; laser2

; damage is measured per tile
; allows for partial damage display
; death occurs if any one tile goes below 0
; test if it makes sense



