define oamL $00
define oamH $01
lda #$10
sta oamL
lda #$02
sta oamH
ldy #$0

;1 y=16
lda #$b
jsr store

;2 y=48
jsr add
lda #$a
jsr store

;3 y=80
jsr add
lda #$9
jsr store

;4
jsr add
lda #$8
jsr store

;5
jsr add
lda #$7
jsr store

;6
jsr add
lda #$6
jsr store

;7
jsr add
lda #$5
jsr store

;8
jsr add
lda #$4
jsr store

;9
inc oamH
lda #$10
sta oamL
lda #$3
jsr store

;10
jsr add
lda #$2
jsr store

;11
jsr add
lda #$1
jsr store

;12
jsr add
lda #$f
jsr store

;13
jsr add
lda #$e
jsr store

;14
jsr add
lda #$d
jsr store

;15
jsr add
lda #$c
jsr store

;16
jsr add
lda #$b
jsr store

;16
jsr addH
lda #$a
jsr store

;17
jsr add
lda #$9
jsr store

;18
jsr add
lda #$8
jsr store

;19
jsr add
lda #$7
jsr store

;20
jsr add
lda #$6
jsr store

;21
jsr add
lda #$5
jsr store

;22
jsr add
lda #$4
jsr store

;23
jsr add
lda #$3
jsr store

;24
jsr add
lda #$2
jsr store

;25
jsr addH
lda #$10
jsr store

;26
jsr add
lda #$11
jsr store

;27
jsr add
lda #$12
jsr store

;28
jsr add
lda #$13
jsr store

;29
jsr add
lda #$15
jsr store

;30
jsr add
lda #$16
jsr store

;31
jsr add
lda #$17
jsr store

;32
jsr add
lda #$18
jsr store

brk

; add 32 to oamL
add:
clc
lda oamL
adc #$20
sta oamL
rts

store:
sta (oamL),y
rts

addH:
inc oamH
lda #$10
sta oamL
rts

