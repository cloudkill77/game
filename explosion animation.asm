define framecounter $10
define oamL $0
define oamH $1

jmp counter 

frame5:
ldy #$08
lda #$0a
;sta [oamL], y
sta (oamL), y
jmp counter

frame4:
ldy #$08
lda #$09
;sta [oamL], y
sta (oamL), y
jmp counter

frame3:
ldy #$08
lda #$08
;sta [oamL], y
sta (oamL), y
jmp counter

frame2:
ldy #$08
lda #$07
;sta [oamL], y
sta (oamL),y
jmp counter

frame1:
ldy #$08
lda #$06
;sta [oamL], y
sta (oamL),y
jmp counter

counter:
inc framecounter
lda framecounter
cmp #$0a
bcs frame5
cmp #$08
bcs frame4
cmp #$06
bcs frame3
cmp #$04
bcs frame2
cmp #$02
bcs frame1
jmp counter