







(was at location 492)
+++++++++++++++++++++++++++++++++++++++++++++
animatedspriteloop:
  CLC
  lda #$50
  sta $0230 ; y-pos of bouncing ball
  ldx sprite ; load current sprite frame into x
  lda array, x ; load value from array based on sprite frame
  sta $0231 ; tile number of bouncing ball
  lda #%00000001
  sta $0232 ; attribute of bouncing ball. colour palette 1 set
  lda #$50
  sta $0233 ; x-pos of bouncing ball

  ldy frame ; load current frame number into y
  iny ; increase y by 1
  sty frame ; store new frame number
  cpy #$6 ; count up to 6
  beq nextframe ; if result is zero, branch to nextframe, otherwise continue
  jmp nmi_end2

; /frame animation
nextframe:
;  jmp nmi_end2
  ldx #$0
  stx frame ; reset frame counter to 0
  ldx sprite ; load current sprite frame number
  inx ; increase frame by 1
  stx sprite ; store new sprite frame number value
  cpx #$a ; count up to 10
  beq resetanimation ; if result is zero, branch to resetanimation, otherwise continue ____ PROBLEM HERE -- problem was 
  ;with rts at end of nextframe and resetanimation. removed and fixed problem.
  jmp nmi_end2
resetanimation:
  ldx #$0
  stx sprite ; reset sprite frame counter to 0

+++++++++++++++++++++++++++++++++++++++++++++++++











(was at location 586)
+++++++++++++++++++++++++++++++++++++++++
array: ; bouncing ball animated frames / tile sequence
  .db $c4
  .db $c5
  .db $c6
  .db $c7
  .db $c8
  .db $c8
  .db $c7
  .db $c6
  .db $c5
  .db $c4

;sprite_array:
;  .db $50, $c4, %00000000, $58 ; frame1
;  .db $60, $c5, %00000000, $60 ; frame2
;  .db $70, $c6, %00000000, $68 ; frame3
;  .db $70, $c7, %00000000, $70 ; frame4
;  .db $70, $c8, %00000000, $78 ; frame5
;  .db $70, $c8, %00000000, $80 ; frame5
;  .db $70, $c7, %00000000, $88 ; frame4
;  .db $70, $c6, %00000000, $90 ; frame3
;  .db $60, $c5, %00000000, $98 ; frame2
;  .db $50, $c4, %00000000, $A0 ; frame1


+++++++++++++++++++++++++++++++++++++++++++++++