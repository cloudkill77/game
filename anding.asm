  inc restartloopcounter
  lda restartloopcounter
  cmp #$6
  bne skip

  lda arrowmove
  clc
  adc #$1
  and #$7
  sta arrowmove

  lda #$89
  clc
  sbc arrowmove
  sta $24b
  lda #$0
  sta restartloopcounter
  
skip: