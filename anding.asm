  inc restartloopcounter
  lda restartloopcounter
  cmp #$6
  bne skip      ; branch to skip until counter has reached 6

  lda arrowmove ; load variable for adjusting x-position of animated arrow
  clc
  adc #$1       ; add with carry 1
  and #$7       ; perform AND to limit to 7 (i think)
  sta arrowmove ; store new variable (should be incrementing between 0 and 7)

  lda #$89      ; load starting position of animated arrow
  sec           ; set carry
  sbc arrowmove ; subtract variable from starting position
  sta $24b      ; write adjusted x-pos of arrow sprite to OAM memory
  lda #$0       ; reset restartloopcounter to 0
  sta restartloopcounter
  
skip: ; branch here until restartloopcounter has reached 6. should result in 10 updates per second if restartloopcounter is running at 60fps?
