  LDA #$00
  STA oamLo       
  LDA #$02
  STA oamHi       ; pointer now says $0200



  LDY #$00
LoadSprites1Loop:
  LDA player, y        ; load data from address
  STA [oamLo], y          ; store into RAM address 
  INY                   ;
  CPY #$10              ; 
  BNE LoadSprites1Loop   ; Branch to LoadSprites1Loop if compare was Not Equal to zero
                        ; if compare was equal to zero, keep going down
  lda #$10
  sta oamLo            ; advance address for next sprite

  LDY #$00
LoadSprites2Loop:
  LDA sprites, y        ; load data from address
  STA [oamLo], y          ; store into RAM address 
  INY                   ;
  CPY #$ef              ; 
  BNE LoadSprites2Loop   ; Branch to LoadSprites2Loop if compare was Not Equal to zero
                        ; if compare was equal to zero, keep going down
