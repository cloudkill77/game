;; tested - 4.12.2023 - working

gravts1 .rs 1         ; index counter for gravity table 1/2
gravts2 .rs 1         ; index counter for gravity table 2/2


;*********************************

Foreverloop:
  JMP Foreverloop     ;jump back to Forever, infinite loop


resetgravts:
  ldx #$0
  stx gravts1
  stx gravts2
  jmp falling
  
falling2:
  ldy #$0
  ldx gravts2
  cpx #$ff
  beq resetgravts
  lda gravt2,x
  sta $0200
  sta $0204
  clc
  adc #$8     ; add 8 pixels for 2nd row of sprites
  sta $0208
  sta $020c
  inc gravts2  ; increment gravity table index  

  jmp fallingend
  
falling:

  ldy #$0
  ldx gravts1  ; index for gravity table
  cpx #$ff
  beq falling2
  lda gravt1,x ; load y-pos from gravity table
  sta $0200
  sta $0204
  clc
  adc #$8     ; add 8 pixels for 2nd row of sprites
  sta $0208
  sta $020c
  inc gravts1  ; increment gravity table index


fallingend:
  rts

;*********************************

ReadRightDone:

  jsr falling


;*********************************

;;;;;;;;;;;;;;  

  .bank 1
  .org $E000
  

gravt1:
  .db $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$2,$2,$2,$2,$2,$2,$2,$2,$3,$3,$3,$3,$3,$3,$3,$3,$4,$4,$4,$4,$4,$4,$5,$5,$5,$5,$5,$5,$6,$6,$6,$6,$6,$7,$7,$7,$7,$7,$8,$8,$8,$8,$8,$9,$9,$9,$9,$A,$A,$A,$A,$B,$B,$B,$B,$C,$C,$C,$C,$D,$D,$D,$D,$E,$E,$E,$F,$F,$F,$F,$10,$10,$10,$11,$11,$11,$12,$12,$12,$12,$13,$13,$13,$14,$14,$14,$15,$15,$15,$16,$16,$17,$17,$17,$18,$18,$18,$19,$19,$19,$1A,$1A,$1B,$1B,$1B,$1C,$1C,$1D,$1D,$1D,$1E,$1E,$1F,$1F,$1F,$20,$20,$21,$21,$21,$22,$22,$23,$23,$24,$24,$25,$25,$25,$26,$26,$27,$27,$28,$28,$29,$29,$2A,$2A,$2B,$2B,$2C,$2C,$2D,$2D,$2E,$2E,$2F,$2F,$30,$30,$31,$31,$32,$32,$33,$33,$34,$34,$35,$35,$36,$36,$37,$38,$38,$39,$39,$3A,$3A,$3B,$3C,$3C,$3D,$3D,$3E,$3E,$3F,$40,$40,$41,$41,$42,$43,$43,$44,$44,$45,$46,$46,$47,$48,$48,$49,$49,$4A,$4B,$4B,$4C,$4D,$4D,$4E,$4F,$4F,$50,$51,$51,$52,$53,$53,$54,$55,$55,$56,$57,$57,$58
gravt2:  
  .db $59,$59,$5A,$5B,$5C,$5C,$5D,$5E,$5E,$5F,$60,$61,$61,$62,$63,$63,$64,$65,$66,$66,$67,$68,$69,$69,$6A,$6B,$6C,$6D,$6D,$6E,$6F,$70,$70,$71,$72,$73,$74,$74,$75,$76,$77,$78,$78,$79,$7A,$7B,$7C,$7C,$7D,$7E,$7F,$80,$81,$81,$82,$83,$84,$85,$86,$87,$87,$88,$89,$8A,$8B,$8C,$8D,$8E,$8E,$8F,$90,$91,$92,$93,$94,$95,$96,$96,$97,$98,$99,$9A,$9B,$9C,$9D,$9E,$9F,$A0,$A1,$A2,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$AA,$AB,$AC,$AD,$AE,$AF,$B0,$B1,$B2,$B3,$B4,$B5,$B6,$B7,$B8,$B9,$BA,$BB,$BC,$BD,$BE,$BF,$C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$CB,$CC,$CD,$CF,$D0,$D1,$D2,$D3,$D4,$D5,$D6,$D7,$D8,$D9,$DA,$DB,$DD,$DE,$DF,$E0,$E1,$E2,$E3,$E4,$E5,$E7,$E8,$E9,$EA,$EB,$EC,$ED,$EE,$F0
