

  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $14, decimal 20. loads the first 20 bytes of sprites (5 sprites)
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to zero, keep going down
                       ; if compare was equal to zero, keep going down 
  

sprites:
player:
  .db $78, $0A, %00000000, $18, $78, $0B, %00000000, $20, $80, $1A, %00000000, $18, $80, $1B, %00000000, $20

tiefighter:
  .db $c8, $4a, %00000000, $32, $c8, $4b, %00000000, $3a, $d0, $5a, %00000000, $32, $d0, $5b, %00000000, $3a 

**********************************************
define slot $0
define playerslot $1


lda slot
sta playerslot
asl
asl
tax
loadplayerloop:
lda player, x
sta $0200, x
inx
cpx #$4
bne loadplayerloop




player:
.db $78, $0A, %00000000, $18, $78, $0B, %00000000, $20, $80, $1A, %00000000, $18, $80, $1B, %00000000, $20
