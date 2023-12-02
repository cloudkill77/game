lda #$00   ; low byte
sta $0
lda #$02   ; high byte
sta $1
define player1slot $10
define enemy1slot $11

player1:
lda $0
sta player1slot
lda #$5
sta ($0),y
inc $0
lda #$fc
sta ($0),y
inc $0
lda #$1
sta ($0),y
inc $0
lda #$20
sta ($0),y
inc $0

tiefighter1:
lda $0
sta enemy1slot
lda #$9
sta ($0),y
inc $0
lda #$2b
sta ($0),y
inc $0
lda #$0
sta ($0),y
inc $0
lda #$40
sta ($0),y
inc $0


