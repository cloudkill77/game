; © ?
; sqrt7
define ROOT $00
define REM $01
define NUMH $11
define NUML $10

lda #$19
sta NUML


LDA #$00
STA ROOT
STA REM
LDX #$8
L1:
SEC
LDA NUMH
SBC #$40
TAY
LDA REM
SBC ROOT
BCC L2
STY NUMH
STA REM
L2:
ROL ROOT
ASL NUML
ROL NUMH
ROL REM
ASL NUML
ROL NUMH
ROL REM
DEX
BNE L1

brk