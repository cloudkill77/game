  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
   
;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
oamLo  .rs 1   ; pointer variables are declared in RAM
oamHi  .rs 1   ; low byte first, high byte immediately after
spriteslot .rs 1  ; 0-63 for sprites 
gamestate  .rs 1  ; $0 .rs 1 means reserve one byte of space
buttons1   .rs 1  ; $1 player 1 gamepad buttons, one bit per button
buttons2   .rs 1  ; $2 player 2 gamepad buttons, one bit per button
framecounter1 .rs 1  ; $5 count nmi frames. 60 frames per sec
framecounter2 .rs 1  ; $6 count nmi frames. 2 frames per sec

; GAMESTATE READY2GO



; GAMESTATE PLAYING

; good sprites:
player_y  .rs 1  ; $10 x-pos of player sprite tile 4/4
player_x  .rs 1  ; $11 y-pos of player sprite tile 4/4
player1slot .rs 1 ; record which spriteslot was used

; GAMESTATE GAMEOVER




;; DECLARE SOME CONSTANTS HERE
STATEINTRO     = $00  ; display into screen
STATEREADY2GO  = $01  ; displaying ready to go screen
STATEPLAYING   = $02  ; move sprite, check for collisions
STATEGAMEOVER  = $03  ; displaying game over screen

t21 = $D4 ; !
t2e = $C3 ; .
t2c = $D3 ; ,
t0 = $E0 ; 0
t1 = $E1 ; 1
t2 = $E2 ; 2
t3 = $E3 ; 3
t4 = $E4 ; 4
t5 = $E5 ; 5
t6 = $E6 ; 6
t7 = $E7 ; 7
t8 = $E8 ; 8
t9 = $E9 ; 9
t3e = $C2 ; >
t3f = $C1 ; ?
tA = $EA ; A
tB = $EB ; B
tC = $EC ; C
tD = $ED ; D
tE = $EE ; E
tF = $EF ; F
tG = $F0 ; G
tH = $F1 ; H
tI = $F2 ; I
tJ = $F3 ; J
tK = $F4 ; K
tL = $F5 ; L
tM = $F6 ; M
tN = $F7 ; N
tO = $F8 ; O
tP = $F9 ; P
tQ = $FA ; Q
tR = $FB ; R
tS = $FC ; S
tT = $FD ; T
tU = $FE ; U
tV = $FF ; V
tW = $C0 ; W
tX = $D0 ; X
tY = $D1 ; Y
tZ = $D2 ; Y
t5f = $D5 ; _


;;;;;;;;;;;;;;;;;
  .bank 0
  .org $C000 
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40	
  STX $4017    ; disable APU frame IRQ
  LDX #$FF	
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002        ; Check value of $2002 and set CPU flag for Zero, Negative, Overflow 
				   ; accordingly without affecting Accumulator
  BPL vblankwait1  ; If Negative flag is clear, branch to vblankwait1
  
  LDX #$00
clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002       ; Check value of $2002 and set CPU flag for Zero, Negative, Overflow 
				  ; accordingly without affecting Accumulator
  BPL vblankwait2 ; If Negative flag is clear, branch to vblankwait2
  
  

LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
  
  
  LDA #$00
  STA oamLo       ; pointer now says $0200
  LDA #$02
  STA oamHi



  LDY #$00
  STY player1slot
LoadSprites1Loop:
  LDA player, y        ; load data from address
  STA [oamLo], y          ; store into RAM address 
  INY                   ;
  CPY #$10              ; 
  BNE LoadSprites1Loop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to zero, keep going down
  lda #$10
  sta oamLo
  
  LDY #$00
LoadSprites2Loop:
  LDA sprites, y        ; load data from address
  STA [oamLo], y          ; store into RAM address 
  INY                   ;
  CPY #$ef              ; 
  BNE LoadSprites2Loop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to zero, keep going down


LoadINBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
 
  LDX #$00              ; start out at 0
LoadINBackgroundLoop1:
  LDA INbackground1, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00           ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadINBackgroundLoop1  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
  LDX #$00 
LoadINBackgroundLoop2:
  LDA INbackground2, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00           ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadINBackgroundLoop2  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
  LDX #$00 
LoadINBackgroundLoop3:
  LDA INbackground3, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00           ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadINBackgroundLoop3  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
  LDX #$00 
LoadINBackgroundLoop4:
  LDA INbackground4, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$a0           ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadINBackgroundLoop4  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero

LoadINAttribute:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadINAttributeLoop:
  LDA INattribute, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$08              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadINAttributeLoop



;; STARTING VARIABLES




;;:Set starting game state
  LDA #STATEPLAYING
  STA gamestate



init_audio:
  ldx #$00
 ; initialize sound registers 
initloop:
  lda #$00
  sta $4000, x
  inx
  cpx #$14
  bne initloop

  lda #$00
  sta $4015
  lda #$0F
  sta $4015
; setup apu frame counter
  lda #$40
  sta $4017

;APUFLAGS ($4015)
; 0F=1111
;76543210
;   |||||
;   ||||+- Square 1 (0: disable; 1: enable)
;   |||+-- Square 2
;   ||+--- Triangle
;   |+---- Noise
;   +----- DMC

  
ppu: 
; PPU registers  
  LDA #%10000000   ; enable NMI, sprite size 8x8, sprites from Pattern Table 0, base nametable address $2000
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, show sprites in leftmost 8 pixels of screen, show background in leftmost 8 pixels of screen
  STA $2001
  ; 7,6,5 color emphasis (BGR)
  ; 4 sprite enable (s)
  ; 3 background enable (b)
  ; 2 sprite left column enable (M)
  ; 1 background left column enable (m)
  ; 0 greyscale (G) 
  LDA #$00 ; there is no scrolling at end of nmi
  STA $2005
  STA $2005



Foreverloop:


  JMP Foreverloop     ;jump back to Forever, infinite loop

falling:
  
  ; 7,0
  lda framecounter2
  cmp #$e
  bne next65
  lda #$f0
  sta $0200
  sta $0204
  clc
  adc #$8 
  sta $0208
  sta $020c

; 6,5
next65:
  lda framecounter2
  cmp #$d
  bne next60
  lda #$cf
  sta $0200
  sta $0204
  clc
  adc #$8 
  sta $0208
  sta $020c

; 6,0
next60:
  lda framecounter2
  cmp #$c
  bne next55
  lda #$b0
  sta $0200
  sta $0204
  clc
  adc #$8 
  sta $0208
  sta $020c

; 5,5
next55:
  lda framecounter2
  cmp #$b
  bne next50
  lda #$94
  sta $0200
  sta $0204
  clc
  adc #$8 
  sta $0208
  sta $020c

; 5,0
next50:
  lda framecounter2
  cmp #$a
  bne next45
  lda #$7a
  sta $0200
  sta $0204
  clc
  adc #$8 
  sta $0208
  sta $020c

; 4,5
next45:
  lda framecounter2
  cmp #$9
  bne next40
  lda #$63
  sta $0200
  sta $0204
  clc
  adc #$8 
  sta $0208
  sta $020c

; 4,0
next40:
  lda framecounter2
  cmp #$8
  bne next35
  lda #$4e
  sta $0200
  sta $0204
  clc
  adc #$8 
  sta $0208
  sta $020c

; 3,5
next35:
  lda framecounter2
  cmp #$7
  bne next30
  lda #$3c
  sta $0200
  sta $0204
  clc
  adc #$8 
  sta $0208
  sta $020c


; 3,0
next30:
  lda framecounter2
  cmp #$6
  bne next25
  lda #$2c
  sta $0200
  sta $0204
  clc
  adc #$8 
  sta $0208
  sta $020c

; 2,5
next25:
  lda framecounter2
  cmp #$5
  bne next20
  lda #$1e
  sta $0200
  sta $0204
  clc
  adc #$8 
  sta $0208
  sta $020c

; 2,0
next20:
  lda framecounter2
  cmp #$4
  bne next15
  lda #$13
  sta $0200
  sta $0204
  clc
  adc #$8 
  sta $0208
  sta $020c

; 1,5
next15:
  lda framecounter2
  cmp #$3
  bne next10
  lda #$b
  sta $0200
  sta $0204
  clc
  adc #$8 
  sta $0208
  sta $020c

; 1,0
next10:
  lda framecounter2
  cmp #$2
  bne next05
  lda #$5
  sta $0200
  sta $0204
  clc
  adc #$8 
  sta $0208
  sta $020c

; 0,5
next05:  
  lda framecounter2
  cmp #$1
  bne next00
  lda #$1
  sta $0200
  sta $0204
  clc
  adc #$8 
  sta $0208
  sta $020c

; 0
next00:
  lda framecounter2
  cmp #$0
  bne fallingend
  lda #$0
  sta $0200
  sta $0204
  clc
  adc #$8 
  sta $0208
  sta $020c
  
fallingend:


; read controller subroutine. called from within NMI
ReadController1:
  LDA #$01
  STA $4016		; write 1 to $4016
  LDA #$00
  STA $4016		; write 0 to $4016. tells cpu to poll the controller
  LDX #$08		; cycle through loading values of $4016 8 times to get value for each button 
ReadController1Loop:
  LDA $4016		; load value stored in $4016
  LSR A			; bit0 -> Carry
  ROL buttons1	; bit0 <- Carry
  DEX
  BNE ReadController1Loop
  RTS





;;;;;;;;;;;

NMI: ; called 60 times per second
; [RENDER]  
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer



;; PPU cleanup?

  JSR ReadController1
; JSR ReadController2  ;;get the current button data for player 2

GameEngine:  
StateIntro:
  LDA gamestate
  CMP #STATEINTRO
  BNE StateReady2Go
  JMP EngineIntro    ;;game is displaying intro screen
  
StateReady2Go:
  LDA gamestate
  CMP #STATEREADY2GO
  BNE StatePlaying
  JMP EngineReady2Go ;; game is ready

StatePlaying:  
  LDA gamestate
  CMP #STATEPLAYING
  BNE StateGameOver
  JMP EnginePlaying   ;;game is playing

StateGameOver:    
  LDA gamestate
  CMP #STATEGAMEOVER
  BNE GameEngineDone
  JMP EngineGameOver  ;;game is displaying ending screen
  
GameEngineDone:  
  
;  dec counter ; 60 fps counting down
  inc framecounter1 ; 60 fps counting up

  lda framecounter1
  cmp #$1e ; compare to decimal 60, hex 3c, (1 second)
  bne nmi_end ; branch until counter has reached $30
  inc framecounter2 ; 1 fps counting up
  lda #$0
  sta framecounter1 ; reset framecounter1 to 0



nmi_end:
  RTI

;;;;;;;;;;;;;;;

;;;;;;;;
 
EngineIntro:


  
  JMP GameEngineDone


 ;;;;;;;;; 
 
EngineReady2Go:





  JMP GameEngineDone



;;;;;;;;;;;
 
EnginePlaying:
  

  LDA buttons1       ; load player 1 - buttons
  AND #%10000000  ; only look at bit 7
  BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
  LDA #$0
  STA oamLo
ReadADone:

 
  LDA buttons1       ; load player 1 - buttons
  AND #%01000000  ; only look at bit 6
  BEQ ReadBDone   ; branch to ReadBDone if button is NOT pressed (0)
  LDA #$1
  STA oamLo
ReadBDone:

 
  LDA buttons1       ; load player 1 - buttons
  AND #%00001000  ; only look at bit 3
  BEQ ReadUpDone   ; branch to ReadUpDone if button is NOT pressed (0)
; move guy up 
  LDA player_y
  CMP #$b ; 
  BEQ ReadUpDone ; if equal to zero, go to readupdone

ReadUpDone:

  LDA buttons1       ; load player 1 - buttons
  AND #%00000100  ; only look at bit 2
  BEQ ReadDownDone   ; branch to ReadDownDone if button is NOT pressed (0)
; move guy down 

ReadDownDone:

  LDA buttons1       ; load player 1 - buttons
  AND #%00000010  ; only look at bit 1
  BEQ ReadLeftDone   ; branch to ReadLeftDone if button is NOT pressed (0)
; Move guy to the left 

ReadLeftDone:

  LDA buttons1       ; load player 1 - buttons
  AND #%00000001  ; only look at bit 0
  BEQ ReadRightDone ; branch to ReadRightDone if button is NOT pressed (0)
; Move guy to the right  

ReadRightDone:


  jsr falling




;  JSR UpdateSpritePosition  ;; setup sprite positions

  JMP GameEngineDone


;;;;;;;;; 
 
EngineGameOver:
  ;;if start button pressed
  ;;  turn screen off
  ;;  load title screen
  ;;  go to Title State
  ;;  turn screen on 
  JMP GameEngineDone





;;;;;;;;;;;;;;  

  .bank 1
  .org $E000
  
; info for palettes, sprites and background, called during initialization


palette:
  .db $0f,$19,$09,$21	;background palette 1 - 0f-black,19-green,09-dark green,21-light blue
  .db $0f,$22,$27,$10	;background palette 2 $22,$36,$17,$0D
  .db $0f,$21,$05,$01	;background palette 3
  .db $0f,$28,$27,$21	;background palette 4
  .db $0f,$10,$00,$30	;sprite palette 1  ; 0f-black,10-light grey,00-grey,30-white
  .db $0f,$22,$27,$10	;sprite palette 2 - fire
  .db $0f,$21,$05,$01	;sprite palette 2
  .db $0f,$2a,$09,$07	;sprite palette 3


;     y,   tile,attribute, x

player:
  .db $0, $0A, %00000000, $40, $0, $0B, %00000000, $48, $8, $1A, %00000000, $40, $8, $1B, %00000000, $48
  .db $f0, $0E, %00000001, $CD   ;missile (y 220, x 205)
sprites:
tiefighter:
  .db $80, $4a, %00000000, $80, $80, $4b, %00000000, $88, $88, $5a, %00000000, $80, $88, $5b, %00000000, $88
game:
  .db $60, tG, %00000000, $68 ; G
  .db $60, tA, %00000000, $70 ; A
  .db $60, tM, %00000000, $78 ; M
  .db $60, tE, %00000000, $80 ; E
sun:
  .db $f0, $40, %00000010, $08
  .db $f0, $41, %00000010, $10
  .db $f0, $42, %00000010, $18
  .db $f0, $50, %00000010, $08
  .db $f0, $51, %00000000, $10
  .db $f0, $52, %00000010, $18
  .db $f0, $60, %00000010, $08
  .db $f0, $61, %00000010, $10
  .db $f0, $62, %00000010, $18
clouds:
  .db $f0, $6b, %00000010, $58 ; cloud1 1/3
  .db $f0, $6c, %00000010, $60 ; cloud1 2/3
  .db $f0, $6d, %00000010, $68 ; cloud1 3/3
  .db $f0, $7b, %00000010, $60 ; cloud2 1/3
  .db $f0, $6c, %00000010, $68 ; cloud2 2/3
  .db $f0, $6d, %00000010, $70 ; cloud2 3/3		
factory:
  .db $f0, $88, %00000010, $65
  .db $f0, $89, %00000010, $6D
  .db $f0, $98, %00000010, $65
  .db $f0, $99, %00000010, $6D
brokenfactory;
  .db $f0, $aa, %00000010, $65 ; a
  .db $f0, $ab, %00000010, $6D ; c
  .db $f0, $ba, %00000010, $65 ; b
  .db $f0, $bb, %00000010, $6D ; d
INblackplane:
  .db $f0, $2c, %00000000, $0 ; plane1 1/4
  .db $f0, $2d, %00000000, $8 ; plane1 2/4
  .db $f0, $3c, %00000000, $0 ; plane1 3/4
  .db $f0, $3d, %00000000, $8 ; plane1 4/4
;  76543210
;  ||||||||
;  ||||||++- Palette (4 to 7) of sprite
;  |||+++--- Unimplemented (read 0)
;  ||+------ Priority (0: in front of background; 1: behind background)
;  |+------- Flip sprite horizontally
;  +-------- Flip sprite vertically

plane2:
  .db $f0, $2a, %00000000, $10 ; plane2 1/4 with lights
  .db $f0, $2b, %00000000, $18 ; plane2 2/4 with lights 
  .db $f0, $3a, %00000000, $10 ; plane2 3/4 with lights
  .db $f0, $3b, %00000000, $18 ; plane2 4/4 with lights
INbutton_ab:
  .db $f0, $48, %00000000, $90 ; A/B Button
INarrowbig:
  .db $f0, $49, %00000000, $98 ; big arrow
INbuttonlabel:  
  .db $f0, $58, %00000000, $8c ; A
  .db $f0, $59, %00000000, $94 ; B 
tombstone:
  .db $f0, $02, %00000000, $68 ; tombstone 1/4
  .db $f0, $03, %00000000, $70 ; tombstone 2/4
  .db $f0, $12, %00000000, $68 ; tombstone 3/4
  .db $f0, $13, %00000000, $70 ; tombstone 4/4
moon:
  .db $f0, $a8, %00000000, $20 ; moon 1/4
  .db $f0, $a9, %00000000, $28 ; moon 2/4
  .db $f0, $b8, %00000000, $20 ; moon 3/4
  .db $f0, $b9, %00000000, $28 ; moon 4/4
buttonstart: ; 
  .db $f0, $d6, %00000000, $80 ; startbutton
otherletters:
  .db $f0, tR, %00000000, $58 ; R
  .db $f0, tD, %00000000, $68 ; D
  .db $f0, tY, %00000000, $70 ; Y
  .db $f0, t21, %00000000, $78 ; !
  .db $f0, tO, %00000000, $80 ; O
  .db $f0, tV, %00000000, $88 ; V
  .db $f0, tU, %00000000, $50 ; U





INbackground1:
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
INbackground2:
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
INbackground3:
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $83, $95, $97, $97, $97, $97, $97, $97, $97, $96, $86, $B7, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $B7, $73, $77, $65, $57, $47, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $47, $57, $57, $57, $57, $66, $77, $87, $97, $A7, $A7, $A7, $B7, $B7
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
INbackground4:
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
INattribute:
  .db %00000000, %00000000, %0000000, %00000000, %00000000, %00000000, %00000000, %00000000
 
GObackground1:
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
GObackground2:
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, tY, tO, tU, $0F, tA, tR, tE, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, tD, tE, tA, tD, t2e, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, tR, tE, tT, tR, tY, t3f, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
GObackground3:
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $83, $95, $97, $97, $97, $97, $97, $97, $97, $96, $86, $B7, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $B7, $73, $77, $65, $57, $47, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $47, $57, $57, $57, $57, $66, $77, $87, $97, $A7, $A7, $A7, $B7, $B7
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
GObackground4:
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
GOattribute:
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
 

;;;;;;;;;;;;;;  

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
;;;;;;;;;;;;;;  

  .bank 2
  .org $0000
  .incbin "game.chr"   ;includes 8KB graphics file from SMB1 (tm)
  
