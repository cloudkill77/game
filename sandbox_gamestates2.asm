  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
   
;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
  
gamestate  .rs 1  ; $0 .rs 1 means reserve one byte of space
buttons1   .rs 1  ; $1 player 1 gamepad buttons, one bit per button
buttons2   .rs 1  ; $2 player 2 gamepad buttons, one bit per button
score1     .rs 1  ; $3 player 1 score, 0-15
score2     .rs 1  ; $4 player 2 score, 0-15

; GAMESTATE INTRO
framecounter1 .rs 1  ; $5 count nmi frames. 60 frames per sec
framecounter2 .rs 1  ; $6 count nmi frames. 1 frame per sec
framecounter3 .rs 1  ; $7 slowly counts up from $0
counter .rs 1        ; $8 counts down from $ff. 60 frames per sec
logohasplayed .rs 1  ; $9 intro has played
backgroundhasloaded .rs 1 ; $A background has loaded
g_done .rs 1         ; $B g has done its thing
planespawn .rs 1     ; $C have the planes been spawned
skipintro .rs 1      ; $D
skipintrocount .rs 1 ; $E
game_active .rs 1 ; $F is gameplay currently active

; GAMESTATE READY2GO



; GAMESTATE PLAYING

; good sprites:
player_x  .rs 1  ; $E x-pos of player sprite tile 4/4
player_y  .rs 1  ; $F y-pos of player sprite tile 4/4
player_h .rs 1   ; $10 player health
player_alive .rs 1 ; S11 player is alive if set to 1
player_l .rs 1 ; $12 player lives
missile_x .rs 1 ; missile x-pos
missile_y .rs 1 ; missile y-pos
missile_h .rs 1 ; missile health
bullet_x .rs 1 ; bullet x-pos
bullet_y .rs 1 ; bullet y-pos
bullet_h .rs 1 ; bullet health
;bad sprites;
enemy_x .rs 1 ; enemy x-pos sprite tile 4/4
enemy_y .rs 1 ; enemy y-pos sprite tile 4/4
enemy_h .rs 1 ; enemy health
enemy_alive .rs 1 ; is enemy alive or dead. 0 hes dead, 1 hes alive
respawntimer .rs 1 ; timer for enemy respawn

;other stuff
boost .rs 1 ; variable stores if boost has been applied. it is reset after having added boost to the sprites movement.
fired .rs 1 ; variable stores if the missile been fired. it is reset after leaving the screen.
fade .rs 1 ; not sure.... had to do with sound from fired missile

sprite .rs 1 ; sprite iteration (animation)
frame .rs 1 ; keep track of frame for animation
gametime .rs 1 ; keep track of gametime (shortterm, up to 255 frames, around 4seconds)
distance_x .rs 1 ; collision distance
distance_y .rs 1 ; collision distance



; GAMESTATE GAMEOVER
player_died .rs 1 ; player has died if set to 1
restartloopcounter .rs 1 ; loop counter after player death
restart_bit .rs 1 ; restart after crash





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
  

  LDX #$00
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$ff              ; Compare X to hex $80, decimal 128. loads the first 128 bytes of sprites (32 sprites)
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
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
;good sprites
  lda #$70
  sta player_y
  lda #$10
  sta player_x
  lda #$64 ; 100 health
  sta player_h
  lda #$5 ; 5 lives
  sta player_l
  lda #$14  ; missile has 20 health
  sta missile_h
  lda #$5 ; bullet has 5 health
  sta bullet_h
;bad sprites
;  lda #$c8 ; enemy has 200 health
  lda #$70
  sta enemy_h
  lda #$1 ; enemy spawn state at beginning
  sta enemy_alive

;;:Set starting game state
  LDA #STATEINTRO
  STA gamestate




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

ppu: 
; PPU registers  
  LDA #%10000000   ; enable NMI, sprite size 8x8, sprites from Pattern Table 0, base nametable address $2000
  STA $2000
  LDA #%00010110   ; enable sprites, disable background, show sprites in leftmost 8 pixels of screen, show background in leftmost 8 pixels of screen
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


  
  
DrawScore:
  ;;draw score on screen using background tiles
  ;;or using many sprites
  RTS
  
UpdateSpritePosition:
  LDA player_y
  STA $0208 ; write to y-pos of sprite 3/4
  STA $020C ; write to y-pos of sprite 4/4
  CLC ; clear carry
  SBC #$7 ; account for shift of tile location in *.chr . add #$8 to x
  STA $0200 ; write to y-pos of sprite 1/4
  STA $0204 ; write to y-pos of sprite 2/4

  LDA player_x
  STA $0207 ; x-pos of sprite 2/4
  STA $020F ; x-pos of sprite 4/4
  CLC
  SBC #$7
  STA $0203 ; x-pos of sprite 1/4
  STA $020B ; x-pos of sprite 3/4
  RTS
  

;;;;;;;;;;;

NMI: ; called 60 times per second
; [RENDER]  
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  JSR DrawScore

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



  
  dec counter ; 60 fps counting down
  inc framecounter1 ; 60 fps counting up

  lda framecounter1
  cmp #$3c ; compare to decimal 60, hex 3c, (1 second)
  bne nmi_end ; branch until counter has reached $30
  inc framecounter2 ; 1 fps counting up
  lda #$0
  sta framecounter1 ; reset framecounter1 to 0

nmi_end:
  RTI

;;;;;;;;;;;;;;;

;;;;;;;;
 
EngineIntro:



  lda skipintro ; load skipintro value to check if intro needs to be skipped
  cmp #$1
  bne runintro   ; branch to runintro if skipintro is 0



runintro: 

  lda logohasplayed
  cmp #$01    ; 1st run: 0-1=-1
  beq logo_end ; branch if logohasplayed is 1. only need it to be run once.

  lda framecounter1
  cmp #$30
  bne framecounter_end ; branch until counter has reached $30

; play game logo jingle
  lda #130
  sta $4006
  lda #200
  sta $4007
  lda #%10011111
  sta $4004

; move game letters onto screen
  lda #$72 ; y-position of g, a, m and e letters
  sta $224 ; g
  sta $228 ; a
  sta $22c ; m
  sta $230 ; e
  lda #$1
  sta logohasplayed  ; set logohasplayed from 0 to 1, to note that intro has played.

framecounter_end:


logo_end:   ; branch here if logo has played  

  lda framecounter2
  cmp #$4
  bne framecounter2a_end ; branch until counter has reached $6

; enable background
  LDA #%00011110   ; enable sprites, enable background, show sprites in leftmost 8 pixels of screen, show background in leftmost 8 pixels of screen
  STA $2001
  ; 7,6,5 color emphasis (BGR)
  ; 4 sprite enable (s)
  ; 3 background enable (b)
  ; 2 sprite left column enable (M)
  ; 1 background left column enable (m)
  ; 0 greyscale (G)
  LDA #$1
  sta backgroundhasloaded

framecounter2a_end:

  lda framecounter2
  cmp #$6
  bne framecounter2b_end ; branch until counter has reached $6
; prepare for ready2go





framecounter2b_end: ; end of intro state engine

  LDA backgroundhasloaded  ; background has loaded and game can be started
  CMP #$1
  BNE ReadStartDone
ReadStart: 
  LDA buttons1       ; load player 1 - buttons
  AND #%00010000  ; only look at bit 4
  BEQ ReadStartDone   ; branch to ReadStartDone if button is NOT pressed (0)
  LDA #STATEREADY2GO
  STA gamestate
ReadStartDone:
  
  ;;if start button pressed
  ;;  remove Game logo off screen
  ;;  load pre-game screen
  ;;  go to ready2go state
  
  JMP GameEngineDone


 ;;;;;;;;; 
 
EngineReady2Go:
  ;;if start button pressed
  ;;  turn screen off
  ;;  load title screen
  ;;  go to Title State
  ;;  turn screen on 

; clearscreen
  ; move game letters off screen
  lda #$f0 ; y-position of g, a, m and e letters
  sta $224 ; g
  sta $22c ; m
  
; spawn player tiles offscreen
  lda #$f0 ; y-pos
  sta $200 ; tile 1/4
  sta $204 ; tile 2/4
  lda #$f0
  sta $208 ; tile 3/4
  sta $20c ; tile 4/4 <-- collision tracking
  lda #$f0 ; x-pos
  sta $203 ; tile 1/4
  sta $20b ; tile 3/4
  lda #$f0 
  sta $207 ; tile 2/4
  sta $20f ; tile 4/4

; move player on screen
  
  LDA player_y
  STA $0208 ; write to y-pos of sprite 3/4
  STA $020C ; write to y-pos of sprite 4/4
  CLC ; clear carry
  SBC #$7 ; account for shift of tile location in *.chr . add #$8 to x
  STA $0200 ; write to y-pos of sprite 1/4
  STA $0204 ; write to y-pos of sprite 2/4

  LDA player_x
  STA $0207 ; x-pos of sprite 2/4
  STA $020F ; x-pos of sprite 4/4
  CLC
  sbc #$7
  STA $0203 ; x-pos of sprite 1/4
  STA $020B ; x-pos of sprite 3/4


; display ready2go message
  lda #$50 ; y-pos
  sta $2e4 ; R
  sta $230 ; E
  sta $228 ; A
  sta $2e8 ; D
  sta $2ec ; Y
  sta $2f0 ; !
  lda #$50 ; x-pos
  sta $2e7 ; R
  lda #$58
  sta $233 ; E
  lda #$60
  sta $22b ; A
  lda #$68
  sta $2eb ; D
  lda #$70
  sta $2ef ; Y
  lda #$78
  sta $2f3 ; !

  lda #$50 ; y-pos
  sta $2b0 ; A/B Button
  sta $2b4 ; big arrow  
  
  lda #$58 ; y-pos
  sta $2b8 ; A
  sta $2bc ; B

  lda #$1
  sta planespawn ; plane has been spawned

  LDA planespawn  ; background has loaded and game can be started
  CMP #$1
  BNE ReadBDone
ReadA: 
  LDA buttons1       ; load player 1 - buttons
  AND #%10000000  ; only look at bit 7
  BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
  LDA #STATEPLAYING
  STA gamestate
ReadADone:
ReadB: 
  LDA buttons1       ; load player 1 - buttons
  AND #%01000000  ; only look at bit 6
  BEQ ReadBDone   ; branch to ReadBDone if button is NOT pressed (0)
  LDA #STATEPLAYING
  STA gamestate
ReadBDone:


  
  JMP GameEngineDone



;;;;;;;;;;;
 
EnginePlaying:

  LDA game_active
  CMP #$1
  BEQ active
; clear screen (run once)
; start stuff
  lda #$F0
  sta $2e4 ; R
  sta $230 ; E
  sta $228 ; A
  sta $2e8 ; D
  sta $2ec ; Y
  sta $2f0 ; !
  sta $2b0 ; A/B Button
  sta $2b4 ; big arrow  
  sta $2b8 ; A
  sta $2bc ; B
  lda #$1
  sta game_active
 

active:


  LDA buttons1       ; load player 1 - buttons
  AND #%00001000  ; only look at bit 3
  BEQ ReadUpDone   ; branch to ReadUpDone if button is NOT pressed (0)
; move guy up 
  LDA player_y
  CMP #$b ; 
  BEQ ReadUpDone ; if equal to zero, go to readupdone
  DEC player_y
  STA $0208 ; write to y-pos of sprite 3/4
  STA $020C ; write to y-pos of sprite 4/4
  CLC ; clear carry
  SBC #$7 ; account for shift of tile location in *.chr . add #$8 to x
  STA $0200 ; write to y-pos of sprite 1/4
  STA $0204 ; write to y-pos of sprite 2/4
ReadUpDone:

  LDA buttons1       ; load player 1 - buttons
  AND #%00000100  ; only look at bit 2
  BEQ ReadDownDone   ; branch to ReadDownDone if button is NOT pressed (0)
; move guy down 
  LDA player_y 
  CMP #$E3 ; ; if it has exceeded y position of xxx, ignore further movement in y axis downwards
  BEQ ReadDownDone ; if equal to zero, go to readdowndone
  INC player_y
  STA $0208 ; write to y-pos of sprite 3/4
  STA $020C ; write to y-pos of sprite 4/4
  CLC
  SBC #$7
  STA $0200 ; write to y-pos of sprite 1/4
  STA $0204 ; write to y-pos of sprite 2/4
ReadDownDone:

  LDA buttons1       ; load player 1 - buttons
  AND #%00000010  ; only look at bit 1
  BEQ ReadLeftDone   ; branch to ReadLeftDone if button is NOT pressed (0)
; Move guy to the left 
  LDA player_x
  CMP #$8  ; changed from SBC #$5
  BEQ ReadLeftDone ; if equal to zero, go to readleftdone
  DEC player_x
  STA $0207 ; x-pos of sprite 2/4
  STA $020F ; x-pos of sprite 4/4
  clc
  sbc #$7
  STA $0203 ; x-pos of sprite 1/4
  STA $020B ; x-pos of sprite 3/4
ReadLeftDone:

  LDA buttons1       ; load player 1 - buttons
  AND #%00000001  ; only look at bit 0
  BEQ ReadRightDone ; branch to ReadRightDone if button is NOT pressed (0)
; Move guy to the right  
  LDA player_x
  CMP #$F8
  BEQ ReadRightDone
  INC player_x
  STA $0207 ; write to x-pos of sprite 2/4
  STA $020F ; write to x-pos of sprite 4/4
  clc
  sbc #$7
  STA $0203 ; write to x-pos of sprite 1/4
  STA $020B ; write to x-pos of sprite 3/4
ReadRightDone:


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
sprites:
player:
  .db $f0, $0A, %00000000, $f0   ;sprite 1/4: player
  .db $f0, $0B, %00000000, $f0   ;sprite 2/4: player
  .db $f0, $1A, %00000000, $f0   ;sprite 3/4: player
  .db $f0, $1B, %00000000, $f0   ;sprite 4/4: player << collision detection configured on this tile
  .db $f0, $0E, %00000001, $CD   ;missile (y 220, x 205)
tiefighter:
  .db $f0, $4a, %00000000, $32 ; tiefighter 1/4
  .db $f0, $4b, %00000000, $3a ; tiefighter 2/4
  .db $f0, $5a, %00000000, $32 ; tiefighter 3/4
  .db $f0, $5b, %00000000, $3a ; tiefighter 4/4
game:
  .db $f0, tG, %00000000, $68 ; G
  .db $f0, tA, %00000000, $70 ; A
  .db $f0, tM, %00000000, $78 ; M
  .db $f0, tE, %00000000, $80 ; E
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
  