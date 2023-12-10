  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring


;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
oamL .rs 1        ; starts at 00, up to FF
oamH .rs 1        ; fixed at 02 
gamestate  .rs 1  ; $0 .rs 1 means reserve one byte of space
buttons1   .rs 1  ; $1 player 1 gamepad buttons, one bit per button
buttons2   .rs 1  ; $2 player 2 gamepad buttons, one bit per button  
laserframe .rs 1     ; frame count for synchronising laser firing

framecounter1 .rs 1  ; $5 count nmi frames. 60 frames per sec
framecounter2 .rs 1  ; $6 count nmi frames. 1 frame per sec
framecounter3 .rs 1  ; $7 slowly counts up from $0

; GAMESTATE PLAYING
p1 .rs 1        ; player 1 low byte oam address of 1st tile eg #$10: $0210
p1s .rs 1       ; size of player 1 object, number of tiles
p1l .rs 1		; player 1 lives
p1y .rs 1       ; player 1 y-pos (center of cel)
p1x .rs 1       ; x-pos (center of cel)
p1.0t .rs 1     ; tile number 0
p1.1t .rs 1     ; tile number 1
p1.2t .rs 1     ; tile number 2
p1.3t .rs 1     ; tile number 3
p1.0a .rs 1     ; attribute tile 0
p1.1a .rs 1     ; attribute tile 1
p1.2a .rs 1     ; attribute tile 2
p1.3a .rs 1     ; attribute tile 3
p1.0h .rs 1     ; tile 0 health
p1.1h .rs 1     ; tile 1 health
p1.2h .rs 1     ; tile 2 health
p1.3h .rs 1     ; tile 3 health
; 76543210
; 5,4,3,2,1,0 ; health 0-63
; 6 : damaged
; 7 : dead

e1 .rs 1        ; enemy 1 low byte oam address of 1st tile
e1s .rs 1       ; size of enemy 1 object, number of tiles
e1y .rs 1       
e1x .rs 1
e1.0t .rs 1
e1.1t .rs 1
e1.2t .rs 1
e1.3t .rs 1
e1.0a .rs 1
e1.1a .rs 1
e1.2a .rs 1
e1.3a .rs 1
e1h .rs 1       ; health 0-255
e1e .rs 1       ; extended attributes
; 76543210
; 4,3,2,1,0 ; 
; 5 : 
; 6 : is currently homing
; 7 : is currently firing


e2 .rs 1        ; enemy 2 low byte oam address of 1st tile
e2s .rs 1       ; size of enemy 2 object, number of tiles
e2y .rs 1       
e2x .rs 1
e2.0t .rs 1
e2.1t .rs 1
e2.2t .rs 1
e2.3t .rs 1
e2.0a .rs 1
e2.1a .rs 1
e2.2a .rs 1
e2.3a .rs 1
e2h .rs 1       ; health 0-255
e2e .rs 1       ; extended attributes
; 76543210
; 4,3,2,1,0 ; 
; 5 : 
; 6 : is currently homing
; 7 : is currently firing

e3 .rs 1        ; enemy 3 low byte oam address of 1st tile
e3s .rs 1       ; size of enemy 3 object, number of tiles
e3y .rs 1       
e3x .rs 1
e3.0t .rs 1
e3.1t .rs 1
e3.2t .rs 1
e3.3t .rs 1
e3.0a .rs 1
e3.1a .rs 1
e3.2a .rs 1
e3.3a .rs 1
e3h .rs 1       ; health 0-255
e3e .rs 1       ; extended attributes
; 76543210
; 4,3,2,1,0 ; 
; 5 : 
; 6 : is currently homing
; 7 : is currently firing



m1 .rs 1
m1s .rs 1       ; size of object, number of tiles
m1y .rs 1       ; missile 1 y-pos
m1x .rs 1
m1t .rs 1
m1a .rs 1
m1h .rs 1       ; health 0-255
m1e .rs 1       ; extended attributes
; 76543210
; 5,4,3,2,1,0 ; 
; 6 : 
; 7 : has been fired

l1 .rs 1
l1s .rs 1       ; size of object, number of tiles
l1y .rs 1       ; laser 1 y-pos
l1x .rs 1
l1t .rs 1
l1a .rs 1
l1h .rs 1
l1e .rs 1       ; extended attributes
; 76543210
; 5,4,3,2,1,0 ; 
; 6 : 
; 7 : has been fired

l2 .rs 1
l2s .rs 1       ; size of object, number of tiles
l2y .rs 1       ; laser 1 y-pos
l2x .rs 1
l2t .rs 1
l2a .rs 1
l2h .rs 1
l2e .rs 1       ; extended attributes
; 76543210
; 5,4,3,2,1,0 ; 
; 6 : 
; 7 : has been fired

yd .rs 1  ; y delta
xd .rs 1  ; x delta
ysequal .rs 1  ; y1 and y2 are equal
y1small .rs 1  ; y1 is smaller than y2
y2small .rs 1  ; y2 is smaller than y1
xsequal .rs 1  ; x1 and x2 are equal
x1small .rs 1  ; x1 is smaller than x2
x2small .rs 1  ; x2 is smaller than x1
prox .rs 1     ; proximity limit for collision
enemy_yr .rs 1 ; enemy reaction delay
enemy_homing .rs 1 ; enemy is homing in on player
player_enemy_collision .rs 1

;; DECLARE SOME CONSTANTS HERE
STATEINTRO     = $00  ; display into screen
STATEREADY2GO  = $01  ; displaying ready to go screen
STATEPLAYING   = $02  ; move sprite, check for collisions
STATEGAMEOVER  = $03  ; displaying game over screen


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
.loop:
  LDA palette, x        ; load data from address (palette + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE .loop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero





LoadINBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
 
  LDX #$00              ; start out at 0
.loop1:
  LDA INbackground1, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00           ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE .loop1  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
  LDX #$00 
.loop2:
  LDA INbackground2, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00           ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE .loop2  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
  LDX #$00 
.loop3:
  LDA INbackground3, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00           ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE .loop3  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
  LDX #$00 
.loop4:
  LDA INbackground4, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$a0           ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE .loop4  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero

LoadINAttribute:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
.loop:
  LDA INattribute, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$08              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE .loop


  lda #$02
  sta oamH

;; STARTING VARIABLES
;good sprites

  lda #$0           ; reserve oam 0-16 for player 1
  sta p1
  lda #$10          ; increment next available oam address by #$10
  sta oamL
  lda #$10          ; p1 uses #$10 of oam address space
  sta p1s
  lda #$70
  sta p1y
  lda #$10
  sta p1x
  lda #$a
  sta p1.0t
  lda #$b
  sta p1.1t
  lda #$1a
  sta p1.2t
  lda #$1b
  sta p1.3t
  lda #%00000010
  sta p1.0a
  sta p1.1a
  sta p1.2a
  sta p1.3a
  lda #$40 ; 64 health
  sta p1.0h
  sta p1.1h
  sta p1.2h
  sta p1.3h
  lda #$5 ; 5 lives
  sta p1l
  
  lda #$4
  sta m1s
  lda #$dc
  sta m1y
  lda #$cd
  sta m1x
  lda #$e
  sta m1t
  lda #%00000001
  sta m1a
  lda #$14  ; missile has 20 health (ie does 20 damage)
  sta m1h
  
;bad sprites
  lda #$10
  sta e1s   ; e1 uses #$10 of oam address space
  lda #$50
  sta e1y
  lda #$c8
  sta e1x
  lda #$4a
  sta e1.0t
  lda #$4b
  sta e1.1t
  lda #$5a
  sta e1.2t
  lda #$5b
  sta e1.3t
  lda #%00000000
  sta e1.0a
  sta e1.1a
  sta e1.2a
  sta e1.3a
  lda #$60  ; enemy starting health
  sta e1h
  
  lda #$10
  sta e2s   ; e2 uses #$10 of oam address space
  lda #$60
  sta e2y
  lda #$cc
  sta e2x
  lda #$4a
  sta e2.0t
  lda #$4b
  sta e2.1t
  lda #$5a
  sta e2.2t
  lda #$5b
  sta e2.3t
  lda #%00000000
  sta e2.0a
  sta e2.1a
  sta e2.2a
  sta e2.3a
  lda #$60 ; enemy starting health
  sta e2h 

  lda #$10
  sta e3s   ; e3 uses #$10 of oam address space
  lda #$40
  sta e3y
  lda #$cc
  sta e3x
  lda #$4a
  sta e3.0t
  lda #$4b
  sta e3.1t
  lda #$5a
  sta e3.2t
  lda #$5b
  sta e3.3t
  lda #%00000000
  sta e3.0a
  sta e3.1a
  sta e3.2a
  sta e3.3a
  lda #$60 ; enemy starting health
  sta e3h 

  lda #$4
  sta l1s   ; l1 uses #$4 of oam address space 
  lda #$4
  sta l1h   ; laser has 4 health
  sta l2h   ; laser has 4 health
; neutral  
  lda #$8
  sta prox           ; store collision limit - no longer used i think






init_audio:
  ldx #$00
 ; initialize sound registers 
.loop:
  lda #$00
  sta $4000, x
  inx
  cpx #$14
  bne .loop

  lda #$00
  sta $4015
  lda #$0F
  sta $4015
; setup apu frame counter
  lda #$40
  sta $4017



  

  
;;:Set starting game state
  LDA #STATEREADY2GO
  STA gamestate
  
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
  
start:

  LDA #STATEPLAYING
  STA gamestate

  RTS
  

;;;;;;;;;;;

NMI: ; called 60 times per second
; [RENDER]  
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer


  
  

;render all frames



;render odd frames


;render even frames







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
  
;  dec counter ; 60 fps counting down
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


  
  JMP GameEngineDone


 ;;;;;;;;; 
 
EngineReady2Go:


  ReadStart: 
  LDA buttons1       ; load player 1 - buttons
  AND #%00010000  ; only look at bit 4
  BEQ ReadStartDone   ; branch to ReadStartDone if button is NOT pressed (0)
 
  jsr start

ReadStartDone:


  JMP GameEngineDone



;;;;;;;;;;;
 
EnginePlaying:

  lda p1
  sta oamL

  LDA buttons1       ; load player 1 - buttons
  AND #%00001000  ; only look at bit 3
  BEQ ReadUpDone   ; branch to ReadUpDone if button is NOT pressed (0)
; move guy up 
  LDA p1y
  CMP #$b ; 
  BEQ ReadUpDone ; if equal to zero, go to readupdone
  DEC p1y
  LDA p1y

  ldy #$8
  STA [oamL], y    ; write to y-pos of sprite 3/4
  ldy #$c
  STA [oamL], y    ; write to y-pos of sprite 4/4
  SEC              ; set carry
  SBC #$8          ; account for shift of tile location in *.chr . add #$8 to x
  ldy #$0
  STA [oamL], y    ; write to y-pos of sprite 1/4
  ldy #$4
  STA [oamL], y    ; write to y-pos of sprite 2/4
  
ReadUpDone:

  LDA buttons1       ; load player 1 - buttons
  AND #%00000100  ; only look at bit 2
  BEQ ReadDownDone   ; branch to ReadDownDone if button is NOT pressed (0)
; move guy down 
  LDA p1y 
  CMP #$E3 ; ; if it has exceeded y position of xxx, ignore further movement in y axis downwards
  BEQ ReadDownDone ; if equal to zero, go to readdowndone
  INC p1y
  LDA p1y

  ldy #$8
  STA [oamL], y    ; write to y-pos of sprite 3/4
  ldy #$c
  STA [oamL], y    ; write to y-pos of sprite 4/4
  SEC              ; set carry
  SBC #$8          ; account for shift of tile location in *.chr . add #$8 to x
  ldy #$0
  STA [oamL], y    ; write to y-pos of sprite 1/4
  ldy #$4
  STA [oamL], y    ; write to y-pos of sprite 2/4
  
ReadDownDone:

  LDA buttons1       ; load player 1 - buttons
  AND #%00000010  ; only look at bit 1
  BEQ ReadLeftDone   ; branch to ReadLeftDone if button is NOT pressed (0)
; Move guy to the left 
  LDA p1x
  CMP #$8  ; changed from SBC #$5
  BEQ ReadLeftDone ; if equal to zero, go to readleftdone
  DEC p1x
  LDA p1x

  ldy #$7
  STA [oamL], y    ; x-pos of sprite 2/4
  ldy #$f
  STA [oamL], y    ; x-pos of sprite 4/4
  SEC
  sbc #$8
  ldy #$3
  STA [oamL], y    ; x-pos of sprite 1/4
  ldy #$b
  STA [oamL], y    ; x-pos of sprite 3/4 
  
ReadLeftDone:

  LDA buttons1       ; load player 1 - buttons
  AND #%00000001  ; only look at bit 0
  BEQ ReadRightDone ; branch to ReadRightDone if button is NOT pressed (0)
; Move guy to the right  
  LDA p1x
  CMP #$F8
  BEQ ReadRightDone
  INC p1x
  LDA p1x

  ldy #$7
  STA [oamL], y    ; x-pos of sprite 2/4
  ldy #$f
  STA [oamL], y    ; x-pos of sprite 4/4
  SEC
  sbc #$8
  ldy #$3
  STA [oamL], y    ; x-pos of sprite 1/4
  ldy #$b
  STA [oamL], y    ; x-pos of sprite 3/4  
  
ReadRightDone:

;  jmp frameend       ; to skip drawing sprites

  lda framecounter3
  cmp #$1
  beq odd
  jmp even
  
odd:
  dec framecounter3

  lda #$10
  sta oamL
  
  lda oamL    ; load available oam address
  sta e1      ; store it for this object
  LDA e1y
  ldy #$8
  STA [oamL], y   ; write to y-pos of sprite 3/4
  ldy #$c
  STA [oamL], y   ; write to y-pos of sprite 4/4
  SEC             ; set carry
  SBC #$8         ; account for shift of tile location in *.chr . add #$8 to x
  ldy #$0
  STA [oamL], y   ; write to y-pos of sprite 1/4
  ldy #$4
  STA [oamL], y   ; write to y-pos of sprite 2/4

  LDA e1.0t
  ldy #$1
  sta [oamL], y
  LDA e1.1t
  ldy #$5
  sta [oamL], y
  LDA e1.2t
  ldy #$9
  sta [oamL], y
  LDA e1.3t
  ldy #$d
  sta [oamL], y

  LDA e1.0a
  ldy #$2
  sta [oamL], y
  ldy #$6
  sta [oamL], y
  ldy #$a
  sta [oamL], y
  ldy #$e
  sta [oamL], y

  LDA e1x
  ldy #$7
  STA [oamL], y   ; x-pos of sprite 2/4
  ldy #$f
  STA [oamL], y   ; x-pos of sprite 4/4
  SEC
  SBC #$8
  ldy #$3
  STA [oamL], y   ; x-pos of sprite 1/4
  ldy #$b
  STA [oamL], y   ; x-pos of sprite 3/4
;update oam
  lda e1
  clc         ; clear carry
  adc e1s    ; add #$10 (size of this object) to increment available oam address
  sta oamL    ; store updated available oam address

  lda oamL    ; load available oam address
  sta e2      ; store it for this object
  LDA e2y
  ldy #$8
  STA [oamL], y   ; write to y-pos of sprite 3/4
  ldy #$c
  STA [oamL], y   ; write to y-pos of sprite 4/4
  SEC             ; set carry
  SBC #$8         ; account for shift of tile location in *.chr . add #$8 to x
  ldy #$0
  STA [oamL], y   ; write to y-pos of sprite 1/4
  ldy #$4
  STA [oamL], y   ; write to y-pos of sprite 2/4

  LDA e2.0t
  ldy #$1
  sta [oamL], y
  LDA e2.1t
  ldy #$5
  sta [oamL], y
  LDA e2.2t
  ldy #$9
  sta [oamL], y
  LDA e2.3t
  ldy #$d
  sta [oamL], y

  LDA e2.0a
  ldy #$2
  sta [oamL], y
  ldy #$6
  sta [oamL], y
  ldy #$a
  sta [oamL], y
  ldy #$e
  sta [oamL], y

  LDA e2x
  ldy #$7
  STA [oamL], y   ; x-pos of sprite 2/4
  ldy #$f
  STA [oamL], y   ; x-pos of sprite 4/4
  SEC
  SBC #$8
  ldy #$3
  STA [oamL], y   ; x-pos of sprite 1/4
  ldy #$b
  STA [oamL], y   ; x-pos of sprite 3/4
;update oam
  lda e2
  clc         ; clear carry
  adc e2s    ; add #$10 (size of this object) to increment available oam address
  sta oamL    ; store updated available oam address


  lda oamL    ; load available oam address
  sta e3      ; store it for this object
  LDA e3y
  ldy #$8
  STA [oamL], y   ; write to y-pos of sprite 3/4
  ldy #$c
  STA [oamL], y   ; write to y-pos of sprite 4/4
  SEC             ; set carry
  SBC #$8         ; account for shift of tile location in *.chr . add #$8 to x
  ldy #$0
  STA [oamL], y   ; write to y-pos of sprite 1/4
  ldy #$4
  STA [oamL], y   ; write to y-pos of sprite 2/4

  LDA e3.0t
  ldy #$1
  sta [oamL], y
  LDA e3.1t
  ldy #$5
  sta [oamL], y
  LDA e3.2t
  ldy #$9
  sta [oamL], y
  LDA e3.3t
  ldy #$d
  sta [oamL], y

  LDA e3.0a
  ldy #$2
  sta [oamL], y
  ldy #$6
  sta [oamL], y
  ldy #$a
  sta [oamL], y
  ldy #$e
  sta [oamL], y

  LDA e3x
  ldy #$7
  STA [oamL], y   ; x-pos of sprite 2/4
  ldy #$f
  STA [oamL], y   ; x-pos of sprite 4/4
  SEC
  SBC #$8
  ldy #$3
  STA [oamL], y   ; x-pos of sprite 1/4
  ldy #$b
  STA [oamL], y   ; x-pos of sprite 3/4
;update oam
  lda e3
  clc         ; clear carry
  adc e3s    ; add #$10 (size of this object) to increment available oam address
  sta oamL    ; store updated available oam address
  
  jmp frameend

even:  
  inc framecounter3
  
; move player on screen
  lda p1
  sta oamL
  LDA p1y
  ldy #$8
  STA [oamL], y    ; write to y-pos of sprite 3/4
  ldy #$c
  STA [oamL], y    ; write to y-pos of sprite 4/4
  SEC              ; set carry
  SBC #$8          ; account for shift of tile location in *.chr . add #$8 to x
  ldy #$0
  STA [oamL], y    ; write to y-pos of sprite 1/4
  ldy #$4
  STA [oamL], y    ; write to y-pos of sprite 2/4

  LDA p1.0t
  ldy #$1
  sta [oamL], y
  LDA p1.1t
  ldy #$5
  sta [oamL], y
  LDA p1.2t
  ldy #$9
  sta [oamL], y
  LDA p1.3t
  ldy #$d
  sta [oamL], y

  LDA p1.0a
  ldy #$2
  sta [oamL], y
  ldy #$6
  sta [oamL], y
  ldy #$a
  sta [oamL], y
  ldy #$e
  sta [oamL], y
 
  LDA p1x
  ldy #$7
  STA [oamL], y    ; x-pos of sprite 2/4
  ldy #$f
  STA [oamL], y    ; x-pos of sprite 4/4
  SEC
  SBC #$8
  ldy #$3
  STA [oamL], y    ; x-pos of sprite 1/4
  ldy #$b
  STA [oamL], y    ; x-pos of sprite 3/4    
;update oam
  lda p1
  clc         ; clear carry
  adc p1s    ; add #$10 (size of this object) to increment available oam address
  sta oamL    ; store updated available oam address
   
; place missile on screen
  lda oamL
  sta m1
  LDA m1y
  ldy #$0
  STA [oamL], y    ; write to y-pos of sprite 1/4


  LDA m1t
  ldy #$1
  sta [oamL], y

  LDA m1a
  ldy #$2
  sta [oamL], y
 
  LDA m1x
  ldy #$3
  STA [oamL], y    ; x-pos of sprite 1/4
;update oam
  lda m1
  clc         ; clear carry
  adc m1s     ; add #$4 (size of this object) to increment available oam address
  sta oamL    ; store updated available oam address
  

  
frameend:

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
;player:
  .db $f0, $0A, %00000010, $f0   ;sprite 1/4: player
  .db $f0, $0B, %00000010, $f0   ;sprite 2/4: player
  .db $f0, $1A, %00000010, $f0   ;sprite 3/4: player
  .db $f0, $1B, %00000010, $f0   ;sprite 4/4: player << collision detection configured on this tile
  .db $f0, $0E, %00000001, $CD   ;missile (y 220, x 205)
;tiefighter:
  .db $f0, $4a, %00000000, $fe ; tiefighter 1/4
  .db $f0, $4b, %00000000, $ff ; tiefighter 2/4
  .db $f0, $5a, %00000000, $fe ; tiefighter 3/4
  .db $f0, $5b, %00000000, $ff ; tiefighter 4/4 << collision detection configured on this tile
;laser:
  .db $f0, $00, %00000011, $ff ; laser1
  .db $f0, $00, %00000011, $ff ; laser2



oam:
  .db $8,$c,$0,$4
  .db $1,$5,$9,$d
  .db $2,$6,$a,$e
  .db $7,$f,$3,$b
  
;VARIABLES
health:         ; health table
  .db p1,p1.0h,p1.1h,p1.2h,p1.3h
  .db e1,e1h
  .db e2,e2h
  .db m1,m1h
  .db l1,l1h
  .db l2,l2h

  
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
    