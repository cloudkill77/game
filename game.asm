  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring


;;;;;;;;;;;;;;;
;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
; .rs 1 means reserve one byte of space.

; good sprites:
player_x  .rs 1  ;  x-pos of player sprite tile 4/4
player_y  .rs 1  ; y-pos of player sprite tile 4/4
player_h .rs 1 ; player health
player_l .rs 1 ; player lives
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

player_dead .rs 1 ; player has died if set to 1
restartloopcounter .rs 1 ; loop counter after player death
restart_bit .rs 1 ; restart after crash

;;;;;;;;;;;;;;;

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
   
   
LoadPalette:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down

  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$74              ; Compare X to hex $14, decimal 20. loads the first 20 bytes of sprites (5 sprites)
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to zero, keep going down
                       ; if compare was equal to zero, keep going down 
   
;  LDX #$00              ; start at 0 
; LoadSpriteArrayLoop:
;  LDA sprite_array, x        ; load data from address (sprites +  x)
;  STA $0214, x          ; store into RAM address ($0200 + x)
;  INX                   ; X = X + 1
;  CPX #$28              ; Compare X to hex $28, decimal 40. loads the first 20 bytes of sprites (5 sprites)
;  BNE LoadSpriteArrayLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to zero, keep going down 

LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
 
  LDX #$00              ; start out at 0
LoadBackgroundLoop1:
  LDA background1, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00           ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadBackgroundLoop1  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
  LDX #$00 
LoadBackgroundLoop2:
  LDA background2, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00           ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadBackgroundLoop2  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
  LDX #$00 
LoadBackgroundLoop3:
  LDA background3, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00           ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadBackgroundLoop3  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
  LDX #$00 
LoadBackgroundLoop4:
  LDA background4, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$a0           ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadBackgroundLoop4  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero




start:
;good sprites
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
  lda #$60
  sta enemy_h



   
  
ppu: 
; PPU registers  
  LDA #%10000000   ; enable NMI, sprite size 8x8, sprites from Pattern Table 0, base nametable address $2000
  STA $2000
  
  LDA #%00011110   ; enable sprites, show sprites in leftmost 8 pixels of screen, show background in leftmost 8 pixels of screen
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
  
  
  LDA $020c ; load y-pos of player sprite 4/4
  STA player_y ; store y-pos in player_y variable  
  LDA $020f ; load x-pos of player sprite 4/4
  STA player_x ; store x-pos in player_x variable

  LDA $0220 ; load y-pos of enemy sprite 4/4
  STA enemy_y ; store y-pos in enemy_y variable
  LDA $0223 ; load x-pos of enemy sprite 4/4
  STA enemy_x ; store x-pos in enemy_x variable


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
 
  
Foreverloop:
  
  JMP Foreverloop     ;jump back to Forever, infinite loop


init_apu:
  lda #$00
  sta $4015
  lda #$0F
  sta $4015
; setup apu frame counter
  lda #$40
  sta $4017  
  rts
 

NMI: 
; [RENDER]  
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

; is player still alive?
  lda player_dead
  cmp #$1 ; branch if not set to 1
  bne gameon
  jmp LatchController

; [update positions]
  LDA $020c ; load y-pos of player sprite 4/4
  STA player_y ; store y-pos in player_y variable  
  LDA $020f ; load x-pos of player sprite 4/4
  STA player_x ; store x-pos in player_x variable
  LDA $0220 ; load y-pos of enemy sprite 4/4
  STA enemy_y ; store y-pos in enemy_y variable
  LDA $0223 ; load x-pos of enemy sprite 4/4
  STA enemy_x ; store x-pos in enemy_x variable
  
; [check collisions]
gameon:  
  clc
  lda #$00
  ldx #$00
  lda player_x
  clc
  sbc enemy_x
  sta distance_x
  CMP #$5     ;Accumulator less than $5?
  BCS clear   ;no, branch to clear
  lda player_y
  clc
  sbc enemy_y
  sta distance_y
  cmp #$5      ;Accumulator less than $5?
  BCS clear   ;no, branch to clear
  
; collision occurred
damagecheck:
  lda player_h
  clc
  sbc enemy_h
  sta player_h
  bmi dead
  lda #$00
  sta enemy_h
  sta enemy_alive  ; mark enemy as dead
  ; move enemy offscreen
  LDA #$f0
  STA $0214 ; y-pos of sprite 1/4
  STA $0218 ; y-pos of sprite 3/4
  STA $021c ; y-pos of sprite 2/4
  STA $0220 ; y-pos of sprite 4/4
 
  LDA $0220 ; load y-pos of enemy sprite 4/4
  STA enemy_y ; store y-pos in enemy_y variable
  LDA $0223 ; load x-pos of enemy sprite 4/4
  STA enemy_x ; store x-pos in enemy_x variable
  
  jmp clear
  
dead:
  ldx player_l
  DEX
  stx player_l
  lda #$1
  sta player_dead
  
clear: 
 ;check lives
  lda #$0
  sta $0238
  sta $0234
  sta $0230
  sta $022c
  sta $0228
  sta $0224

  clc
  lda player_l
  cmp #$5
  beq l5
  cmp #$4
  beq l4
  cmp #$3
  beq l3
  cmp #$2
  beq l2
  cmp #$1
  beq l1
  cmp #$0
  beq l0
  jmp livesok
  
l5:
  lda #$a
  sta $0238
  jmp livesok
  
l4:
  lda #$a
  sta $0234
  jmp livesok
  
l3:
  lda #$a
  sta $0230
  jmp livesok

l2:
  lda #$a
  sta $022c
  jmp livesok

l1:
  lda #$a
  sta $0228
  jmp livesok
  
l0:
  lda #$a
  sta $0224
  jmp livesok
  
livesok:

; move enemy across screen
;  LDA enemy_x
;  STA $0217 ; x-pos of sprite 1/4
;  STA $021f ; x-pos of sprite 3/4
;  TAX
;  CLC
;  sbc #$08; account for shift of tile location in *.chr . add #$8 to x
;  STA $021b ; x-pos of sprite 2/4
;  STA $0223 ; x-pos of sprite 4/4
;  DEX
;  STX enemy_x   

  LDA $0220 ; load y-pos of enemy sprite 4/4
  STA enemy_y ; store y-pos in enemy_y variable
  LDA $0223 ; load x-pos of enemy sprite 4/4
  STA enemy_x ; store x-pos in enemy_x variable 
 
; if it has exceeded x position of xxx, reset the missile
  LDA $0213 ; load x coordinates of missile sprite
  CMP #$F0 ; check if the coordinates equals screen out of bound area
  BEQ missilereset ; if result is zero, branch to missilereset label to reset the status of the missile
  CMP #$F1 ; check if the coordinates equals screen out of bound area
  BEQ missilereset ; if result is zero, branch to missilereset label to reset the status of the missile
  
 ;if it has been fired, add 2 to x-pos of missile $0213
  LDA fired
  BEQ nmi_end ; branch if equal to zero, branch to nmi_end if it hasnt been fired // check if this works without CMP
  LDA $0213 ; load current x-pos of missile
  CLC
  ADC #$2 ; if it has been fired, move the missile to the right
  STA $0213
  STA missile_x
  JMP nmi_end

;move the missile back to the initial position. reset the fired variable to 0 
missilereset:
  jsr init_apu ; reinitialize audio to stop the missile sound effect
  LDA #$CD ; $CD = 205
  STA $0213 ; set the x coordinates of the missile in the status bar
  LDA #$DC ; $DC = 220
  STA $0210 ; set the y coordinates of the missile in the status bar
  LDA #$0 ; 0
  STA fired ; reset status to missile as unfired

nmi_end:

LatchController:
  LDA #$01        ; store 1 in acumulator
  STA $4016       ; Write 1 to $4016 to signal the controller 1 to poll its input
  LDA #$00        ; store 0 in acumulator
  STA $4016       ; Write 0 to $4016 to finish the poll on controller 1

; sequence is *always* A, B, Select, Start, Up, Down, Left, Right. Controller 1: $4016, Controller 2: $4017

ReadA: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
;boost  
  LDA #$1 ; set boost to 1
  STA boost

ReadADone:        ; handling this button is done

ReadB: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadBDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
;fire logic
  LDA fired ; load value if it has been fired
  BNE ReadBDone ; branch if not equal to zero (if it has been fired, go to readbdone), otherwise, continue
  LDA #$1	; load #$1 into accumulator
  STA fired	; store 1 in variable "fired"
;generate fired missile sound effect 
  lda #130
  sta $4002
  lda #200
  sta $4003
  lda #%10111111
  sta $4000
  
  LDA player_x ; load x-pos of player
  STA missile_x ; set starting x-pos of missile
  STA $0213 ; store in x-pos of missile (starting position)
  LDA player_y ; load y-pos of player
  STA missile_y ; set starting y-pos of missile
  STA $0210 ; store in y-pos of missile (starting position)

ReadBDone:        ; handling this button is done

ReadSelect: 
  LDA $4016       ; player 1 - Select
  AND #%00000001  ; only look at bit 0
  BEQ ReadSelectDone   ; branch to ReadSelectDone if button is NOT pressed (0)
  jmp RESET
ReadSelectDone:

ReadStart: 
  LDA $4016       ; player 1 - Start
  AND #%00000001  ; only look at bit 0
  BEQ ReadStartDone   ; branch to ReadStartDone if button is NOT pressed (0)
  lda #$1
  sta restart_bit
ReadStartDone:

ReadUp: 
  LDA $4016       ; player 1 - Up
  AND #%00000001  ; only look at bit 0
  BEQ ReadUpDone   ; branch to ReadUpDone if button is NOT pressed (0)

; move guy up 
  LDA player_y
  CMP #$b ; // changed from sbc to CMP
  BEQ ReadUpDone ; if equal to zero, go to readupdone
 
  DEC player_y

  STA $0208 ; write to y-pos of sprite 3/4
  STA $020C ; write to y-pos of sprite 4/4
  CLC ; clear carry
  SBC #$08 ; account for shift of tile location in *.chr . add #$8 to x
  STA $0200 ; write to y-pos of sprite 1/4
  STA $0204 ; write to y-pos of sprite 2/4

ReadUpDone:

ReadDown: 
  LDA $4016       ; player 1 - Down
  AND #%00000001  ; only look at bit 0
  BEQ ReadDownDone   ; branch to ReadDownDone if button is NOT pressed (0)

; move guy down  
  LDA player_y
  CMP #$DB ; ; if it has exceeded y position of xxx, ignore further movement in y axis downwards
  BEQ ReadDownDone ; if equal to zero, go to readdowndone

  INC player_y

  STA $0208 ; write to y-pos of sprite 3/4
  STA $020C ; write to y-pos of sprite 4/4
  CLC
  SBC #$08
  STA $0200 ; write to y-pos of sprite 1/4
  STA $0204 ; write to y-pos of sprite 2/4

ReadDownDone:

ReadLeft: 
  LDA $4016       ; player 1 - Left
  AND #%00000001  ; only look at bit 0
  BEQ ReadLeftDone   ; branch to ReadLeftDone if button is NOT pressed (0)
  
; Move guy to the left 
  LDA player_x
  CMP #$d  ; changed from SBC #$5
  BEQ ReadLeftDone ; if equal to zero, go to readleftdone

  DEC player_x
  
  STA $0207 ; x-pos of sprite 2/4
  STA $020F ; x-pos of sprite 4/4
  clc
  sbc #$8
  STA $0203 ; x-pos of sprite 1/4
  STA $020B ; x-pos of sprite 3/4
  
ReadLeftDone:

ReadRight: 
  LDA $4016       ; player 1 - Right
  AND #%00000001  ; only look at bit 0
 
  BEQ ReadRightDone ; branch to ReadRightDone if button is NOT pressed (0)
  
  ;Move guy to the right  
  LDA player_x
  CMP #$F0 ; ; if it has exceeded x position of xxx, ignore further movement in x axis to the right
  BEQ ReadRightDone ; if equal to zero, go to readrightdone
  CMP #$F1 ; ; if it has exceeded x position of xxx, ignore further movement in x axis to the right
  BEQ ReadRightDone ; if equal to zero, go to readrightdone
  
  INC player_x
 
  STA $0207 ; write to x-pos of sprite 2/4
  STA $020F ; write to x-pos of sprite 4/4
  clc
  sbc #$8
  STA $0203 ; write to x-pos of sprite 1/4
  STA $020B ; write to x-pos of sprite 3/4
  
 ; boost 
  LDY boost
  CPY #$1
  BNE ReadRightDone
  
  LDA player_x  
  INC player_x
 
  STA $0207 ; write to x-pos of sprite 2/4
  STA $020F ; write to x-pos of sprite 4/4
  clc
  sbc #$8
  STA $0203 ; write to x-pos of sprite 1/4
  STA $020B ; write to x-pos of sprite 3/4
  LDY #$0 ; reset boost to zero
  STY boost  
  
ReadRightDone:

; ldx #$0
; frame animation
; sprite animation 

; is player still alive?
  lda player_dead
  cmp #$1 
  bne nmi_end2  ; branch if not set to 1
  
restart: ; pause and wait for keystroke in collision routine

  LDX #$00              ; start at 0
  lda #$0
  ldy #$0
restartloop:
  lda restart_bit
  cmp #$1
  beq continue
  
  
  ldx restartloopcounter


  dec $273
  iny
  jmp LatchController
  
continue: 
  lda #$64 ; reset health back to 100
  sta player_h
  lda #$0
  sta restart_bit ; reset restart bit
  sta player_dead ; resurrect player
  	
nmi_end2:
	
  RTI

;;;;;;;;;;;;;;  

  .bank 1
  .org $E000
palette:
  .db $22,$29,$07,$1A	;background palette 1 - blue, light green, dark green, brown
  .db $22,$36,$17,$0D	;background palette 2
  .db $22,$30,$21,$0D	;background palette 3
  .db $22,$27,$17,$0D	;background palette 4
  .db $22,$15,$30,$0d	;sprite palette 1  ;  background, col1, col2, col3:  blue,red  ,white , black
  .db $22,$16,$27,$28	;sprite palette 2 - fire
  .db $22,$07,$30,$27	;sprite palette 2
  .db $22,$16,$30,$27	;sprite palette 3
  
; previously: 
;background_palette:
;  .db $22,$29,$1A,$0F	;background palette 1
;  .db $22,$36,$17,$0F	;background palette 2
;  .db $22,$30,$21,$0F	;background palette 3
;  .db $22,$27,$17,$0F	;background palette 4
  
;sprite_palette:
;  .db $22,$0F,$00,$15	;sprite palette 1
;  .db $22,$16,$27,$28	;sprite palette 2 - fire
;  .db $22,$1A,$30,$27	;sprite palette 2
;  .db $22,$16,$30,$27	;sprite palette 3
;  .db $22,$0F,$36,$17	;sprite palette 4

	
sprites:
player:
  .db $18, $0A, %00000000, $08   ;sprite 1/4: player
  .db $18, $0B, %00000000, $10   ;sprite 2/4: player
  .db $20, $1A, %00000000, $08   ;sprite 3/4: player
  .db $20, $1B, %00000000, $10   ;sprite 4/4: player
  .db $DC, $0E, %00000001, $CD ; sprite 5: projectile (y 220, x 205)
;     y,   tile,attribute, x
tiefighter:
  .db $c8, $4a, %00000000, $32 ; tiefighter 1/4
  .db $c8, $4b, %00000000, $3a ; tiefighter 2/4
  .db $d0, $5a, %00000000, $32 ; tiefighter 3/4
  .db $d0, $5b, %00000000, $3a ; tiefighter 4/4

; attribute
;  76543210
;  ||||||||
;  ||||||++- Palette (4 to 7) of sprite
;  |||+++--- Unimplemented (read 0)
;  ||+------ Priority (0: in front of background; 1: behind background)
;  |+------- Flip sprite horizontally
;  +-------- Flip sprite vertically

s0:
  .db $f0, $e0, %00000001, $14 ; $a - $14
s1:  
  .db $f0, $e1, %00000001, $1c ; $a - $1c
s2:  
  .db $f0, $e2, %00000001, $24 ; $a - $24
s3:
  .db $f0, $e3, %00000000, $2c ; $a - $2c
s4:
  .db $f0, $e4, %00000000, $34 ; $a - $34
s5:
  .db $f0, $e5, %00000000, $3c ; $a - $3c
s6:  
  .db $f0, $e6, %00000000, $44 ; $a - $44
s7:  
  .db $f0, $e7, %00000000, $4c ; $a - $4c
s8:  
  .db $f0, $e8, %00000000, $54 ; $a - $54
s9:  
  .db $f0, $e9, %00000000, $5c ; $a - $5c


cursor:
  .db $78, $c2, %00000000, $5f  
s_restart:
  .db $78, $fb, %00000000, $67 ; r
  .db $78, $ee, %00000000, $6f ; e
  .db $78, $fc, %00000000, $77 ; s
  .db $78, $fd, %00000000, $7f ; t
  .db $78, $ea, %00000000, $87 ; a
  .db $78, $fb, %00000000, $8f ; r
  .db $78, $fd, %00000000, $97 ; t
button:
  .db $80, $48, %00000000, $77
arrow:
  .db $80, $49, %00000000, $87


background1:
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F

  
background2:
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F


background3:
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $0F, $0F, $0F, $0F, $0F, $0F, $83, $95, $97, $97, $97, $97, $97, $97, $97, $96, $86, $B7, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
  .db $B7, $73, $77, $65, $57, $47, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $47, $57, $57, $57, $57, $66, $77, $87, $97, $A7, $A7, $A7, $B7, $B7
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F

background4:
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
  .db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F



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
