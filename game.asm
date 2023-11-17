  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring


;;;;;;;;;;;;;;;
;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
  
player_x  .rs 1  ; .rs 1 means reserve one byte of space. x-pos of player 1/4 sprite
player_y  .rs 1  ; player y cordinates. y-pos of player 1/4 sprite
enemy_x .rs 1
enemy_y .rs 1
;player_h .rs 1 ; player health
;player_l .rs 1 ; player lives
;enemy_h .rs 1 ; enemy health
;missile_h .rs 1 ; missile health
;bullet_h .rs 1 ; bullet health
boost .rs 1 ; variable stores if boost has been applied. it is reset after having added boost to the sprites movement.
fired .rs 1 ; variable stores if the missile been fired. it is reset after leaving the screen.
fade .rs 1 ; 
;proj_x .rs 1
;proj_y .rs 1
sprite .rs 1 ; sprite iteration (animation)
frame .rs 1 ; keep track of frame
gametime .rs 1 ; keep track of gametime (shortterm, up to 255 frames, around 4seconds)

;good sprites
player_h = 100
player_l = 5
missile_h = 20
bullet_h = 5
;bad sprites
enemy_h = 200
;art sprites


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
  STA $0200, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
;  LDA #$FE
;  STA $0300, x
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
LoadBackgroundPaletteLoop:
  LDA background_palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$10              ; Compare X to hex $10, decimal 16
  BNE LoadBackgroundPaletteLoop  ; Branch to LoadBackgroundPaletteLoop if compare was Not Equal to zero
  
  LDX #$00      ; set X to Zero
LoadSpritePaletteLoop:
  LDA sprite_palette, x     ;load palette byte
  STA $2007					;write to PPU
  INX                   	;set index to next byte
  CPX #$10            
  BNE LoadSpritePaletteLoop  ;if x = $10, all done

  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$14              ; Compare X to hex $14, decimal 20. loads the first 20 bytes of sprites (5 sprites)
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to zero, keep going down 

LoadEnemyLoop:
  LDA tiefighter, x        ; load data from address (sprites +  x)
  STA $0234, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$10              ; Compare X to hex $14, decimal 20. loads the first 20 bytes of sprites (5 sprites)
  BNE LoadEnemyLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to zero, keep going down 
 
  
;  LDX #$00              ; start at 0 
; LoadSpriteArrayLoop:
;  LDA sprite_array, x        ; load data from address (sprites +  x)
;  STA $0214, x          ; store into RAM address ($0200 + x)
;  INX                   ; X = X + 1
;  CPX #$28              ; Compare X to hex $14, decimal 20. loads the first 20 bytes of sprites (5 sprites)
;  BNE LoadSpriteArrayLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to zero, keep going down 


 
   
  
  
; PPU registers  
  LDA #%10000000   ; enable NMI, sprite size 8x8, sprites from Pattern Table 0, base nametable address $2000
  STA $2000
  
  LDA #%00010000   ; enable sprites, hide sprites in leftmost 8 pixels of screen, hide background in leftmost 8 pixels of screen
  STA $2001
  
  LDA $0203 ; load x-pos of sprite 1/4
  STA player_x ; store x-pos in player_x variable
  LDA $0200 ; load y-pos of sprite 1/4
  STA player_y ; store y-pos in player_y variable
  
  JSR init_apu ; jump to init_apu label for initialising sound registers
 
  LDA $0237 ; load x-pos of tiefighter 1/4
  STA enemy_x ; store x-pos in player_x variable
  LDA $0234 ; load y-pos of sprite 1/4
  STA enemy_y ; store y-pos in player_y variable

 
Foreverloop:
  JMP Foreverloop     ;jump back to Forever, infinite loop

restart:
  ldx 100
  stx player_h
  ; pause or wait for keystroke 

init_apu:
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
  rts

NMI: 
; [RENDER]  
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer
 
  lda player_l
  cmp 5
  beq l5
  cmp 4
  beq l4
;  cmp 3
;  beq l3
;  cmp 2
;  beq l2
;  cmp 1
;  beq l1
;  cmp 0
;  beq l0  
  
l5:
  lda s5, x
  sta $022c, x
  inx
  cpx #$4
  bne l5
  jmp livesok
  
l4:
  lda s4, x
  sta $022c, x
  inx
  cpx #$4
  bne l4  
  jmp livesok

livesok:

;LoadEnemyLoop:
;  LDA tiefighter, x        ; load data from address (sprites +  x)
;  STA $0234, x          ; store into RAM address ($0200 + x)
;  INX                   ; X = X + 1
;  CPX #$10              ; Compare X to hex $14, decimal 20. loads the first 20 bytes of sprites (5 sprites)
;  BNE LoadEnemyLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to zero, keep going down   
  
 
 ;[check collisions]
  lda player_x
  cmp enemy_x
  bne clear
  lda player_y
  cmp enemy_y
  bne clear
  ;collision has occured
  clc
  lda player_h
  cmp enemy_h
  bmi dead
  ;enemy destroyed and survived
  ;set enemy alive status to 0
  
dead:
  ldx player_l
  DEX
  stx player_l
  beq gameover
  jmp restart
  
  
gameover:
 
  
  
  
clear:
  
  
  LDA enemy_x
  STA $0237 ; x-pos of sprite 1/4
  STA $023f ; x-pos of sprite 3/4
  TAX
  CLC
  ADC #$08; account for shift of tile location in *.chr . add #$8 to x
  STA $023b ; x-pos of sprite 2/4
  STA $0243 ; x-pos of sprite 4/4
  DEX
  STX enemy_x   
  
 
 
; if it has exceeded x position of xxx, reset the missile
  LDA $0213 ; load x coordinates of missile sprite
  CMP #$F0 ; check if the coordinates equals screen out of bound area
  BEQ missilereset ; if result is zero, branch to missilereset label to reset the status of the missile
  CMP #$F1 ; check if the coordinates equals screen out of bound area
  BEQ missilereset ; if result is zero, branch to missilereset label to reset the status of the missile
  
 
;if it has been fired, add 2 to x-pos of missile $0213
  LDA fired
  BEQ nmi_end ; branch if equal to zero, branch to nmi_end if it hasnt been fired
  LDA $0213 ; load current x-pos of missile
  CLC
  ADC #$2 ; if it has been fired, move the missile to the right
  STA $0213
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
  STA $0213 ; store in x-pos of missile (starting position)
  LDA player_y ; load y-pos of player
  STA $0210 ; store in y-pos of missile (starting position)

ReadBDone:        ; handling this button is done

ReadSelect: 
  LDA $4016       ; player 1 - Select
  AND #%00000001  ; only look at bit 0
  BEQ ReadSelectDone   ; branch to ReadSelectDone if button is NOT pressed (0)

ReadSelectDone:

ReadStart: 
  LDA $4016       ; player 1 - Start
  AND #%00000001  ; only look at bit 0
  BEQ ReadStartDone   ; branch to ReadStartDone if button is NOT pressed (0)

ReadStartDone:

ReadUp: 
  LDA $4016       ; player 1 - Up
  AND #%00000001  ; only look at bit 0
  BEQ ReadUpDone   ; branch to ReadUpDone if button is NOT pressed (0)

; move guy up 
  LDA player_y
  ;CMP #$0F ; ; if it has reached position x of xxx, ignore further movement in y axis upwards
  SBC #$5
  BEQ ReadUpDone ; if equal to zero, go to readupdone
  
  LDA player_y
  STA $0200 ; y-pos of sprite 1/4
  STA $0204 ; y-pos of sprite 2/4
  TAX ; transfer a to x
  CLC ; clear carry
  ADC #$08 ; account for shift of tile location in *.chr . add #$8 to x
  STA $0208 ; y-pos of sprite 3/4
  STA $020C ; y-pos of sprite 4/4
  DEX ; decrease x by 1
  STX player_y  ; store new x as new y-pos for player sprite

ReadUpDone:

ReadDown: 
  LDA $4016       ; player 1 - Down
  AND #%00000001  ; only look at bit 0
  BEQ ReadDownDone   ; branch to ReadDownDone if button is NOT pressed (0)

; move guy down  
  LDA player_y
  CMP #$DB ; ; if it has exceeded y position of xxx, ignore further movement in y axis downwards
  BEQ ReadDownDone ; if equal to zero, go to readdowndone

;  LDA player_y
  STA $0200 ; y-pos of sprite 1/4
  STA $0204 ; y-pos of sprite 2/4
  TAX
  CLC
  ADC #$08
  STA $0208 ; y-pos of sprite 3/4
  STA $020C ; y-pos of sprite 4/4
  INX
  STX player_y

ReadDownDone:

ReadLeft: 
  LDA $4016       ; player 1 - Left
  AND #%00000001  ; only look at bit 0
  BEQ ReadLeftDone   ; branch to ReadLeftDone if button is NOT pressed (0)
;Move guy to the left 
  LDA player_x
  ;CMP #$0F ; ; if it has reached position x of xxx, ignore further movement in x axis to the left
  SBC #$5
  BEQ ReadLeftDone ; if equal to zero, go to readleftdone
  
  LDA player_x
  STA $0203 ; x-pos of sprite 1/4
  STA $020B ; x-pos of sprite 3/4
  TAX
  CLC
  ADC #$08; account for shift of tile location in *.chr . add #$8 to x
  STA $0207 ; x-pos of sprite 2/4
  STA $020F ; x-pos of sprite 4/4
  DEX
  STX player_x   

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
    
  STA $0203
  STA $020B
  TAX
  CLC
  ADC #$08
  STA $0207
  STA $020F
  INX
  STX player_x
  LDA boost
  TAY
  CPY #$1
  BNE ReadRightDone
  INX
  STX player_x
  LDA #$0 ; reset boost to zero
  STA boost  
  
ReadRightDone:

; ldx #$0
; frame animation
; sprite animation 
  
animatedspriteloop:
  CLC
  lda #$50
  sta $0230 ; y-pos of bouncing ball
  ldx sprite ; load current sprite frame into x
  lda array, x ; load value from array based on sprite frame
  sta $0231 ; tile number of bouncing ball
  lda #%00000001
  sta $0232 ; attribute of bouncing ball. colour palette 1 set
  lda #$50
  sta $0233 ; x-pos of bouncing ball

  ldy frame ; load current frame number into y
  iny ; increase y by 1
  sty frame ; store new frame number
  cpy #$6 ; count up to 6
  beq nextframe ; if result is zero, branch to nextframe, otherwise continue
  jmp nmi_end2

; /frame animation
nextframe:
;  jmp nmi_end2
  ldx #$0
  stx frame ; reset frame counter to 0
  ldx sprite ; load current sprite frame number
  inx ; increase frame by 1
  stx sprite ; store new sprite frame number value
  cpx #$a ; count up to 10
  beq resetanimation ; if result is zero, branch to resetanimation, otherwise continue ____ PROBLEM HERE -- problem was 
  ;with rts at end of nextframe and resetanimation. removed and fixed problem.
  jmp nmi_end2
resetanimation:
  ldx #$0
  stx sprite ; reset sprite frame counter to 0
  	
nmi_end2:
	
  RTI

;;;;;;;;;;;;;;  

  .bank 1
  .org $E000
background_palette:
  .db $22,$29,$1A,$0F	;background palette 1
  .db $22,$36,$17,$0F	;background palette 2
  .db $22,$30,$21,$0F	;background palette 3
  .db $22,$27,$17,$0F	;background palette 4
  
sprite_palette:
  .db $22,$0F,$00,$15	;sprite palette 1
  .db $22,$16,$27,$28	;sprite palette 2 - fire
  .db $22,$1A,$30,$27	;sprite palette 2
  .db $22,$16,$30,$27	;sprite palette 3
;  .db $22,$0F,$36,$17	;sprite palette 4

	
sprites:

       ;y,tile, attribute,   x
  .db $18, $0A, %00000000, $08   ;sprite 1/4: player
  .db $18, $0B, %00000000, $10   ;sprite 2/4: player
  .db $20, $1A, %00000000, $08   ;sprite 3/4: player
  .db $20, $1B, %00000000, $10   ;sprite 4/4: player
  .db $DC, $0E, %00000001, $CD ; sprite 5: projectile (y 220, x 205)
 
tiefighter:
  .db $c8, $4a, %00000000, $32 ; tiefighter 1/4
  .db $c8, $4b, %00000000, $3a ; tiefighter 2/4
  .db $d0, $5a, %00000000, $32 ; tiefighter 3/4
  .db $d0, $5b, %00000000, $3a ; tiefighter 4/4


 
;sprite_array:
;  .db $50, $c4, %00000000, $58 ; frame1
;  .db $60, $c5, %00000000, $60 ; frame2
;  .db $70, $c6, %00000000, $68 ; frame3
;  .db $70, $c7, %00000000, $70 ; frame4
;  .db $70, $c8, %00000000, $78 ; frame5
;  .db $70, $c8, %00000000, $80 ; frame5
;  .db $70, $c7, %00000000, $88 ; frame4
;  .db $70, $c6, %00000000, $90 ; frame3
;  .db $60, $c5, %00000000, $98 ; frame2
;  .db $50, $c4, %00000000, $A0 ; frame1

;  76543210
;  ||||||||
;  ||||||++- Palette (4 to 7) of sprite
;  |||+++--- Unimplemented (read 0)
;  ||+------ Priority (0: in front of background; 1: behind background)
;  |+------- Flip sprite horizontally
;  +-------- Flip sprite vertically


array: ; bouncing ball animated frames / tile sequence
  .db $c4
  .db $c5
  .db $c6
  .db $c7
  .db $c8
  .db $c8
  .db $c7
  .db $c6
  .db $c5
  .db $c4


s0:
  .db $10, $e0, %00000001, $a
s1:  
  .db $20, $e1, %00000001, $b
s2:  
  .db $30, $e2, %00000001, $c
s3:
  .db $40, $e3, %00000000, $d
s4:
  .db $50, $e4, %00000000, $e
s5:
  .db $60, $e5, %00000000, $f
s6:  
  .db $70, $e6, %00000000, $10
s7:  
  .db $80, $e7, %00000000, $11
s8:  
  .db $90, $e8, %00000000, $12
s9:  
  .db $A0, $e9, %00000000, $13


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
  .incbin "game.nsf"   ; includes nsf audio file from Super C (tm)
  
  ;  * No expansion chip
;Building music data...
; * Sequences used: 0 (0 bytes)
; * Instruments used: 1 (1 bytes)
; * Song 1, 145 patterns (582 bytes), 29 frames (348 bytes)
;
; * Samples located at: $C000
; * DPCM samples used: 0 (0 bytes)
; * NSF load address: $A745
;Writing output file...
; * Driver size: 5128 bytes
; * Song data size: 955 bytes (3%)
; * NSF type: Linear (driver @ $AB00)
;Done, total file size: 6211 bytes

