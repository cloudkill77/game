  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background vertical mirroring, used for horizontal scrolling


;;;;;;;;;;;;;;;
;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
; .rs 1 means reserve one byte of space.

; good sprites:
player_x  .rs 1  ; x-pos of player sprite tile 4/4
player_y  .rs 1  ; y-pos of player sprite tile 4/4
player_h .rs 1 ; player health
player_alive .rs 1 ; player is alive if set to 1
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

player_died .rs 1 ; player has died if set to 1
restartloopcounter .rs 1 ; loop counter after player death
restart_bit .rs 1 ; restart after crash
arrowmove .rs 1 ; animate the restart arrow
buttons1 .rs 1 ; store controller 1 output
buttons2 .rs 1 ; store controller 2 output

;;;;;;;;;;;;;;;


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
  CPX #$4c              ; Compare X to hex $14, decimal 20. loads the first 20 bytes of sprites (5 sprites)
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to zero, keep going down
                       ; if compare was equal to zero, keep going down 
  

ppu: 
; PPU registers  
  LDA #%10000000   ; enable NMI, sprite size 8x8, sprites from Pattern Table 0, base nametable address $2000
  STA $2000
  
  LDA #%00010110   ; enable sprites, show sprites in leftmost 8 pixels of screen, show background in leftmost 8 pixels of screen
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


  lda #$0
  sta player_alive
  LDA $020C ; read y-pos of sprite 4/4
  STA player_y
  LDA $020F ; read x-pos of sprite 4/4
  STA player_x

Foreverloop:
  
  JMP Foreverloop     ;jump back to Forever, infinite loop

; read controller subroutine. called from within NMI
ReadController:

  LDA #$01
  STA $4016		; write 1 to $4016
  LDA #$00
  STA $4016		; write 0 to $4016. tells cpu to poll the controller
  LDX #$08		; cycle through loading values of $4016 8 times to get value for each button 

ReadControllerLoop:

  LDA $4016		; load value stored in $4016
  LSR A			; bit0 -> Carry
  ROL buttons1	; bit0 <- Carry
  DEX
  BNE ReadControllerLoop
  RTS

NMI: ; gets called 60 times per second
; [RENDER]  
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer
  
  jsr ReadController

; is player still alive?
  lda player_alive
  cmp #$1
  beq gameon ; branch if not equal to zero
  jmp pressrestart

gameon:




; A, B, Select, Start, Up, Down, Left, Right
; sequence is *always* A, B, Select, Start, Up, Down, Left, Right. Controller 1: $4016, Controller 2: $4017

; bit:       7     6     5     4     3     2     1     0

; button:    A     B   select start  up   down  left right

; If the bit is 1, that button is pressed.

ReadA: 
  LDA buttons1       ; load player 1 - buttons
  AND #%10000000  ; only look at bit 7
  BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)

ReadADone:        ; handling this button is done

ReadB: 
  LDA buttons1       ; load player 1 - buttons
  AND #%01000000  ; only look at bit 6
  BEQ ReadBDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)

ReadBDone:        ; handling this button is done

ReadSelect: 
  LDA buttons1       ; load player 1 - buttons
  AND #%00100000  ; only look at bit 5
  BEQ ReadSelectDone   ; branch to ReadSelectDone if button is NOT pressed (0)
  jmp RESET
ReadSelectDone:

ReadStart: 
  LDA buttons1       ; load player 1 - buttons
  AND #%00010000  ; only look at bit 4
  BEQ ReadStartDone   ; branch to ReadStartDone if button is NOT pressed (0)

ReadStartDone:

ReadUp: 
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

ReadDown: 
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

ReadLeft: 
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

ReadRight: 
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
  jmp nmi_end

continue: 
  lda #$64 ; reset health back to 100
  sta player_h
  lda #$0
  sta restart_bit ; reset restart bit
  lda #$1
  sta player_alive ; resurrect player
  lda #$80
  sta player_y
  STA $0208 ; write to y-pos of sprite 3/4
  STA $020C ; write to y-pos of sprite 4/4
  CLC ; clear carry
  SBC #$7 ; account for shift of tile location in *.chr . subtract #$8
  STA $0200 ; write to y-pos of sprite 1/4
  STA $0204 ; write to y-pos of sprite 2/4
  lda #$20
  sta player_x
  STA $0207 ; write to x-pos of sprite 2/4
  STA $020F ; write to x-pos of sprite 4/4
  clc
  sbc #$7
  STA $0203 ; write to x-pos of sprite 1/4
  STA $020B ; write to x-pos of sprite 3/4

; clear restart option from screen 
  lda #$f0
  sta $224
  sta $228
  sta $22c
  sta $230
  sta $234
  sta $238
  sta $23c
  sta $240
  sta $244
  sta $248

 
  jmp nmi_end

pressrestart:
; write press start to retry on screen
  lda #$68
  sta $224
  sta $228
  sta $22c
  sta $230
  sta $234
  sta $238
  sta $23c
  sta $240
  lda #$70
  sta $244
  sta $248

  inc restartloopcounter
  lda restartloopcounter
  cmp #$6
  bne skip

  lda arrowmove
  clc
  adc #$1
  and #$7
  sta arrowmove

  lda #$89
  clc
  sbc arrowmove
  sta $24b
  lda #$0
  sta restartloopcounter
  
skip:  
  
;RestartReadStart: 
  LDA buttons1       ; load player 1 - buttons
  AND #%00010000  ; only look at bit 4
  BEQ RestartReadStartDone   ; branch to ReadStartDone if button is NOT pressed (0)
  JMP continue
RestartReadStartDone:



  
  
nmi_end:

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
 



sprites:
player:
  .db $78, $0A, %00000000, $18   ;sprite 1/4: player
  .db $78, $0B, %00000000, $20   ;sprite 2/4: player
  .db $80, $1A, %00000000, $18   ;sprite 3/4: player
  .db $80, $1B, %00000000, $20   ;sprite 4/4: player << collision detection configured on this tile
  .db $DC, $0E, %00000001, $CD ; sprite 5: projectile (y 220, x 205)
;     y,   tile,attribute, x
tiefighter:
  .db $c8, $4a, %00000000, $32 ; tiefighter 1/4
  .db $c8, $4b, %00000000, $3a ; tiefighter 2/4
  .db $d0, $5a, %00000000, $32 ; tiefighter 3/4
  .db $d0, $5b, %00000000, $3a ; tiefighter 4/4

cursor:
  .db $f0, $c2, %00000000, $5f  
s_restart:
  .db $f0, $fb, %00000000, $67 ; r
  .db $f0, $ee, %00000000, $6f ; e
  .db $f0, $fc, %00000000, $77 ; s
  .db $f0, $fd, %00000000, $7f ; t
  .db $f0, $ea, %00000000, $87 ; a
  .db $f0, $fb, %00000000, $8f ; r
  .db $f0, $fd, %00000000, $97 ; t
button:
  .db $f0, $d6, %00000000, $77 ; d6 - 00
arrow:
  .db $f0, $49, %00000000, $87


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
