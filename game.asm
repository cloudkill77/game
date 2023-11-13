  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring


;;;;;;;;;;;;;;;
;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
  
player_x  .rs 1  ; .rs 1 means reserve one byte of space
player_y  .rs 1  
boost .rs 1
fired .rs 1
;proj_x .rs 1
;proj_y .rs 1

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
  
  LDA #%10000000   ; enable NMI, sprites from Pattern Table 0
  STA $2000
  
  LDA #%00010000   ; enable sprites
  STA $2001
  
  LDA $0203
  STA player_x
  LDA $0200
  STA player_y
 
Foreverloop:
  JMP Foreverloop     ;jump back to Forever, infinite loop

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
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer
 
; if it has exceeded x position of xxx, reset the missile
  LDA $0213 ; x coordinates
  CMP #$F0 ; 
  BEQ reset
 
;if it has been fired, add 1 to x position of projectile $0213
  LDA fired
  BEQ nmi_end ; branch if equal to zero, branch to nmi_end if it hasnt been fired
  LDA $0213
  CLC
  ADC #$1 ; if it has been fired, move the missile to the right
  STA $0213
  JMP nmi_end

;move the missile to the default position. reset the fired variable to 0 
reset:
  jsr init_apu
  LDA #$CD ;CD = 205
  STA $0213 ; x coordinates
  LDA #$DC ; DC = 220
  STA $0210 ; y coordinates
  LDA #$0 ; 0
  STA fired ; stored in fired variable

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
 
  lda #138
  sta $400A

  lda #139
  sta $400B

  lda #%11000000
  sta $4008
  sta $4017
  
  LDA player_x ; load x coordinates of player
  STA $0213 ; store in x coordinates of sprite 5
  LDA player_y ; load y coordinates of player
  STA $210 ; store y position of projectile

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
  STA $0200 ; y of sprite 1
  STA $0204 ; y of sprite 2
  TAX ; transfer a to x
  CLC ; clear carry
  ADC #$08 ; account for shift of tile location in *.chr . add #$8 to x
  STA $0208 ; y coordinates of sprite 3
  STA $020C ; y coordinates if sprite 4
  DEX ; decrease x by 1
  STX player_y  ; store new x as new y coordinates for player sprite

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
  STA $0200
  STA $0204
  TAX
  CLC
  ADC #$08
  STA $0208
  STA $020C
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
  STA $0203
  STA $020B
  TAX
  CLC
  ADC #$08
  STA $0207
  STA $020F
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
  .db $22,$0F,$36,$17	;sprite palette 4

	
sprites:

       ;y,tile, attribute,   x
  .db $18, $0A, %00000000, $08   ;sprite 1/4: player
  .db $18, $0B, %00000000, $10   ;sprite 2/4: player
  .db $20, $1A, %00000000, $08   ;sprite 3/4: player
  .db $20, $1B, %00000000, $10   ;sprite 4/4: player
  .db $DC, $0E, %00000001, $CD ; sprite 5: projectile (y 220, x 205)

;  76543210
;  ||||||||
;  ||||||++- Palette (4 to 7) of sprite
;  |||+++--- Unimplemented (read 0)
;  ||+------ Priority (0: in front of background; 1: behind background)
;  |+------- Flip sprite horizontally
;  +-------- Flip sprite vertically

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

