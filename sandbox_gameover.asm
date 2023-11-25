  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
                                   

;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
; .rs 1 means reserve one byte of space.

restartloopcounter .rs 1 ; loop counter after player death
restart_bit .rs 1 ; restart after crash
arrowmove .rs 1 ; animate the restart arrow
buttons1 .rs 1 ; store controller 1 output
buttons2 .rs 1 ; store controller 2 output
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


LoadPalettes:
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


  LDX #$00
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$3c              ; Compare X to hex $80, decimal 128. loads the first 128 bytes of sprites (32 sprites)
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
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



LoadAttribute:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadAttributeLoop:
  LDA attribute, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$08              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadAttributeLoop
  
  


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
 


  
Foreverloop:

  

  JMP Foreverloop     ;jump back to Forever, infinite loop

init_apu: ; resets audio?
  lda #$00
  sta $4015
  lda #$0F
  sta $4015
; setup apu frame counter
  lda #$40
  sta $4017  
  rts

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
 
;2 -- sun
;4 -- clouds
;7 -- factory
;22 
;25 
;26 ---


 

NMI: 
; [RENDER]  
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer



;  jsr ReadController



; sequence is *always* A, B, Select, Start, Up, Down, Left, Right. Controller 1: $4016, Controller 2: $4017

ReadA: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)

ReadADone:        ; handling this button is done

ReadB: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadBDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)

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
 
  
  lda #%00000011
  ; 7,6 - not used
  ; 5 - envelope loop / length counter halt
  ; 4 - constant volume
  ; 3,2,1,0 - volume/envelope
  sta $400C  
  lda #%00000110
  ; 7 - loop noise
  ; 6,5,4 - not used
  ; 3,2,1,0 - noise period
  sta $400E
  lda #%01111000
  ; 7,6,5,4,3 - length counter load
  sta $400F  
  
  
ReadStartDone:

ReadUp: 
  LDA $4016       ; player 1 - Up
  AND #%00000001  ; only look at bit 0
  BEQ ReadUpDone   ; branch to ReadUpDone if button is NOT pressed (0)

ReadUpDone:

ReadDown: 
  LDA $4016       ; player 1 - Down
  AND #%00000001  ; only look at bit 0
  BEQ ReadDownDone   ; branch to ReadDownDone if button is NOT pressed (0)

ReadDownDone:

ReadLeft: 
  LDA $4016       ; player 1 - Left
  AND #%00000001  ; only look at bit 0
  BEQ ReadLeftDone   ; branch to ReadLeftDone if button is NOT pressed (0)

ReadLeftDone:

ReadRight: 
  LDA $4016       ; player 1 - Right
  AND #%00000001  ; only look at bit 0
  BEQ ReadRightDone ; branch to ReadRightDone if button is NOT pressed (0)
  
ReadRightDone:

  jmp skip
  inc restartloopcounter
  lda restartloopcounter
  cmp #$8
  bne skip

  lda arrowmove
  clc
  adc #$1
  and #$7
  sta arrowmove

  lda #$93
  clc
  sbc arrowmove
  sta $23b
  lda #$0
  sta restartloopcounter
  
skip:

  	
nmi_end2:
	
  RTI

;;;;;;;;;;;;;;  

  .bank 1
  .org $E000
palette:
  .db $0f,$19,$15,$09	;background palette 1 - 0d-black,19-green,15-red (not used),09-dark green
  .db $27,$36,$17,$11	;background palette 2 $22,$36,$17,$0D
  .db $27,$30,$21,$0f	;background palette 3
  .db $27,$27,$17,$0f	;background palette 4
  .db $0f,$10,$00,$30	;sprite palette 1  ; 0d-black,10-light grey,00-grey,30-white
  .db $27,$16,$27,$28	;sprite palette 2 - fire
  .db $27,$07,$30,$27	;sprite palette 2
  .db $27,$16,$30,$27	;sprite palette 3


	


;  76543210
;  ||||||||
;  ||||||++- Palette (4 to 7) of sprite
;  |||+++--- Unimplemented (read 0)
;  ||+------ Priority (0: in front of background; 1: behind background)
;  |+------- Flip sprite horizontally
;  +-------- Flip sprite vertically


sprites:
s3:
  .db $f0, $e3, %00000000, $72	; "3" not used currently
 
tombstone:
  .db $90, $02, %00000000, $68 ; tombstone 1/4
  .db $90, $03, %00000000, $70 ; tombstone 2/4
  .db $98, $12, %00000000, $68 ; tombstone 3/4
  .db $98, $13, %00000000, $70 ; tombstone 4/4
;sun:
;  .db $f0, $40, %00000010, $08
;  .db $f0, $41, %00000010, $10
;  .db $f0, $42, %00000010, $18
;  .db $f0, $50, %00000010, $08
;  .db $f0, $51, %00000000, $10
;  .db $f0, $52, %00000010, $18
;  .db $f0, $60, %00000010, $08
;  .db $f0, $61, %00000010, $10
;  .db $f0, $62, %00000010, $18
moon:
  .db $20, $a8, %00000000, $20 ; moon 1/4
  .db $20, $a9, %00000000, $28 ; moon 2/4
  .db $28, $b8, %00000000, $20 ; moon 3/4
  .db $28, $b9, %00000000, $28 ; moon 4/4
blackplane:
  .db $a0, $2c, %00000000, $68 ; plane1 1/4
  .db $a0, $2d, %00000000, $70 ; plane1 2/4
  .db $a8, $3c, %00000000, $68 ; plane1 3/4
  .db $a8, $3d, %00000000, $70 ; plane1 4/4

button:
  .db $a4, $d6, %00000000, $80 ; startbutton
arrow:
  .db $a4, $49, %00000000, $88  

score:


;  76543210
;  ||||||||
;  ||||||++- Palette (4 to 7) of sprite
;  |||+++--- Unimplemented (read 0)
;  ||+------ Priority (0: in front of background; 1: behind background)
;  |+------- Flip sprite horizontally
;  +-------- Flip sprite vertically



  


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



attribute:
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
 


;sun
; 40 41 42
; 50 51 52
; 60 61 62


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
