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
  CPX #$20              ; Compare X to hex $20, decimal 32 - copying 32 bytes = 4 for sprites, 4 for background
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
  
  
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
  
  
Foreverloop:

  JMP Foreverloop     ;jump back to Forever, infinite loop
  


NMI: 
; [RENDER]  
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer



  RTI


;;;;;;;;;;;;;;  

  .bank 1
  .org $E000
  
; info for palettes, sprites and background, called during initialization


palette:
  .db $0f,$19,$15,$09	;background palette 1 - 0f-black,19-green,15-red (not used),09-dark green
  .db $0f,$36,$17,$11	;background palette 2 $22,$36,$17,$0D
  .db $0f,$30,$21,$0D	;background palette 3
  .db $0f,$27,$17,$0D	;background palette 4
  .db $0f,$10,$00,$30	;sprite palette 1  ; 0f-black,10-light grey,00-grey,30-white
  .db $0f,$16,$27,$28	;sprite palette 2 - fire
  .db $0f,$07,$30,$27	;sprite palette 2
  .db $0f,$16,$30,$27	;sprite palette 3



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
  