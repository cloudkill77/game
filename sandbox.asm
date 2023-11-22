  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring

;;;;;;;;;;;;;;;
;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
num1 .rs 1
num2 .rs 1
result .rs 1
enemy_x .rs 1
enemy_y .rs 1
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
  CPX #$28              ; Compare X to hex $14, decimal 20. loads the first 20 bytes of sprites (5 sprites)
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to zero, keep going down 
  
LoadEnemyLoop:
  LDA tiefighter, x        ; load data from address (sprites +  x)
  STA $0234, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$10              ; Compare X to hex $14, decimal 20. loads the first 20 bytes of sprites (5 sprites)
  BNE LoadEnemyLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to zero, keep going down 
  

  
; PPU registers  
  LDA #%10000000   ; enable NMI, sprite size 8x8, sprites from Pattern Table 0, base nametable address $2000
  STA $2000
  
  LDA #%00010000   ; enable sprites, hide sprites in leftmost 8 pixels of screen, hide background in leftmost 8 pixels of screen
  STA $2001
  
  lda #$00
  sta $4015
  lda #$0F
  sta $4015
; setup apu frame counter
  lda #$40
  sta $4017  


  LDA $0237 ; load x-pos of tiefighter 1/4
  STA enemy_x ; store x-pos in player_x variable
  LDA $0234 ; load y-pos of sprite 1/4
  STA enemy_y ; store y-pos in player_y variable


Foreverloop:
  JMP Foreverloop     ;jump back to Forever, infinite loop





NMI: 
; [RENDER]  
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer
 
 
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


  clc
  lda num1
  adc num2
  sta result
  
;  lda #$b0
;  sta $0234 ;y 1/4
;  sta $0238 ;y 2/4
;  lda #$b8
;  sta $023c ;y 3/4
;  sta $0240 ;y 4/4
;  lda #$4a
;  sta $0235 ;tile 1/4
;  lda #$4b
;  sta $0239 ;tile 2/4
;  lda #$5a
;  sta $023d ;tile 3/4
;  lda #$5b
;  sta $0241 ;tile 4/4
;  lda %00000000
;  sta $0236 ;attrib 1/4
;  sta $023a ;attrib 2/4
;  sta $023e ;attrib 3/4
;  sta $0242 ;attrib 4/4
;  lda #$32
;  sta $0237 ;x 1/4
;  sta $023f ;x 3/4
;  lda #$3a
;  sta $023b ;x 2/4
;  sta $0243 ;x 4/4


tiefighter:
  .db $b0, $4a, %00000000, $32 ; tiefighter 1/4
  .db $b0, $4b, %00000000, $3a ; tiefighter 2/4
  .db $b8, $5a, %00000000, $32 ; tiefighter 3/4
  .db $b8, $5b, %00000000, $3a ; tiefighter 4/4
  
  
  
   RTI
  
;;;;;;;;;;;;;;  

  .bank 1
  .org $E000
background_palette:
  .db $22,$29,$1A,$0F	;background palette 1
  .db $22,$36,$17,$0F	;background palette 2
  .db $22,$30,$21,$0F	;background palette 3
  .db $22,$27,$17,$0F	;background palette 4
  
sprite_palette;
  .db $22,$0F,$00,$15	;sprite palette 1
  .db $22,$16,$27,$28	;sprite palette 2 - fire
 ; .db $22,$1A,$30,$27	;sprite palette 2
  ;.db $22,$16,$30,$27	;sprite palette 3
;  .db $22,$0F,$36,$17	;sprite palette 4
  
 ;sprite_palette:
 ; .byte $0f,$30,$16,$07, $0f,$30,$00,$00, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f
 ; .byte $0f,$30,$30,$30, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f 


sprites:
  .db $10, $e0, %00000001, $a
  .db $20, $e1, %00000001, $b
  .db $30, $e2, %00000001, $c
  .db $40, $e3, %00000000, $d
  .db $50, $e4, %00000000, $e
  .db $60, $e5, %00000000, $f
  .db $70, $e6, %00000000, $10
  .db $80, $e7, %00000000, $11
  .db $90, $e8, %00000000, $12
  .db $A0, $e9, %00000000, $13

;table:
;  .db e, 00
;  .db e, 01
;  .db e, 02
;  .db e, 03
;  .db e, 04
;  .db e, 05
;  .db e, 06
;  .db e, 07
;  .db e, 08
;  .db e, 09
  
  
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
  