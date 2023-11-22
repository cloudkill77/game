  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring


;;;;;;;;;;;;;;;
;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
; .rs 1 means reserve one byte of space.

framecounter1 .rs 1  ;  $0 count nmi frames
framecounter2 .rs 1  ;  $1 count nmi frames
framecounter3 .rs 1  ; $2 slowly counts up from $0
counter .rs 1 ; $3 slowly counts down from $ff
logohasplayed .rs 1; $4 intro has played
g_done .rs 1 ; $5 g has done its thing
planespawn .rs 1 ; have the planes been spawned
skipintro .rs 1
skipintrocount .rs 1

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
  CPX #$B0              ; Compare X to hex $80, decimal 128. loads the first 128 bytes of sprites (32 sprites)
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
 
  lda #$ff
  sta counter
  ldy #$0


  
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

  lda skipintro
  cmp #$1
  bne runintro 

initialise_game:
  lda logohasplayed
  cmp #$01    ; 1st run: 0-1=-1
  bne runintro ; branch if hasplayed is 0

  ldy skipintrocount
  
  cpy #$3
  bne skip3
  lda #$26
  sta framecounter3
skip3:  
  cpy #$2
  bne skip2
  lda #$7
  sta framecounter3
skip2:
  cpy #$1
  bne skip1
  lda #$4
  sta framecounter3
skip1:
  cpy #$0
  bne initialise_end  
  lda #$2
  sta framecounter3
initialise_end:
  iny
  sty skipintrocount 
  
 
runintro: 
  lda logohasplayed
  cmp #$01    ; 1st run: 0-1=-1
  beq intro_end ; branch if logohasplayed is 1

  lda framecounter1
  cmp #$30
  bne framecounter_end ; branch until counter has reached $30

  lda #130
  sta $4006
  lda #200
  sta $4007
  lda #%10011111
  sta $4004


  ; move game letters onto screen
  lda #$72 ; y-position of g, a, m and e letters
  sta $204 ; g
  sta $208 ; a
  sta $20c ; m
  sta $210 ; e
  inc logohasplayed  ; increase hasplayed from 0 to 1, to note that intro has played.


framecounter_end:

intro_end:   ; branch here if intro1 has played
  inc framecounter1


  lda logohasplayed
  cmp #$01    ; 1st run: 0-1=-1
  beq clearscreen ; branch if has played is 1
  jmp intro2_end

clearscreen:
  lda framecounter2
  inc framecounter2
  cmp #$ff
  bne intro2_end ; branch until counter has reached $ff
  

  inc framecounter3
  dec counter
  
  lda g_done
  cmp #$1
  beq g_isdone
  
  lda #30
  sta $4006  ; timer low
  ;lda #200
  lda #%11111000
  sta $4007  
  ;  7-3 length counter load, 2-0 timer high
  lda #%00000011
  sta $4004
  ;  7,6 - duty (width of the pulse)
  ;  5 - loop (1) or not (0)
  ;  4 - volume constant (1) or with envelope (0)
  ;  3 - volume if set to constant, rate of decay if envelope set

  inc $0204 ; increase y-position of letter g
g_isdone:  
  
intro2_end:

  lda framecounter3
  cmp #$2
  bne counting1_done
  
  ; move sun on screen after timer has met its target
  lda #$10
  sta $214
  sta $218
  sta $21c
  clc
  adc #$8
  sta $220
  sta $224
  sta $228
  clc
  adc #$8
  sta $22c
  sta $230
  sta $234
  
counting1_done:  

  lda framecounter3
  cmp #$4
  bne counting2_done
  
  ; let there be clouds
  lda #$50
  sta $238
  sta $23c
  sta $240
  lda #$40
  sta $244
  sta $248
  sta $24c

counting2_done:  

  lda framecounter3
  cmp #$7
  bne counting3_done

  ; build the factory
  lda #$C0
  sta $250
  sta $254
  lda #$C8
  sta $258
  sta $25c

counting3_done:

  lda framecounter3 ; load framecounter3
  cmp #$22 ; compare to #22
  bne counting4_done
  lda #$1
  sta g_done ; save 1 in g_done variable to denote that it no longer needs to move down
  
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
 
; move factory offscreen
  lda #$f0
  sta $250
  sta $254
  sta $258
  sta $25c

; move brokenfactory onscreen
  lda #$c8
  sta $260
  sta $264
  jmp counting4_done

; move brokenfactory onscreen randomized
  lda $FE
  cmp #$40
  bcc aa
  cmp #$80
  bcc ab
  cmp #$C0
  bcc ba
  cmp #$ff
  bcc bb
          
aa:
  lda #$c8
  sta $260
  sta $264
  jmp counting4_done
ab:
  lda #$c8
  sta $260
  sta $26c
  jmp counting4_done
ba:
  lda #$c8
  sta $268
  sta $264
  jmp counting4_done
bb:
  lda #$c8
  sta $268
  sta $26c
  jmp counting4_done

;brokenfactory;
;  .db $f0, $aa, %00000010, $65 ; row aL ; $260 / $263
;  .db $f0, $ab, %00000010, $6D ; row aR ; $264 / $267
;  .db $f0, $ba, %00000010, $65 ; row bL ; $268 / $26b
;  .db $f0, $bb, %00000010, $6D ; row bR ; $26c / $26f
  
counting4_done:


  lda framecounter3 ; load framecounter3
  cmp #$25 ; compare to #22
  bne counting5_done

; move jets on screen

  lda #$60
  sta $280
  sta $284
  lda #$68
  sta $288
  sta $28c
  
  lda #$74
  sta $270
  sta $274
  lda #$7c
  sta $278
  sta $27c

  lda #$1
  sta planespawn ; set planespawn to 1
  


counting5_done:

  lda planespawn
  cmp #$1
  bne planeskip ; skip if planes havent spawned yet
; move plane 2 across the screen
  inc $283
  inc $287
  inc $28b
  inc $28f
; move plane 1 across the screen
  inc $273
  inc $277
  inc $27b
  inc $27f



planeskip:

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
  lda #$1
  sta skipintro
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
 
game:
  .db $f0, $f0, %00000000, $68 ; g
  .db $f0, $ea, %00000000, $70 ; a
  .db $f0, $f6, %00000000, $78 ; m
  .db $f0, $ee, %00000000, $80 ; e
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
blackplane:
  .db $f0, $2c, %00000000, $0 ; plane1 1/4
  .db $f0, $2d, %00000000, $8 ; plane1 2/4
  .db $f0, $3c, %00000000, $0 ; plane1 3/4
  .db $f0, $3d, %00000000, $8 ; plane1 4/4
  .db $f0, $2a, %00000000, $10 ; plane2 1/4 with lights
  .db $f0, $2b, %00000000, $18 ; plane2 2/4 with lights 
  .db $f0, $3a, %00000000, $10 ; plane2 3/4 with lights
  .db $f0, $3b, %00000000, $18 ; plane2 4/4 with lights
button:
  .db $f0, $48, %00000000, $90
arrow:
  .db $f0, $49, %00000000, $98  
buttonlabel:  
  .db $f0, $58, %00000000, $90 ; A
  .db $f0, $59, %00000000, $98 ; B 
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
  .db %00000000, %00000000, %0000000, %00000000, %00000000, %00000000, %00000000, %00000000
 


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
