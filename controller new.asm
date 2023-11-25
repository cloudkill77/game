player_x  .rs 1  ; x-pos of player sprite tile 4/4
player_y  .rs 1  ; y-pos of player sprite tile 4/4
buttons1 .rs 1 ; store controller 1 output
buttons2 .rs 1 ; store controller 2 output

RESET:


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
  
  

; controller actions, read values stored in buttons1 variable
  
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
  
; continue game
continue:
  
; ################  
  
; second instance to read controller values

;RestartReadStart: 
  LDA buttons1       ; load player 1 - buttons
  AND #%00010000  ; only look at bit 4
  BEQ RestartReadStartDone   ; branch to ReadStartDone if button is NOT pressed (0)
  JMP continue
RestartReadStartDone:  

