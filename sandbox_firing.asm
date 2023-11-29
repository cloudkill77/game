; if it has exceeded x position of xxx, reset the missile
  LDA $0213 ; load x coordinates of missile sprite
  CMP #$F0 ; check if the coordinates equals screen out of bound area
  BEQ missilereset ; if result is zero, branch to missilereset label to reset the status of the missile
  CMP #$F1 ; check if the coordinates equals screen out of bound area
  BEQ missilereset ; if result is zero, branch to missilereset label to reset the status of the missile
  
 ;if it has been fired, add 2 to x-pos of missile $0213
  LDA fired
  BEQ nmi_end ; branch if equal to zero, branch to nmi_end if it hasnt been fired // this works without CMP
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
