gamestate  .rs 1  ; .rs 1 means reserve one byte of space
buttons1   .rs 1  ; player 1 gamepad buttons, one bit per button
buttons2   .rs 1  ; player 2 gamepad buttons, one bit per button
score1     .rs 1  ; player 1 score, 0-15
score2     .rs 1  ; player 2 score, 0-15





;; DECLARE SOME CONSTANTS HERE
STATEINTRO     = $00  ; display into screen
STATEREADY2GO  = $01  ; displaying ready to go screen
STATEPLAYING   = $02  ; move sprite, check for collisions
STATEGAMEOVER  = $03  ; displaying game over screen




;;:Set starting game state
  LDA #STATEINTRO
  STA gamestate




Forever:
  JMP Forever     ;jump back to Forever, infinite loop, waiting for NMI
  
NMI:
 
  JSR DrawScore
 
  JSR ReadController1  ;;get the current button data for player 1
  JSR ReadController2  ;;get the current button data for player 2 
  
  
    
GameEngine:  
  LDA gamestate
  CMP #STATEINTRO
  BEQ EngineIntro    ;;game is displaying intro screen
    
  LDA gamestate
  CMP #STATEREADY2GO
  BEQ EngineReady2Go  ;;game is displaying ready to go screen
  
  LDA gamestate
  CMP #STATEPLAYING
  BEQ EnginePlaying   ;;game is playing
  
  LDA gamestate
  CMP #STATEGAMEOVER
  BEQ EngineGameOver  ;;game is displaying ending screen
GameEngineDone:  
  
  JSR UpdateSprites  ;;set ball/paddle sprites from positions

  RTI             ; return from interrupt
 
 ;;;;;;;;
 
EngineIntro:
  ;;if start button pressed
  ;;  finish loading sprites
  ;;  prepare for game screen
  ;;  go to Ready2Go State
  ;;  turn screen on
  JMP GameEngineDone

;;;;;;;;; 
 
EngineGameOver:
  ;;if start button pressed
  ;;  turn screen off
  ;;  load Ready2Go screen
  ;;  go to Ready2Go State
  ;;  turn screen on 
  JMP GameEngineDone
 
;;;;;;;;;;;