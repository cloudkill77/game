; homing
  lda x2small  ; enemy has passed player and will give up chase
  cmp #$1
  beq ontarget_reset

  lda ysequal  ; enemy is on target and maintains course
  cmp #$1
  beq ontarget

  lda y1small  ; enemy is below and moves up to intercept
  cmp #$1
  beq up

  lda y2small  ; enemy is above and moves down to intercept
  cmp #$1
  beq down

  brk
  jmp ontarget

up:
; enemy moves up
  inc enemy_yr
  lda enemy_yr
  cmp #$9      ; is accumulator less than 3? this delays the enemys reaction
  bcc ontarget ; yes, branch to ontarget
  dec enemy_y
  lda #$7      ; subsequent course changes are responded to more swiftly, but not instanstaneously
  sta enemy_yr 
  jmp ontarget

down:
; enemy moves down
  inc enemy_yr
  lda enemy_yr
  cmp #$9      ; is accumulator less than 3? this delays the enemys reaction
  bcc ontarget ; yes, branch to ontarget
  inc enemy_y
  lda #$7       ; subsequent course changes are responded to more swiftly, but not instanstaneously
  sta enemy_yr
  jmp ontarget

ontarget_reset:
  lda #$0
  sta enemy_yr  ; reset the enemy reaction time as the enemy has passed the player
ontarget: