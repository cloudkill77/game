  lda #$0
  sta ysequal
  sta y1small
  sta y2small
  sta xsequal
  sta x1small
  sta x2small

checky:
  lda player_y 
  cmp enemy_y   ; is accumulator (y1) less than y2?
  beq l_ysequal ; neither, y1 is equal to y2
  bcc l_y1small ; yes, y1 (accumulator) is less than y2
  bcs l_y2small ; no, y2 is less than y1 (accumulator)
error0:
  brk

l_ysequal:
  lda #$1
  sta ysequal ; record y1=y2
  jmp checkx 

l_y1small:
  lda #$1
  sta y1small ; record y1<y2
  jmp checkx 

l_y2small:
  lda #$1
  sta y2small ; record y2<y1
  jmp checkx 

checkx:
  lda player_x
  cmp enemy_x   ; is accumulator (x1) less than x2?
  beq l_xsequal ; neither, x1 is equal to x2
  bcc l_x1small ; yes, x1 (accumulator) is less than x2
  bcs l_x2small ; no, x2 is less than x1 (accumulator)
error1:
  brk

l_xsequal:
  lda #$1
  sta xsequal ; record x1=x2
  jmp check1end

l_x1small:
  lda #$1
  sta x1small ; record x1<x2
  jmp check1end

l_x2small:
  lda #$1
  sta x2small ; record x2<x1
  jmp check1end

check1end:
  lda y1small
  cmp #$1
  beq l_y2y1 ; branch to y2-y1 calculation

  lda y2small
  cmp #$1
  beq l_y1y2 ; branch to y1-y2 calculation

  lda ysequal
  cmp #$1
  beq l_ynop
error2:
  brk
  
l_y2y1:
  sec
  lda enemy_y
  sbc player_y
  sta yd ; record y-delta
  jmp calcx

l_y1y2:
  sec
  lda player_y
  sbc enemy_y
  sta yd ; record y-delta
  jmp calcx

l_ynop:
  lda #$0 
  sta yd ; record y-delta of 0
  jmp calcx  

calcx:
  lda x1small
  cmp #$1
  beq l_x2x1 ; branch to x2-x1 calculation

  lda x2small
  cmp #$1
  beq l_x1x2 ; branch to x1-x2 calculation

  lda xsequal
  cmp #$1
  beq l_xnop
error3:
  brk

l_x2x1:
  sec
  lda enemy_x
  sbc player_x
  sta xd ; store x-delta
  jmp end3

l_x1x2:
  sec
  lda player_x
  sbc enemy_x
  sta xd ; store x-delta
  jmp end3 

l_xnop:
  lda #$0
  sta xd ; store x-delta of 0
  jmp end3

end3:
 
; proximity detector (60 deep by 224 wide) for homing
  lda yd
  cmp #$3c  ; is the y-delta less than 60?
  bcs clear ; no, branch to clear
  lda xd    
  cmp #$e0  ; is the x-delta less than 90?
  bcs clear ; no, branch to clear
  lda #$1
  sta enemy_homing ; set enemy to be homing
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
  bcc ontarget ; yes, branch to ontarget until delay counter has reached its threshold
  dec enemy_y  ; move to intercept
  lda #$7      ; subsequent course changes are responded to more swiftly, but not instanstaneously
  sta enemy_yr 
  jmp ontarget

down:
; enemy moves down
  inc enemy_yr
  lda enemy_yr
  cmp #$9      ; is accumulator less than 3? this delays the enemys reaction
  bcc ontarget ; yes, branch to ontarget until delay counter has reached its threshold
  inc enemy_y  ; move to intercept
  lda #$7      ; subsequent course changes are responded to more swiftly, but not instanstaneously
  sta enemy_yr
  jmp ontarget

ontarget_reset:
  lda #$0
  sta enemy_yr  ; reset the enemy reaction time as the enemy has passed the player
  sta enemy_homing
ontarget:

  jsr collision  
  
; within firing range.  is the enemy homing?
  lda enemy_homing
  cmp #$1    ; is acumulator equal to 1?
  bne clear  ; no, branch to clear
  lda yd
  cmp #$10  ; is the y-delta less than 10?
  bcs clear ; no, branch to clear
; start firing
  jsr firing

clear:
;  jmp endcollision 