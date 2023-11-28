; homing
  lda x2small
  cmp #$1
  jmp ontarget

  lda ysequal
  cmp #$1
  beq ontarget

  lda y1small
  cmp #$1
  beq up

  lda y2small
  cmp #$1
  beq down

  jmp ontarget

up:
; enemy moves up
  dec enemy_y
  jmp ontarget

down:
; enemy moves down
  inc enemy_y
  jmp ontarget


ontarget:
