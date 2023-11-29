enemy1_y = 0
enemy1_x = fa
enemy2_y = 5
enemy2_x = ff
enemy1pattern = #$3c
enemy2pattern = #$64

define enemy1_homing $30
define enemy2_homing $31
define pattern1count $40 ; depth of swoop
define pattern2count $41 ; depth of swoop

lda #$0
sta pattern1count  ; index for pattern loop
sta pattern2count  ; index for pattern loop

start:
lda enemy1_homing  ; is enemy homing?
bne skippattern    ; yes, skip pattern
ldy pattern1count
cmp enemy1pattern  ; is A less than 60?
bcs skippattern:   ; no, branch to skippattern
inc enemy1_y       ; follow pattern
inc pattern1count  ; increase pattern count


ontarget_reset:   ; reset after target has passed
lda #$0
sta enemy_homing  ; reset enemy homing
sta pattern1count ; reset pattern1 count

ontarget:

skippattern:
