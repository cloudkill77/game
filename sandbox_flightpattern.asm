enemy_y = 0
enemy_x = ff
define pattern1 = #$0


start:
lda enemy_homing ; is enemy homing?
bne skippattern ; yes, skip pattern
ldy pattern1
cmp #$3c   ; is A less than 60?
bc  skippattern: no
inc enemy_y
inc pattern1





ontarget_reset:
lda #$0
sta enemy_homing

ontarget:

skippattern:
