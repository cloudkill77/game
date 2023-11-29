enemy_y = 0
enemy_x = ff
define pattern1 = #$0


start:
lda enemy_homing ; is enemy homing?
bne skippattern ; yes, skip pattern
ldy pattern1
cmp #$3c   ; is A less than 60?
bcs skippattern: no
inc enemy_y
inc pattern1





ontarget_reset:
lda #$0
sta enemy_homing  ; reset enemy homing
sta pattern1      ; reset pattern1 count

ontarget:

skippattern:
