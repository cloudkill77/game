define gravt $0

loop:
ldx gravt
lda grav, x
sta $0200
inx
stx gravt

jmp loop

grav:
dcb 0
dcb 1
dcb 2
dcb 4
dcb 6
dcb 8
dcb 11
dcb 14
dcb 18
dcb 23
