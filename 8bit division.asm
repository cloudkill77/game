lda #$a          ; load &
sta $0           ; store dividend
sta $10          ; store dividend for subtraction
lda #$8          ; load &
sta $1           ; store divisor
define temp $5f  ; temp variable used for 10x multiplication of the remainder

loop:
inx       ; keep track of quotient = x
sec
lda $10   ; load dividend
sbc $1    ; subtract divisor
sta $10   ; store new dividend value
beq zero  ; if A is equal to zero, branch to label "zero"
cmp $1    ; compare A to divisor
bcc less  ; if A is less than divisor, branch to label "less"
jmp loop ; repeat loop for repeated subtractions

loop1:
iny
sec
lda $11
sbc $1
sta $11
beq zero1
cmp $1
bcc less1
jmp loop1

loop2:
iny
sec
lda $12
sbc $1
sta $12
beq zero2
cmp $1
bcc less2
jmp loop2

zero:
stx $20   ; no fractions, store quotient in $20
brk

zero1:
sty $21   ; store quotient1 in $21
brk

zero2:
sty $22   ; store quotient2 in $22
brk

less:     ; perform 10x multiplication of first remainder
stx $20   ; store quotient1 in $20
asl       ; x2  
sta temp  ; store product
asl       ; x4
asl       ; x8
clc
adc temp  ; add product
sta $11   ; store remainder
jmp loop1 ; jmp to loop1 to perform division by subtraction

less1:    ; perform 10x multiplication of second remainder
sty $21   ; store quotient2 in $21
asl       ; x2
sta temp  ; store product
asl       ; x4
asl       ; x8
clc
adc temp  ; add product
sta $12   ; store remainder
ldy #$0   ; reset quotient counter to 0
jmp loop2

less2:
sty $22   ; store quotient2 in $22
brk
