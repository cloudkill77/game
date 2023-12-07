
define highbyte $0
define lowbyte $1

lda #$1
sta highbyte  ; 2*256 = 512
lda #$40
sta lowbyte

start:
LDA lowbyte      ; load low 8 bits of 16 bit value
CLC              ; clear carry
ADC #$60        ; add 1
STA lowbyte      ; done with low bits, save back
LDA highbyte     ; load upper 8 bits
ADC #$00         ; add 0 and carry from previous add
STA highbyte     ; save back

jmp start