; add an 8 bit number to a 16 bit number which is stored as "high" and "low" bytes in $00 and $01 respectively. if an overflow occurs after addition to the low byte, a carry flag is set. this carry flag then gets added to the high byte by adding #$00 with carry (ADC #$00).

define highbyte $00
define lowbyte $01

lda #$01
sta highbyte     ; eg 2*256 = 512
lda #$40
sta lowbyte

start:
LDA lowbyte      ; load low 8 bits of 16 bit value
CLC              ; clear carry
ADC #$60         ; add #$60 (for example)
STA lowbyte      ; done with low bits, save back
LDA highbyte     ; load upper 8 bits
ADC #$00         ; add 0 and carry from previous add
STA highbyte     ; save back

jmp start        ; keep adding value infinitely