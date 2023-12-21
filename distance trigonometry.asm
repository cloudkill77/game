; calculate distance between two coordinates using pythagoras theorem
; manually set y and x-positions are subtracted from each other to get the yd and xd (y-delta and x-delta). these are then processed using pythagoras' theorem to get the length of the hypotenuse (ie the distance between the two points). currently p1y and p1x need to be smaller than e1y and e1x for the calculation to work.




define p1y $00
define p1x $01
define e1y $10
define e1x $11
define i1y $20
define i1x $21
define squarey $30
define squarex $31
define tempy $40
define tempx $41

lda #$44
sta p1y
lda #$43
sta p1x
lda #$41
sta e1y
lda #$3f
sta e1x

; load p1x
; load e1x
; load p1y
; load e1y

lda p1y
sec
sbc e1y
sta i1y
lda p1x
sec
sbc e1x
sta i1x

; square i1y
; square i1x


clc
ldx #$00
loopy:
lda $30    ; load result
adc $20   ; add factor 1
sta $30    ; store as result
inx            ; increment x
cpx $20       ; compare x against factor 2
bne loopy   ; loop if x is less than factor 2

clc
ldx #$00
loopx:
lda $31
adc $21
sta $31
inx
cpx $21
bne loopx

; a^2 + b^2 = c^2

clc
lda $30
adc $31
sta $32

; √(c^2)

; © Lee Davison
; sqrt2
; Calculates the 8 bit root and 9 bit remainder of a 16 bit unsigned integer in
; Numberl/Numberh. The result is always in the range 0 to 255 and is held in
; Root, the remainder is in the range 0 to 511 and is held in Reml/Remh
;
; partial results are held in templ/temph
;
; This routine is the complement to the integer square program.
;
; Destroys A, X registers.

; variables - must be in RAM

define Numberl	 $50		; number to find square root of low byte
define Numberh		$51	; number to find square root of high byte
define Reml	 $52		; remainder low byte
define Remh	 $53	; remainder high byte
define templ		$54		; temp partial low byte
define temph	 $55	; temp partial high byte
define Root		$22		; square root

lda $32   ; load c^2
sta Numberl
lda #$0
sta Numberh ; 0 for now


SqRoot:
LDA	#$00		  ; clear A
STA Reml     ; clear remainder low byte
STA Remh    ; store remainder high byte
STA Root      ; clear Root
LDX	#$08		; 8 pairs of bits to do
Loop:
ASL Root ;  Root = Root * 2
ASL Numberl ; shift highest bit of number ..
ROL Numberh ;
ROL Reml ; .. into remainder
ROL Remh ;
ASL Numberl ; shift highest bit of number ..
ROL Numberh ;
ROL Reml  ; .. into remainder
ROL Remh ;

LDA Root ; copy Root ..
STA templ ; .. to templ
LDA #$00 ; clear byte
STA temph ; clear temp high byte

SEC			; +1
ROL templ ; temp = temp * 2 + 1
ROL temph ;

LDA Remh  ; get remainder high byte
CMP temph ; compare with partial high byte
BCC Next ; skip sub if remainder high byte smaller

BNE Subtr ; do sub if <> (must be remainder>partial !)

LDA Reml ; get remainder low byte
CMP templ ; compare with partial low byte
BCC Next ; skip sub if remainder low byte smaller

				; else remainder>=partial so subtract then
				; and add 1 to root. carry is always set here
Subtr:
LDA Reml  ; get remainder low bytr
SBC templ ; subtract partial low byte
STA Reml ; save remainder low byte
LDA Remh  ; get remainder high byte
SBC temph ; subtract partial high byte
STA Remh ; save remainder high byte

INC Root ; increment Root
Next:
DEX			; decrement bit pair count
BNE Loop		; loop if not all done

brk

