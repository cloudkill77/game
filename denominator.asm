; Find divisors of a two-digit number in 6502 assembly
; Input: A = two-digit number
; Output: Prints divisors to console
; from ChatGPT, not tested yet

        LDX #01          ; Set X to 1 (start divisor at 1)
CheckDivisor:
        TXA              ; Transfer X (divisor) to A
        STA TempDiv      ; Store X in a temporary location (TempDiv)
        LDA Num          ; Load the original number
        SEC              ; Set carry for subtraction
LoopSubtract:
        SBC TempDiv      ; Subtract the current divisor (X)
        BCS LoopSubtract ; Keep subtracting while result >= 0

        ; If the result is exactly 0, it's a divisor
        BEQ IsDivisor    ; Branch if the result is exactly zero (divisible)

NextDivisor:
        INX              ; Increment X (try next divisor)
        CPX Num          ; Compare X with the original number
        BNE CheckDivisor ; If X <= number, keep checking
        JMP Done         ; If we've checked all divisors, we're done

IsDivisor:
        ; Print or store the divisor (in X register)
        ; Add your print routine here
        JMP NextDivisor  ; Go to the next possible divisor

Done:
        RTS              ; Return from subroutine

; Data Section
Num:        .byte $32    ; Example number (50)
TempDiv:    .byte $00    ; Temporary storage for divisor