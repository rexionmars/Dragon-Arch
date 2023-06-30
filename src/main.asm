; ORG (directive)
; Tells assembler where we expect our code to be laoded. The assembler uses
; this information to calculate label addresses.
org 0x7C00

; BITS (directive)
; Tells assembler to emit 16/32/64-bit code.
bits 16


main:
    ; HLT: Stops from executing (it can be resumed by an interrupt).
    hlt

.halt:
    jmp .halt

; DB byte1, byte2, byte3... (directive)
; Stands for "define byte(s)". Writes given byte(s) to the assembled binary file.
; TIMES number instruction/data (directive)
; Repeats given instruction or piece of data a number of times.
; $: Special symbol which is equal to the memory offset of the current line.
; $$: Special symbol which is equal to the memory offset of the beginning of the current section (in or case, program).
; $-$$: Gives the size of our program so far (in bytes).
; DW word1, word2, word3... (directive)
; Stands for "define word(s)". Writes given word(s) (2 bytes value, encoded in little endian) to the assembler binary file.
times 510-($-$$) db 0
dw 0AA55h
