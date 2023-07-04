; ORG (directive)
; Tells assembler where we expect our code to be laoded. The assembler uses
; this information to calculate label addresses.
; BITS (directive)
; Tells assembler to emit 16/32/64-bit code.
org 0x7C00
bits 16

%define ENDL 0x0D, 0x0A

start:
    jmp main

;
; Prints a string in the screen
; Params:
;      - ds::si points to string
;
puts:
    ; save registers we will modify
    push si
    push ax
    push bx

.loop:
    ;; LODSB, LODSW, LODSD
    ;; These instructions load a byte/word/double-word from DS:SI into
    ;; AL/AX/FAX, then increments SI by the number of bytes loaded.
    lodsb                       ; loads next character in al

    ;; OR destination, source
    ;; Performs bitwise OR between source and destination, stores
    ;; result in destination. Also modifies some flags in the flags
    ;; registers, such as the Zero Flag if the result is zero.
    or al, al                   ; verify if next character is null ?
    jz .done                    ; JZ destination, Jumps to destination if zero flag is set.

    mov ah, 0x0E                ; call bios interrupt
    mov bh, 0                   ; set page number to 0
    int 0x10

    jmp .loop

.done:
    pop bx
    pop ax
    pop si
    ret

main:
    ;; setup data segments
    mov ax, 0                   ; can't set ds/es directly
    mov ds, ax
    mov es, ax

    ;; setup stack
    mov ss, ax
    mov sp, 0x7C00              ; stack grows downwards from where we are loaded in memory

    ; print message
    mov si, msg_hello
    call puts

    ; HLT: Stops from executing (it can be resumed by an interrupt).
    hlt

.halt:
    jmp .halt

msg_hello: db 'Dragon Arch by Joao Leonardi', ENDL, 0

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
