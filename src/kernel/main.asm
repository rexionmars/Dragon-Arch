;; ORG (directive)
;; Tells assembler where we expect our code to be laoded. The assembler uses
;; this information to calculate label addresses.
;; BITS (directive)
;; Tells assembler to emit 16/32/64-bit code.
org 0x0
bits 16

%define ENDL 0x0D, 0x0A

start:
    ;; print message
    mov si, msg_hello
    call puts

.halt:
    ;; HLT: Stops from executing (it can be resumed by an interrupt).
    cli
    hlt

;;
;; Prints a string in the screen
;; Params:
;;      - ds::si points to string
;;
puts:
    ;; save registers we will modify
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

msg_hello: db 'Dragon Arch from kernel', ENDL, 0
