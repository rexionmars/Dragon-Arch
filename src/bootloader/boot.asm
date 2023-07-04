; ORG (directive)
; Tells assembler where we expect our code to be laoded. The assembler uses
; this information to calculate label addresses.
; BITS (directive)
; Tells assembler to emit 16/32/64-bit code.
org 0x7C00
bits 16

%define ENDL 0x0D, 0x0A

;;
;; Fat 12 header
;;
jmp short start
nop

bdb_oem                     db 'MSWIN4.1'           ; 8bytes
bdb_bytes_per_sector:       dw  512
bdb_sector_per_cluster:     db 1
bdb_reserved_sectors:       dw 1
bdb_fat_count:              db 2
bdb_dir_entries_count:      dw 0E0h
bdb_total_sectors:          dw 2880                 ; 2880 * 512 = 1.44MB
bdb_media_descriptor_type:  db 0F0h                 ; F0 = 3.5" floppy disk
bdb_sectors_per_fat:        dw 9                    ; 9 sectors/fat
bdb_sectors_per_track:      dw 18
bdb_heads:                  dw 2
bdb_hidden_sectors:         dd 0
bdb_large_sector_count:     dd 0

;; extended boot record
ebr_drive_number:           db 0                    ; 0x00 floppy, 0x80 hdd, userlines
                            db 0                    ; reversed
ebr_signature:              db 29h
ebr_volume_id:              db 12h, 34h, 56h, 78h   ; serial number, value doens't matter
ebr_volume_label:           db 'DragonArch'        ; 11 bytes padded
ebr_system_id:              db 'FAT12   '           ; 8 bytes

;;
;; Code goes here
;;

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

    ;;  read something from floppy disk
    ;;  BIOS should set DL to drive number
    mov [ebr_drive_number], dl

    mov ax, 1                   ; LBA=1, second sector from disk
    mov cl, 1                   ; 1 sector to read
    mov bx, 0x7E00              ; data should be after the bootloader
    call disk_read

    ; print message
    mov si, msg_hello
    call puts

    cli                                 ; disables interrupts, this way CPU can't get out of "halt" state
    hlt                                 ; HLT: Stops from executing (it can be resumed by an interrupt).

;;
;; Error handlers
;;
floppy_error:
    mov si, msg_read_failed
    call puts
    jmp  wait_key_and_reboot

wait_key_and_reboot:
    mov ah, 0
    int 16h                             ; wait for keypress
    jmp 0FFFFh:0                        ; jump to beginning of BIOS, should reboot

.halt:
    cli                                 ; disables interrupts, this way CPU can't get out of "halt" state
    htl

;
; Disk Routines
;

;
; Converts an LBA address to a CHS address
; Parameters:
;   - ax: LBA address
; Returns:
;   - cx [bits 0-5]: sector
;   - cx [bits 6-15]: cylinder
;   - dh: head
;
lba_to_chs:
    push ax
    push dx

    xor dx, dx                          ; dx = 0
    div word [bdb_sectors_per_track]    ; ax = LBA / SectorsPerTrack
                                        ; dx = LBA % SectorsPerTrack

    inc dx                              ; dx = (LBA % SectorsPerTrack + 1) = sector
    mov cx, dx                          ; cx = sector

    xor dx, dx                          ; dx = 0
    div word [bdb_heads]                ; ax = (LBA / SectorsPerTrack) / Heads = cylinder
                                        ; dx = (LBA / SectorsPerTrack) % Heads = head
    mov dh, dl                          ; dl = head
    mov ch, al                          ; ch = cylinder (lower 8 bits)
    shl ah, 6
    or cl, ah                           ; put upper 2 bits of cylinder in CL

    pop ax
    mov dl, al                          ; restore DL
    pop ax
    ret

;
; Reads sectors from a dis
; Parameters:
;   - ax: LBA address
;   - cl: number of sectors to read (up to 128)
;   - dl: drive number
;   - es:bx: memory address where to store read data
;
disk_read:
    push ax                             ; dave registers we will modify
    push bx
    push cx
    push dx
    push di

    push cx                             ; remporarily save CL (number of sectors to read)
    call lba_to_chs                     ; compute CHS
    pop ax                              ; AL = number of sectors to read

    mov ah, 02h
    mov di, 3                           ; retry count

.retry:
    pusha                               ; save all registers, we don't know what bios modifies
    stc                                 ; set carry flag, some BIOS'es don't set it
    int 13h                             ; carry flag cleared = success
    jnc .done                           ; jump if carry not set

    ;; read failed
    popa
    call disk_reset

    dec di
    test di, di
    jnz .retry

.fail:
    ;; after all attemps are exhausted
    jmp floppy_error

.done:
    popa

    pop di
    pop dx
    pop cx
    pop bx
    pop ax                             ; restore registers we will modified
    ret

;;
;; Reset disk controller
;; Paramenters:
;;  dl: drive number
;;
disk_reset:
    pusha
    mov ah, 0
    stc
    int 13h
    jc floppy_error
    popa
    ret

msg_hello:          db 'Dragon Arch by Leonardi', ENDL, 0
msg_read_failed:    db 'Read from disk failed', ENDL, 0

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
