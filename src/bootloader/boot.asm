;; ORG (directive)
;; Tells assembler where we expect our code to be laoded. The assembler uses
;; this information to calculate label addresses.
;; BITS (directive)
;; Tells assembler to emit 16/32/64-bit code.
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
ebr_volume_label:           db 'DragonArch'         ; 11 bytes padded
ebr_system_id:              db 'FAT12   '           ; 8 bytes

;;
;; Code goes here
;;

start:
    ;; setup data segments
    mov ax, 0                           ; can't set ds/es directly
    mov ds, ax
    mov es, ax

    ;; setup stack
    mov ss, ax
    mov sp, 0x7C00                      ; stack grows downwards from where we are loaded in memory

    ;; some BIOSes might start us at 07C0:0000 instead of 0000:7C00, make sure we are in the
    ;; expected location
    push es
    push word .after
    retf
.after:
    ;; read somethings from floppy disk
    ;; BIOS should set DL to drive number
    mov [ebr_drive_number], dl

    ;;  read something from floppy disk
    ;;  BIOS should set DL to drive number
    mov [ebr_drive_number], dl

    ;; show loading message
    mov si, msg_loading
    call puts

    ;; read drive parameters (sectors per track and head count),
    ;; instead of relying on data on formated disk
    push es
    mov ah, 08h
    int 13h
    jc floppy_error
    pop es

    and cl, 0x3F                        ; remove top 2 bits
    xor ch, ch
    mov [bdb_sectors_per_track], cx     ; sectors count

    inc dh
    mov [bdb_heads], dh                 ; head count

    ;; compute LBA of root directory = reversed * fats * sectors_per_fat
    ;; note: this section can be hardcoded
    mov ax, [bdb_sectors_per_fat]
    mov bl, [bdb_fat_count]
    xor bh, bh
    mul bx                              ; ax = (fats * sectors_per_fat)
    add ax, [bdb_reserved_sectors]      ; ax = LBA of root directory
    push ax

    ;; compute size of root directory = 32 * (number_of_entrys) / bytes_per_sector
    mov ax, [bdb_sectors_per_fat]
    shl ax, 5                           ; ax *= 32
    xor dx, dx                          ; dx = 0
    div word [bdb_bytes_per_sector]    ; number of sectors we need to read

    test dx, dx                         ; if dx !=0, add
    jz .root_dir_after
    inc ax                              ; division remainder != 0
                                        ; this means we have a sector only partially filled with entries

.root_dir_after:
    ;; read root directory
    mov cl, al                          ; cl = number of sectors to read = size of root directory
    pop ax                              ; ax = LBA of root directory
    mov dl, [ebr_drive_number]          ; dl = drive number (we saved it previously)
    mov bx, buffer                      ; es:bx = buffer
    call disk_read

    ;; search for kernel.bin
    xor bx, bx
    mov di, buffer

.search_kernel:
    mov si, file_kernel_bin
    mov cx, 11                          ; compare up to 11 characters
    push di

    ;; cmpsb
    ;; compare the two bytes located in memory at address ds:si and es:di
    ;; si and di are incremented (when direction flag=0) or decremented (when direction flag=1)
    ;; comparasion is performed sismilary to the CMP instruction - a subtraction is performed,
    ;; and the flags are set accordingly
    ;; cmpsw, cmpsd, cmpsq are equivalent for comparing words , double words, quads.

    ;; repe
    ;; repeats a string instruction while the operands are equal (zero flag=1), or until
    ;; cx reaches 0
    ;; cx is decremented on each iteration
    ;; comparasion is performed sismilary to the CMP instruction - a subtraction is performed,
    ;; and the flags are set accordingly
    repe cmpsb
    pop di
    je .found_kernel

    add di, 32
    inc bx
    cmp bx, [bdb_dir_entries_count]
    jl .search_kernel

    ;; kernel not found
    jmp kernel_not_found_error

.found_kernel:
    ;; di should have the address to the entry
    mov ax, [di + 26]                   ; first logical cluster field (offset 26)
    mov [kernel_cluster], ax

    ;; load FAT from disk into memory
    mov ax, [bdb_reserved_sectors]
    mov bx, buffer
    mov cl, [bdb_sectors_per_fat]
    mov dl, [ebr_drive_number]
    call disk_read

    ;; read kernel and process FAT chain
    mov bx, KERNEL_LOAD_SEGMENT
    mov es, bx
    mov bx, KERNEL_LOAD_OFFSET

.load_kernel_loop:
    ;; read next cluster
    mov ax, [kernel_cluster]

    ;; not nice hardcoded value
    add ax, 31                          ; first cluster = (kernel_cluster - 2) * sectors_per_cluster + start_sector
                                        ; start sector = reversed + fats + root directory size = 1 * 18 + 134 = 33

    mov cl, 1
    mov dl, [ebr_drive_number]
    call disk_read

    add bx, [bdb_bytes_per_sector]

    ;; compute location of next cluster
    mov ax, [kernel_cluster]
    mov cx, 3
    mul cx
    mov cx, 2
    div cx                              ; ax = index of entry in FAT, dx = cluster mod 2

    mov si, buffer
    add si, [ds:si]                     ; read entry from FAT table at index ax

    or dx, dx
    jz .even

.odd:
    shr ax, 4
    jmp .next_cluster_after

.even:
    and ax, 0x0FFF

.next_cluster_after:
    cmp ax, 0x0FF8                      ; end of chain
    jae .read_finish

    mov [kernel_cluster], ax
    jmp .load_kernel_loop

.read_finish:
    ;; jump to our kernel
    mov dl, [ebr_drive_number]          ; boot device in dl

    mov ax, KERNEL_LOAD_SEGMENT         ; set segment registers
    mov ds, ax
    mov es, ax

    jmp KERNEL_LOAD_SEGMENT:KERNEL_LOAD_OFFSET
    jmp wait_key_and_reboot             ; should never happen

    cli                                 ; disables interrupts, this way CPU can't get out of "halt" state
    hlt                                 ; HLT: Stops from executing (it can be resumed by an interrupt).

;;
;; Error handlers
;;
floppy_error:
    mov si, msg_read_failed
    call puts
    jmp  wait_key_and_reboot

kernel_not_found_error:
    mov si, msg_kernel_not_found
    call puts
    jmp wait_key_and_reboot

wait_key_and_reboot:
    mov ah, 0
    int 16h                             ; wait for keypress
    jmp 0FFFFh:0                        ; jump to beginning of BIOS, should reboot

.halt:
    cli                                 ; disables interrupts, this way CPU can't get out of "halt" state
    htl

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

;;
;; Disk Routines
;;

;;
;; Converts an LBA address to a CHS address
;; Parameters:
;;   - ax: LBA address
;; Returns:
;;   - cx [bits 0-5]: sector
;;   - cx [bits 6-15]: cylinder
;;   - dh: head
;;
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

;;
;; Reads sectors from a dis
;; Parameters:
;;   - ax: LBA address
;;   - cl: number of sectors to read (up to 128)
;;   - dl: drive number
;;   - es:bx: memory address where to store read data
;;
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

msg_loading:            db 'Loading...', ENDL, 0
msg_read_failed:        db 'Read from disk failed', ENDL, 0
msg_kernel_not_found    db 'KERNEL.BIN file not found!', ENDL, 0

file_kernel_bin:        db 'KERNEL  BIN'
kernel_cluster:         dw 0

KERNEL_LOAD_SEGMENT     equ 0x2000
KERNEL_LOAD_OFFSET      equ 0

;; DB byte1, byte2, byte3... (directive)
;; Stands for "define byte(s)". Writes given byte(s) to the assembled binary file.
;; TIMES number instruction/data (directive)
;; Repeats given instruction or piece of data a number of times.
;; $: Special symbol which is equal to the memory offset of the current line.
;; $$: Special symbol which is equal to the memory offset of the beginning of the current section (in or case, program).
;; $-$$: Gives the size of our program so far (in bytes).
;; DW word1, word2, word3... (directive)
;; Stands for "define word(s)". Writes given word(s) (2 bytes value, encoded in little endian) to the assembler binary file.
times 510-($-$$) db 0
dw 0AA55h

buffer:
