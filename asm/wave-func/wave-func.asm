    !cpu 6502
    !to "wave-func.prg",cbm

;    .const CIA1_TIMER_A_LO = $dc04
;    .const CIA1_TIMER_A_HI = $dc05
;    .const CIA1_TIMER_B_LO = $dc06
;    .const CIA1_TIMER_B_HI = $dc07

;    .const SCR_MEM = $0400
;    .const COL_MEM = $d800

; add a BASIC startline (generate sys)

    * = $0801
    !word entry
    !word $07ea
    !byte $9e
    !text "2066"
    !byte $00
    !word $00

; entry point at $0812 (2066)

    * = $0812

entry
    jsr blank
    jsr init_shapes    
    jsr wave_init

loop
    jsr wave

    lda $91			; STKEY - check if stop key was pressed
    bmi loop			; negative? then loop

    jsr restore
    rts

; clear screen and store background colors
blank
    jsr $e544			; clear
    lda $d020			; save old colors
    sta store
    lda $d021
    sta store+1
    lda #$00			; black
    sta $d020
    sta $d021
    rts

; restore background colors
restore
    lda store
    sta $d020
    lda store+1
    sta $d021
    rts

; not used
init_text
    ldx #$00			; load x register with $00
loop_text
    lda message,x		; read characters from message data...
    sta $0590,x			; ...and place it into screen memory
    inx				; increment to next character
    cpx #$28			; false if != 40
    bne loop_text		; loop if false
    rts

;========================
!zone WAVE_FUNCTIONS
;========================
; Do the thing

wave_init
    ; Fill screen
    lda #$00			; Set X & Y to 0
    sta wave_pos
    sta wave_pos+1
    sta wave_pass
    lda #$53			; char to fill with
    sta wave_char
.fill_loop
    lda wave_char
    jsr putchar
    inc wave_pos+1
    lda wave_pos+1
    cmp #40
    bcc .fill_loop
    lda #$00		; Back to start
    sta wave_pos+1
    inc wave_pos	; Next row
    lda wave_pos
    cmp #25
    bcc .fill_loop

    lda #$00			; Set X & Y to 0 (again)
    sta wave_pos
    sta wave_pos+1

    rts

wave
    ; FIXME: 
    ;  pass 0: count possible shapes for all
    ;  pass 1: find lowest (lowest has count of 0 -> end)
    ;  pass 2: pick random place, place random masked shape -> pass 0
    ;  pass 3: end, done

    ; Find number of lowest choices - if we find one with 0 -> fail, goto blank
    lda #$00		; Reset char, counter and pos
    sta wave_pos
    sta wave_pos+1
    sta wave_char
    sta wave_cnt+1
    lda #$01
    sta wave_cnt

    jsr getchar
    sta wave_char
.search_loop
    ; inc wave_pos+1
    inc wave_pos+1
    lda wave_pos+1
    cmp #40
    bcc .search_loop_end
    lda #$00		; Back to start
    sta wave_pos+1
    inc wave_pos	; Next row
    lda wave_pos
    cmp #25
    bcs .search_end
    lda #$00		; Back to start
    sta wave_pos
.search_loop_end

;if not "empty", rts

;    jsr getmask
;get & add masks form neighbours
; Mask is what current char need: UpLeftRigthDown
;   UDLR ->
; U 0100 -> 100
; D 1000 -> 0100
; L 0001 -> 0010
; R 0010 -> 0001
;      or = mask
;count shapes matching  mask
    

; if same as wave_char, count up
; if lower than wave_char, switch    
; if higher than wave_char, skip

.search_end
    ; We should have wave_cnt of wave_char, the lowest char on screen

    ; Select random
    ; Check choises - If empty, just pick random shape
      ; find char in rules; for up, left, right, down
      ;
    ; Select one

    ; TEST
    ; set random char at X Y

    ldx #25
    jsr random
    sta wave_pos

    ldx #40
    jsr random
    sta wave_pos+1
    
    ldx shape_count	; Get random shape
    jsr random
    rol
    tax
    dex
    lda shapes,x
    jsr putchar
   
    rts

wave_pass
    !byte $00
wave_char
    !byte $00
wave_cnt
    !byte $00,$00
wave_pos
    !byte $00, $00
wave_tmp
    !byte $00

;========================
!zone CHAR
;========================

wave_ptr = $fb			; Unused word in ZP
putchar
    sta .tmp_char
    lda #$00
    sta wave_tmp
    sta wave_ptr+1

    ; Get X offset
    lda wave_pos

    jsr mul40			; return A * 40

    clc				; Add base addr
    lda #$00
    adc wave_ptr
    sta wave_ptr
    lda #$04
    adc wave_ptr+1
    sta wave_ptr+1
    
    lda .tmp_char
    ldy #$00
    sta (wave_ptr),y
    rts

getchar
    lda #$00
    sta wave_tmp
    sta wave_ptr+1

    ; Get X offset
    lda wave_pos

    jsr mul40			; return A * 40

    clc				; Add base addr $0400 (screen)
    lda #$00
    adc wave_ptr
    sta wave_ptr
    lda #$04
    adc wave_ptr+1
    sta wave_ptr+1
    
    ldy #$00
    lda (wave_ptr),y
    rts

.tmp_char
    !byte $00

; FIXME clean up, take two addresses instead of A?
mul40
    sta .in
    sta wave_ptr
    ldx #$00
    stx .mul_tmp
wave_mul32
    asl wave_ptr
    rol wave_ptr+1

    inx
    cpx #$05
    bne wave_mul32
    
    lda .in
    asl				; * 8
    rol .mul_tmp
    asl
    rol .mul_tmp
    asl
    rol .mul_tmp

    clc				; add * 8 part
    adc wave_ptr 
    sta wave_ptr 
    lda .mul_tmp
    adc wave_ptr+1
    sta wave_ptr+1

    clc				; Add Y
    lda wave_pos+1
    adc	wave_ptr
    sta wave_ptr
    lda #$00
    adc	wave_ptr+1
    sta wave_ptr+1
    rts

.in
    !byte $00
.mul_tmp
    !byte $00

;========================
!zone RANDOM
;========================

random
    stx .rnd_mod
    lda $dc04			; Timer A
    adc .rnd_pool+1		; Add pool
    adc .rnd_pool
    sta .rnd_pool		; Store current to pool
    ror				; Mix
    ror
    ror
    sta .rnd_pool+1
.mod				; mod X
    sec 
    sbc .rnd_mod
    bcs .mod
    adc .rnd_mod
    rts

.rnd_mod
    !byte $00, $00
.rnd_pool
    !byte $00,$00

;========================
!zone SHAPE_FUN
;========================

init_shapes
    ldx #$00
    ldy #$00
.cnt_loop
    lda shapes,y
    cmp #$00
    beq .cnt_end
    inx
    iny
    iny
    jmp .cnt_loop
.cnt_end
    stx shape_count
    rts 

;========================
!zone DATA
;========================
; Store the things

store
    ; BG color 1, 2
    !byte $00,$00

shape_count
    !byte $00		; run init_shapes to set this
shapes
    ; CHAR, 0000UpDownLeftRight
    !byte $49, %00000110 ; LD curve
    !byte $4a, %00001001 ; UR curve
    !byte $4b, %00001010 ; LU curve
    !byte $55, %00000101 ; DR curve
    !byte $5b, %00001111 ; Cross

    !byte $6e, %00000110 ; LD
    !byte $6d, %00001001 ; UR
    !byte $7d, %00001010 ; LU
    !byte $70, %00000101 ; DR

    !byte $6b, %00001101 ; UDR
    !byte $72, %00000111 ; DLR
    !byte $73, %00001110 ; UDL
    !byte $71, %00001011 ; ULR

    !byte $20, %00000000 ; SPACE
    !byte $00 ; END

    !byte $51, %00000000 ; DOT

message
    !scr "           -= hello world! =-           "		;40 cols of text
