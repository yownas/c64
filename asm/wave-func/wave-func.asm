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
    jsr init_text
    jsr wave_init

loop
    jsr wave

    lda $91			; STKEY - check if stop key was pressed
    bmi loop			; negative? then loop

    jsr restore
    rts

; clear screen and store background colors
blank
; TODO loop 40 x 25 and set characters and colors to blank
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

init_text
    ldx #$00			; load x register with $00
loop_text
    lda message,x		; read characters from message data...
    sta $0590,x			; ...and place it into screen memory
    inx				; increment to next character
    cpx #$28			; false if != 40
    bne loop_text		; loop if false
    rts

;random
;    // generate random number between 0 and 255
;    lda CIA1_TIMER_A_LO
;    eor CIA1_TIMER_A_HI
;    eor CIA2_TIMER_A_LO
;    adc CIA2_TIMER_A_HI
;    eor CIA2_TIMER_B_LO
;    eor CIA2_TIMER_B_HI
;
;    // greater than 99? then subtract 100
;    cmp #100
;    bcc rnd_done
;    sbc #100
;
;    // still greater than 99? then subtract 100 more
;    cmp #100
;    bcc rnd_done
;    sbc #100
;
;    clc
;rnd_done
;    adc #1


wave_init
    lda #$00			; Set X & Y to 0
    sta wave_pos
    sta wave_pos+1

    lda #$53
    sta $0401
    rts

;========================
!zone WAVE_FUNCTION
;========================
; Do the thing

wave
    ; Find number of lowest choices - if we find one with 0 -> fail, goto blank
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
    
    ldx shape_count
    jsr random
    rol
    tax
    dex
    lda shapes,x
    jsr putchar
   
    rts

;========================
!zone CHAR
;========================

wave_ptr = $fb			; Unused byte in ZP
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

.tmp_char
    !byte $00

;========================
!zone RANDOM
; Return A * 40 to wave_ptr
; FIXME clean up, take two addresses instead of A
;========================

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
    asl			; * 8
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
    lda $dc04
    adc .rnd_pool
    sta .rnd_pool
.mod
    sec 
    sbc .rnd_mod
    bcs .mod
    adc .rnd_mod
    rts

.rnd_mod
    !byte $00, $00
.rnd_pool
    !byte $00

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

wave_pos
    !byte $00, $00
wave_tmp
    !byte $00

store
    ; BG color 1, 2
    !byte $00,$00

shape_count
    !byte $00
shapes
    ; * = $2a, dot = $71
    ; SPACE, LD, UR, LU, DR
    ; !byte $20, $69, $6a, $6b, $75

    ; CHAR, 0000DownRightLeftUp
    !byte $20, %00000000 ; SPACE
    !byte $49, %00001010 ; LD
    !byte $4a, %00000101 ; UR
    !byte $4b, %00000011 ; LU
    !byte $55, %00001100 ; DR
    !byte $51, %00000000 ; DOT

    !byte $00 ; END

message
    !scr "           -= hello world! =-           "		;40 cols of text
