    !cpu 6502
    !to "hello-world.prg",cbm

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
    jsr init_text		; write line of text

loop
    lda $c5
    and #$01
    beq loop			; If not pressed, continue looping
    jsr restore
    rts				; If pressed, exit

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

restore
    lda store
    sta $d020
    lda store+1
    sta $d021
    rta

init_text
    ldx #$00			; load x register with $00
loop_text
    lda message,x		; read characters from message data...
    sta $0590,x			; ...and place it into screen memory
    inx				; increment to next character
    cpx #$28			; false if != 40
    bne loop_text		; loop if false
    rts

store
    !byte $00,$00,$00
message
    !scr "           -= hello world! =-           "		;40 cols of text
