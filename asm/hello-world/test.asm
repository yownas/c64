		!cpu 6502
		!to "test.prg",cbm

; add a BASIC startline (generate sys)

		* = $0801
		!word entry-2
		!byte $00,$00,$9e
		!text "2066"
		!byte $00,$00,$00

; entry point at $0812 (2066)

		* = $0812

entry
		jsr $e544
		jsr init_text		; write line of text
loop
		jmp loop			; infinite loop


init_text
		ldx #$00			; load x register with $00
loop_text
		lda message,x		; read characters from message data...
		sta $0590,x			; ...and place it into screen screen memory
		inx					; increment to next character
		cpx #$28			; false if != 40
		bne loop_text		; loop if false
		rts

message
		!scr "              hello world!              "		;40 cols of text	
