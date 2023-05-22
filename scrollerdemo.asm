
 processor 6502
	org $801
StartBlock801:
	; Starting new memory block at $801
	.byte $b ; lo byte of next line
	.byte $8 ; hi byte of next line
	.byte $0a, $00 ; line 10 (lo, hi)
	.byte $9e, $20 ; SYS token and a space
	.byte   $32,$30,$36,$34
	.byte $00, $00, $00 ; end of program
	; Ending memory block at $801
EndBlock801:
	org $810
StartBlock810:
	; Starting new memory block at $810
ScrollerDemo
	jmp block1
curBank	dc.b	$00
time	dc.b	$00
row	dc.b	$00
scrollValx	dc.b	$04
scrollValy	dc.b	$04
scrollOffsx	dc.b	$28
scrollOffsy	dc.b	$1e
src	= $02
dst	= $04
map	= $08
nextscrollx	dc.w	$03
nextscrolly	dc.w	$04
scx	dc.w	$03
scy	dc.w	$03
moveleft	dc.b	$00
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init16x8mul
	;    Procedure type : Built-in function
	;    Requires initialization : no
mul16x8_num1Hi = $4C
mul16x8_num1 = $4E
mul16x8_num2 = $50
mul16x8_procedure
	lda #$00
	ldy #$00
	beq mul16x8_enterLoop
mul16x8_doAdd
	clc
	adc mul16x8_num1
	tax
	tya
	adc mul16x8_num1Hi
	tay
	txa
mul16x8_loop
	asl mul16x8_num1
	rol mul16x8_num1Hi
mul16x8_enterLoop
	lsr mul16x8_num2
	bcs mul16x8_doAdd
	bne mul16x8_loop
	rts
end_procedure_init16x8mul
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initeightbitmul
	;    Procedure type : Built-in function
	;    Requires initialization : no
multiplier = $4C
multiplier_a = $4E
multiply_eightbit
	cpx #$00
	beq mul_end
	dex
	stx $4E
	lsr
	sta multiplier
	lda #$00
	ldx #$08
mul_loop
	bcc mul_skip
mul_mod
	adc multiplier_a
mul_skip
	ror
	ror multiplier
	dex
	bne mul_loop
	ldx multiplier
	rts
mul_end
	txa
	rts
initeightbitmul_multiply_eightbit2
	rts
end_procedure_initeightbitmul
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initjoy2
	;    Procedure type : Built-in function
	;    Requires initialization : no
	; ----------
	; ReadJoy1 and ReadJoy2 (on supported platforms)
	; populates joy1 and joy1pressed which can be tested by AND-ing with the following constants:
;JOY_DOWN  = %00000010
;JOY_UP    = %00000001
;JOY_LEFT  = %00000100
;JOY_RIGHT = %00001000
;JOY_FIRE  = %00010000
C64_JOY_CIAPRA = $DC00   ; joy2
C64_JOY_CIAPRB = $DC01   ; joy1
joy2: dc.b 0
joy2last: dc.b 0
joy2pressed: dc.b 0
callReadJoy2
	lda C64_JOY_CIAPRA
	eor #255
	sta joy2
	eor joy2last
	and joy2
	sta joy2pressed
	lda joy2
	sta joy2last
	rts
end_procedure_initjoy2
	
; //const border_color : byte = black;
; // 0,1,2=-3,-2,-1;  3=0;  4,5,6=1,2,3
	; NodeProcedureDecl -1
	; ***********  Defining procedure : movescrollX
	;    Procedure type : User-defined procedure
nx	dc.b	0
movescrollX_block3
movescrollX
	; Binary clause Simplified: GREATEREQUAL
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scrollOffsx
	clc
	adc nx
	 ; end add / sub var with constant
	sec
	; Forcetype: NADA
	sbc #$3
	 ; end add / sub var with constant
	; cmp #$00 ignored
	bcc movescrollX_elsedoneblock7
movescrollX_localsuccess9: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: LESS
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scrollOffsx
	clc
	adc nx
	 ; end add / sub var with constant
	sec
	; Forcetype: NADA
	sbc #$3
	 ; end add / sub var with constant
	; Compare with pure num / var optimization
	cmp #$3b;keep
	bcs movescrollX_elsedoneblock7
movescrollX_ConditionalTrueBlock5: ;Main true block ;keep 
	ldy #0 ; Fake 16 bit
	lda nx
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta nextscrollx
	sty nextscrollx+1
movescrollX_elsedoneblock7
	rts
end_procedure_movescrollX
	; NodeProcedureDecl -1
	; ***********  Defining procedure : movescrollY
	;    Procedure type : User-defined procedure
ny	dc.b	0
movescrollY_block11
movescrollY
	; Binary clause Simplified: GREATEREQUAL
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scrollOffsy
	clc
	adc ny
	 ; end add / sub var with constant
	sec
	; Forcetype: NADA
	sbc #$3
	 ; end add / sub var with constant
	; cmp #$00 ignored
	bcc movescrollY_elsedoneblock15
movescrollY_localsuccess17: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: LESS
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scrollOffsy
	clc
	adc ny
	 ; end add / sub var with constant
	sec
	; Forcetype: NADA
	sbc #$3
	 ; end add / sub var with constant
	; Compare with pure num / var optimization
	cmp #$4b;keep
	bcs movescrollY_elsedoneblock15
movescrollY_ConditionalTrueBlock13: ;Main true block ;keep 
	ldy #0 ; Fake 16 bit
	lda ny
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta nextscrolly
	sty nextscrolly+1
movescrollY_elsedoneblock15
	rts
end_procedure_movescrollY
	; NodeProcedureDecl -1
	; ***********  Defining procedure : fire
	;    Procedure type : User-defined procedure
fire
	; Generic 16 bit op
	ldy #0
	; Forcetype: INTEGER
	lda #$3
fire_rightvarInteger_var22 = $54
	sta fire_rightvarInteger_var22
	sty fire_rightvarInteger_var22+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	ldy nextscrollx+1
	lda nextscrollx
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	sec
	sbc fire_rightvarInteger_var22
fire_wordAdd20
	sta fire_rightvarInteger_var22
	; High-bit binop
	tya
	sbc fire_rightvarInteger_var22+1
	tay
	lda fire_rightvarInteger_var22
	; Calling storevariable on generic assign expression
	sta nextscrollx
	sty nextscrollx+1
	; Generic 16 bit op
	ldy #0
	; Forcetype: INTEGER
	lda #$3
fire_rightvarInteger_var25 = $54
	sta fire_rightvarInteger_var25
	sty fire_rightvarInteger_var25+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	ldy nextscrolly+1
	lda nextscrolly
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	sec
	sbc fire_rightvarInteger_var25
fire_wordAdd23
	sta fire_rightvarInteger_var25
	; High-bit binop
	tya
	sbc fire_rightvarInteger_var25+1
	tay
	lda fire_rightvarInteger_var25
	; Calling storevariable on generic assign expression
	sta nextscrolly
	sty nextscrolly+1
	rts
end_procedure_fire
	; NodeProcedureDecl -1
	; ***********  Defining procedure : HandleJoy
	;    Procedure type : User-defined procedure
HandleJoy
	; Assigning memory location
	; Forcetype: NADA
	lda #$5
	; Calling storevariable on generic assign expression
	sta $d020
	jsr callReadJoy2
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joy2
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq HandleJoy_elsedoneblock30
HandleJoy_ConditionalTrueBlock28: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta ny
	jsr movescrollY
HandleJoy_elsedoneblock30
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joy2
	; Forcetype: NADA
	and #$2
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq HandleJoy_elsedoneblock36
HandleJoy_ConditionalTrueBlock34: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta ny
	jsr movescrollY
HandleJoy_elsedoneblock36
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joy2
	; Forcetype: NADA
	and #$4
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq HandleJoy_elsedoneblock42
HandleJoy_ConditionalTrueBlock40: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta nx
	jsr movescrollX
HandleJoy_elsedoneblock42
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joy2
	; Forcetype: NADA
	and #$8
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq HandleJoy_elsedoneblock48
HandleJoy_ConditionalTrueBlock46: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta nx
	jsr movescrollX
HandleJoy_elsedoneblock48
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joy2
	; Forcetype: NADA
	and #$10
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq HandleJoy_elsedoneblock54
HandleJoy_ConditionalTrueBlock52: ;Main true block ;keep 
	jsr fire
HandleJoy_elsedoneblock54
	; Assigning memory location
	; Forcetype: NADA
	lda #$c
	; Calling storevariable on generic assign expression
	sta $d020
	rts
end_procedure_HandleJoy
	; NodeProcedureDecl -1
	; ***********  Defining procedure : MapPos
	;    Procedure type : User-defined procedure
MapPos
	; Generic 16 bit op
	ldy #0
	ldx #0 ; Fake 24 bit
	lda scrollOffsx
MapPos_rightvarInteger_var60 = $54
	sta MapPos_rightvarInteger_var60
	sty MapPos_rightvarInteger_var60+1
	; Generic 16 bit op
	lda #<mapData
	ldy #>mapData
MapPos_rightvarInteger_var63 = $56
	sta MapPos_rightvarInteger_var63
	sty MapPos_rightvarInteger_var63+1
	; Swapping nodes :  num * expr -> exp*num (mul only)
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$63
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype:  INTEGER
	lda scrollOffsy
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc MapPos_rightvarInteger_var63
MapPos_wordAdd61
	sta MapPos_rightvarInteger_var63
	; High-bit binop
	tya
	adc MapPos_rightvarInteger_var63+1
	tay
	lda MapPos_rightvarInteger_var63
	; Low bit binop:
	clc
	adc MapPos_rightvarInteger_var60
MapPos_wordAdd58
	sta MapPos_rightvarInteger_var60
	; High-bit binop
	tya
	adc MapPos_rightvarInteger_var60+1
	tay
	lda MapPos_rightvarInteger_var60
	sta map
	sty map+1
	rts
end_procedure_MapPos
	
; //  Update banks when scroll = 8
; //  Sets pointers etc
; //
; //
	; NodeProcedureDecl -1
	; ***********  Defining procedure : UpdateBanks
	;    Procedure type : User-defined procedure
UpdateBanks
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda curBank
	clc
	; Forcetype: NADA
	adc #$1
	 ; end add / sub var with constant
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta curBank
	; Binary clause Simplified: EQUALS
	clc
	; cmp #$00 ignored
	bne UpdateBanks_elseblock67
UpdateBanks_ConditionalTrueBlock66: ;Main true block ;keep 
	; Set bank
	; Forcetype: NADA
	lda #$3
	sta $dd00
	lda #$00
	ldx #$44
	sta dst
	stx dst+1
	jmp UpdateBanks_elsedoneblock68
UpdateBanks_elseblock67
	; Set bank
	; Forcetype: NADA
	lda #$2
	sta $dd00
	lda #$00
	ldx #$04
	sta dst
	stx dst+1
UpdateBanks_elsedoneblock68
	rts
end_procedure_UpdateBanks
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DecideNextMove
	;    Procedure type : User-defined procedure
DecideNextMove
	jsr HandleJoy
	ldy nextscrolly+1 ;keep
	lda nextscrolly
	; Calling storevariable on generic assign expression
	sta scy
	sty scy+1
	ldy nextscrollx+1 ;keep
	lda nextscrollx
	; Calling storevariable on generic assign expression
	sta scx
	sty scx+1
	; Generic 16 bit op
	ldy #0
	; Forcetype: NADA
	lda #$3
DecideNextMove_rightvarInteger_var76 = $54
	sta DecideNextMove_rightvarInteger_var76
	sty DecideNextMove_rightvarInteger_var76+1
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda scrollOffsx
	clc
	adc scx
	; Testing for byte:  scx+1
	; RHS is word, no optimization
	pha 
	tya 
	adc scx+1
	tay 
	pla 
	; Low bit binop:
	sec
	sbc DecideNextMove_rightvarInteger_var76
DecideNextMove_wordAdd74
	sta DecideNextMove_rightvarInteger_var76
	; High-bit binop
	tya
	sbc DecideNextMove_rightvarInteger_var76+1
	tay
	lda DecideNextMove_rightvarInteger_var76
	; Calling storevariable on generic assign expression
	sta scrollOffsx
	; Generic 16 bit op
	ldy #0
	; Forcetype: NADA
	lda #$3
DecideNextMove_rightvarInteger_var80 = $54
	sta DecideNextMove_rightvarInteger_var80
	sty DecideNextMove_rightvarInteger_var80+1
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda scrollOffsy
	clc
	adc scy
	; Testing for byte:  scy+1
	; RHS is word, no optimization
	pha 
	tya 
	adc scy+1
	tay 
	pla 
	; Low bit binop:
	sec
	sbc DecideNextMove_rightvarInteger_var80
DecideNextMove_wordAdd78
	sta DecideNextMove_rightvarInteger_var80
	; High-bit binop
	tya
	sbc DecideNextMove_rightvarInteger_var80+1
	tay
	lda DecideNextMove_rightvarInteger_var80
	; Calling storevariable on generic assign expression
	sta scrollOffsy
	jsr MapPos
	sta map
	sty map+1
	ldx map+1
	sta src
	stx src+1
	; Binary clause INTEGER: NOTEQUALS
	lda scx+1   ; compare high bytes
	cmp #$00 ;keep
	beq DecideNextMove_pass188
	jmp DecideNextMove_ConditionalTrueBlock83
DecideNextMove_pass188
	lda scx
	cmp #$03 ;keep
	beq DecideNextMove_localfailed87
	jmp DecideNextMove_ConditionalTrueBlock83
DecideNextMove_localfailed87: ;keep
	; ; logical OR, second chance
	; Binary clause INTEGER: NOTEQUALS
	lda scy+1   ; compare high bytes
	cmp #$00 ;keep
	beq DecideNextMove_pass189
	jmp DecideNextMove_ConditionalTrueBlock83
DecideNextMove_pass189
	lda scy
	cmp #$03 ;keep
	beq DecideNextMove_elsedoneblock85
	jmp DecideNextMove_ConditionalTrueBlock83
DecideNextMove_ConditionalTrueBlock83: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$8
	; Calling storevariable on generic assign expression
	sta moveleft
	
; // how much move left from the scroll
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta row
DecideNextMove_elsedoneblock85
	; Binary clause INTEGER: EQUALS
	lda scx+1   ; compare high bytes
	cmp scy+1 ;keep
	bne DecideNextMove_elsedoneblock94
	lda scx
	cmp scy ;keep
	bne DecideNextMove_elsedoneblock94
	jmp DecideNextMove_localsuccess96
DecideNextMove_localsuccess96: ;keep
	; ; logical AND, second requirement
	; Binary clause INTEGER: NOTEQUALS
	lda scx+1   ; compare high bytes
	cmp #$00 ;keep
	beq DecideNextMove_pass197
	jmp DecideNextMove_ConditionalTrueBlock92
DecideNextMove_pass197
	lda scx
	cmp #$03 ;keep
	beq DecideNextMove_elsedoneblock94
	jmp DecideNextMove_ConditionalTrueBlock92
DecideNextMove_ConditionalTrueBlock92: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta scrollValx
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta scrollValy
DecideNextMove_elsedoneblock94
	; Binary clause INTEGER: NOTEQUALS
	lda scx+1   ; compare high bytes
	cmp scy+1 ;keep
	beq DecideNextMove_pass1105
	jmp DecideNextMove_localsuccess104
DecideNextMove_pass1105
	lda scx
	cmp scy ;keep
	beq DecideNextMove_elsedoneblock102
	jmp DecideNextMove_localsuccess104
DecideNextMove_localsuccess104: ;keep
	; ; logical AND, second requirement
	; Binary clause INTEGER: NOTEQUALS
	lda scx+1   ; compare high bytes
	cmp #$00 ;keep
	beq DecideNextMove_pass1107
	jmp DecideNextMove_localsuccess106
DecideNextMove_pass1107
	lda scx
	cmp #$03 ;keep
	beq DecideNextMove_elsedoneblock102
	jmp DecideNextMove_localsuccess106
DecideNextMove_localsuccess106: ;keep
	; ; logical AND, second requirement
	; Binary clause INTEGER: NOTEQUALS
	lda scy+1   ; compare high bytes
	cmp #$00 ;keep
	beq DecideNextMove_pass1108
	jmp DecideNextMove_ConditionalTrueBlock100
DecideNextMove_pass1108
	lda scy
	cmp #$03 ;keep
	beq DecideNextMove_elsedoneblock102
	jmp DecideNextMove_ConditionalTrueBlock100
DecideNextMove_ConditionalTrueBlock100: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta scrollValx
	; Forcetype: NADA
	lda #$5
	; Calling storevariable on generic assign expression
	sta scrollValy
DecideNextMove_elsedoneblock102
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$3
	; Calling storevariable on generic assign expression
	sta nextscrollx
	sty nextscrollx+1
	; Forcetype: INTEGER
	; Calling storevariable on generic assign expression
	sta nextscrolly
	sty nextscrolly+1
	rts
end_procedure_DecideNextMove
	
; //	Copy 40 bytes every frame
; //
	; NodeProcedureDecl -1
	; ***********  Defining procedure : CopyPart
	;    Procedure type : User-defined procedure
CopyPart
	; Assigning memory location
	; Forcetype: NADA
	lda #$c
	; Calling storevariable on generic assign expression
	sta $d020
	; Binary clause INTEGER: GREATEREQUAL
	lda src+1   ; compare high bytes
	cmp #$a6 ;keep
	bcc CopyPart_localfailed116
	bne CopyPart_ConditionalTrueBlock112
	lda src
	cmp #$49 ;keep
	bcc CopyPart_localfailed116
	jmp CopyPart_ConditionalTrueBlock112
CopyPart_localfailed116: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda row
	; Compare with pure num / var optimization
	cmp #$19;keep
	bne CopyPart_elsedoneblock114
CopyPart_ConditionalTrueBlock112: ;Main true block ;keep 
	rts
CopyPart_elsedoneblock114
	; memcpy unrolled
	ldy #0
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	iny
	lda (src),y
	sta (dst),y
	lda dst
	clc
	adc #$28
	sta dst+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc CopyPart_WordAdd118
	inc dst+1
CopyPart_WordAdd118
	lda src
	clc
	adc #$63
	sta src+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc CopyPart_WordAdd119
	inc src+1
CopyPart_WordAdd119
	; Test Inc dec D
	inc row
	rts
end_procedure_CopyPart
	; NodeProcedureDecl -1
	; ***********  Defining procedure : UpdateScroll
	;    Procedure type : User-defined procedure
UpdateScroll
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda moveleft
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc UpdateScroll_elsedoneblock124
UpdateScroll_ConditionalTrueBlock122: ;Main true block ;keep 
	; Test Inc dec D
	dec moveleft
UpdateScroll_elsedoneblock124
	; HandleVarBinopB16bit
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy scy+1 ;keep
	lda scy
	sec
	sbc #$03
	; Testing for byte:  #$00
	; RHS is word, no optimization
	pha 
	tya 
	sbc #$00
	tay 
	pla 
UpdateScroll_rightvarInteger_var129 = $54
	sta UpdateScroll_rightvarInteger_var129
	sty UpdateScroll_rightvarInteger_var129+1
	lda scrollValy+1
	sec
	sbc UpdateScroll_rightvarInteger_var129+1
	tay
	lda scrollValy
	sec
	sbc UpdateScroll_rightvarInteger_var129
	bcs UpdateScroll_wordAdd127
	dey
UpdateScroll_wordAdd127
	; Calling storevariable on generic assign expression
	sta scrollValy
	; HandleVarBinopB16bit
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy scx+1 ;keep
	lda scx
	sec
	sbc #$03
	; Testing for byte:  #$00
	; RHS is word, no optimization
	pha 
	tya 
	sbc #$00
	tay 
	pla 
UpdateScroll_rightvarInteger_var132 = $54
	sta UpdateScroll_rightvarInteger_var132
	sty UpdateScroll_rightvarInteger_var132+1
	lda scrollValx+1
	sec
	sbc UpdateScroll_rightvarInteger_var132+1
	tay
	lda scrollValx
	sec
	sbc UpdateScroll_rightvarInteger_var132
	bcs UpdateScroll_wordAdd130
	dey
UpdateScroll_wordAdd130
	; Calling storevariable on generic assign expression
	sta scrollValx
	; Binary clause Simplified: EQUALS
	lda scrollValy
	; Compare with pure num / var optimization
	cmp #$9;keep
	bne UpdateScroll_localfailed143
	jmp UpdateScroll_ConditionalTrueBlock134
UpdateScroll_localfailed143: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	clc
	lda scrollValy
	; cmp #$00 ignored
	bne UpdateScroll_localfailed142
	jmp UpdateScroll_ConditionalTrueBlock134
UpdateScroll_localfailed142: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda scrollValx
	; Compare with pure num / var optimization
	cmp #$9;keep
	bne UpdateScroll_localfailed144
	jmp UpdateScroll_ConditionalTrueBlock134
UpdateScroll_localfailed144: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	clc
	lda scrollValx
	; cmp #$00 ignored
	bne UpdateScroll_elsedoneblock136
UpdateScroll_ConditionalTrueBlock134: ;Main true block ;keep 
	jsr UpdateBanks
	; 8 bit binop
	; Add/sub where right value is constant number
	; Modulo
	; Forcetype: NADA
	lda #$8
UpdateScroll_val_var146 = $54
	sta UpdateScroll_val_var146
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scrollValx
	clc
	; Forcetype: NADA
	adc #$7
	 ; end add / sub var with constant
	sec
UpdateScroll_modulo147
	sbc UpdateScroll_val_var146
	bcs UpdateScroll_modulo147
	adc UpdateScroll_val_var146
	clc
	; Forcetype: NADA
	adc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta scrollValx
	; 8 bit binop
	; Add/sub where right value is constant number
	; Modulo
	; Forcetype: NADA
	lda #$8
UpdateScroll_val_var148 = $54
	sta UpdateScroll_val_var148
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scrollValy
	clc
	; Forcetype: NADA
	adc #$7
	 ; end add / sub var with constant
	sec
UpdateScroll_modulo149
	sbc UpdateScroll_val_var148
	bcs UpdateScroll_modulo149
	adc UpdateScroll_val_var148
	clc
	; Forcetype: NADA
	adc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta scrollValy
UpdateScroll_elsedoneblock136
	; Binary clause Simplified: EQUALS
	clc
	lda moveleft
	; cmp #$00 ignored
	bne UpdateScroll_elsedoneblock153
UpdateScroll_ConditionalTrueBlock151: ;Main true block ;keep 
	jsr DecideNextMove
UpdateScroll_elsedoneblock153
	
; // Set scroll value
	; ScrollY method 
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scrollValy
	sec
	; Forcetype: NADA
	sbc #$1
	 ; end add / sub var with constant
	sta $58
	lda $d011  
	and #$78
	ora $58
	and #$7F
	sta $d011
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scrollValx
	sec
	; Forcetype: NADA
	sbc #$1
	 ; end add / sub var with constant
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	rts
end_procedure_UpdateScroll
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Raster
	;    Procedure type : User-defined procedure
Raster
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	lda $D011
	and #%11110111
	sta $D011
	lda $D016
	and #%11110111
	sta $D016
	jsr UpdateScroll
	jsr CopyPart
	jsr CopyPart
	jsr CopyPart
	; Assigning memory location
	; Forcetype: NADA
	lda #$c
	; Calling storevariable on generic assign expression
	sta $d020
	; Test Inc dec D
	inc time
	; RasterIRQ : Hook a procedure
	; Forcetype: NADA
	lda #$f6
	sta $d012
	lda #<RasterBottom
	sta $fffe
	lda #>RasterBottom
	sta $ffff
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	rti
end_procedure_Raster
	; NodeProcedureDecl -1
	; ***********  Defining procedure : RasterBottom
	;    Procedure type : User-defined procedure
RasterBottom
	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	jsr CopyPart
	jsr CopyPart
	jsr CopyPart
	jsr CopyPart
	
; //CopyPart();
	; Assigning memory location
	; Forcetype: NADA
	lda #$c
	; Calling storevariable on generic assign expression
	sta $d020
	; RasterIRQ : Hook a procedure
	; Forcetype: NADA
	lda #$0
	sta $d012
	lda #<Raster
	sta $fffe
	lda #>Raster
	sta $ffff
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	rti
end_procedure_RasterBottom
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Init
	;    Procedure type : User-defined procedure
Init
	; Clear screen with offset
	; Forcetype: NADA
	lda #$20
	ldx #$fa
Init_clearloop159
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne Init_clearloop159
	; Clear screen with offset
	; Forcetype: NADA
	lda #$20
	ldx #$fa
Init_clearloop160
	dex
	sta $0000+$4400,x
	sta $00fa+$4400,x
	sta $01f4+$4400,x
	sta $02ee+$4400,x
	bne Init_clearloop160
	; Clear screen with offset
	; Forcetype: NADA
	lda #$1
	ldx #$fa
Init_clearloop161
	dex
	sta $0000+$d800,x
	sta $00fa+$d800,x
	sta $01f4+$d800,x
	sta $02ee+$d800,x
	bne Init_clearloop161
	lda $d018
	and #%11110001
	ora #14
	sta $d018
	; Assigning memory location
	; Forcetype: NADA
	lda #$c
	; Calling storevariable on generic assign expression
	sta $d020
	; Assigning memory location
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta $d021
	rts
end_procedure_Init
block1
main_block_begin_
	jsr Init
	jsr UpdateBanks
	jsr DecideNextMove
	
; //CopyFullScreen(src, dst);
	; Disable interrupts
	ldy #$7f    ; $7f = %01111111
	sty $dc0d   ; Turn off CIAs Timer interrupts
	sty $dd0d   ; Turn off CIAs Timer interrupts
	lda $dd0d   ; cancel all CIA-IRQs in queue/unprocessed
	sei
	; Set Memory Config
	lda $01
	and #%11111000
	ora #%101
	sta $01
	sei
	; Disable interrupts
	sty $dc0d   ; Turn off CIAs Timer interrupts
	sty $dd0d   ; Turn off CIAs Timer interrupts
	; RasterIRQ : Hook a procedure
	; Forcetype: NADA
	lda #$0
	sta $d012
	lda #<RasterBottom
	sta $fffe
	lda #>RasterBottom
	sta $ffff
	; Enable raster IRQ
	lda $d01a
	ora #$01
	sta $d01a
	lda #$1B
	sta $d011
	asl $d019
	cli
	jmp * ; loop like (�/%
main_block_end_
	; End of program
	; Ending memory block at $810
EndBlock810:
	org $3800
StartBlock3800:
	org $3800
charset1:
	incbin	 "C:/Dev/c64/myProject///resources/charmap.bin"
end_incbin_charset1:
EndBlock3800:
	org $7800
StartBlock7800:
	org $7800
charset2:
	incbin	 "C:/Dev/c64/myProject///resources/charmap.bin"
end_incbin_charset2:
EndBlock7800:
	org $8000
StartBlock8000:
	org $8000
mapData:
	incbin	 "C:/Dev/c64/myProject///resources/charimage99x99.bin"
end_incbin_mapData:
EndBlock8000:

