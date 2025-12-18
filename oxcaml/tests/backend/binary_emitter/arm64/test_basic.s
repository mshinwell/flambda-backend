	.file	""
	.data
	.globl	_camlTest_basic__data_begin
_camlTest_basic__data_begin:
	.text
	.globl	_camlTest_basic__code_begin
_camlTest_basic__code_begin:
	nop
	.align	3
	.data
	.align	3
	.globl	_camlTest_basic__gc_roots
_camlTest_basic__gc_roots:
	.8byte	0
	.data
	.align	3
	.8byte	13056
	.globl	_camlTest_basic
_camlTest_basic:
	.8byte	_camlTest_basic__add_12
	.8byte	_camlTest_basic__sub_13
	.8byte	_camlTest_basic__mul_14
	.8byte	_camlTest_basic__max_15
	.8byte	_camlTest_basic__min_16
	.8byte	_camlTest_basic__abs_17
	.8byte	_camlTest_basic__sum_to_18
	.8byte	_camlTest_basic__sum_array_19
	.8byte	_camlTest_basic__fadd_20
	.8byte	_camlTest_basic__fsub_21
	.8byte	_camlTest_basic__fmul_22
	.8byte	_camlTest_basic__fdiv_23
	.data
	.align	3
	.8byte	5888
	.globl	_camlTest_basic__Pmakearray390
_camlTest_basic__Pmakearray390:
	.8byte	3
	.8byte	5
	.8byte	7
	.8byte	9
	.8byte	11
	.data
	.align	3
	.8byte	4087
	.globl	_camlTest_basic__fdiv_23
_camlTest_basic__fdiv_23:
	.8byte	_caml_curry2
	.8byte	0x280000000000007
	.8byte	_camlTest_basic__fdiv_11_23_code
	.data
	.align	3
	.8byte	4087
	.globl	_camlTest_basic__fmul_22
_camlTest_basic__fmul_22:
	.8byte	_caml_curry2
	.8byte	0x280000000000007
	.8byte	_camlTest_basic__fmul_10_22_code
	.data
	.align	3
	.8byte	4087
	.globl	_camlTest_basic__fsub_21
_camlTest_basic__fsub_21:
	.8byte	_caml_curry2
	.8byte	0x280000000000007
	.8byte	_camlTest_basic__fsub_9_21_code
	.data
	.align	3
	.8byte	4087
	.globl	_camlTest_basic__fadd_20
_camlTest_basic__fadd_20:
	.8byte	_caml_curry2
	.8byte	0x280000000000007
	.8byte	_camlTest_basic__fadd_8_20_code
	.data
	.align	3
	.8byte	3063
	.globl	_camlTest_basic__sum_array_19
_camlTest_basic__sum_array_19:
	.8byte	_camlTest_basic__sum_array_7_19_code
	.8byte	0x180000000000005
	.data
	.align	3
	.8byte	4087
	.globl	_camlTest_basic__sum_to_18
_camlTest_basic__sum_to_18:
	.8byte	_caml_curry2
	.8byte	0x280000000000007
	.8byte	_camlTest_basic__sum_to_6_18_code
	.data
	.align	3
	.8byte	3063
	.globl	_camlTest_basic__abs_17
_camlTest_basic__abs_17:
	.8byte	_camlTest_basic__abs_5_17_code
	.8byte	0x180000000000005
	.data
	.align	3
	.8byte	4087
	.globl	_camlTest_basic__min_16
_camlTest_basic__min_16:
	.8byte	_caml_curry2
	.8byte	0x280000000000007
	.8byte	_camlTest_basic__min_4_16_code
	.data
	.align	3
	.8byte	4087
	.globl	_camlTest_basic__max_15
_camlTest_basic__max_15:
	.8byte	_caml_curry2
	.8byte	0x280000000000007
	.8byte	_camlTest_basic__max_3_15_code
	.data
	.align	3
	.8byte	4087
	.globl	_camlTest_basic__mul_14
_camlTest_basic__mul_14:
	.8byte	_caml_curry2
	.8byte	0x280000000000007
	.8byte	_camlTest_basic__mul_2_14_code
	.data
	.align	3
	.8byte	4087
	.globl	_camlTest_basic__sub_13
_camlTest_basic__sub_13:
	.8byte	_caml_curry2
	.8byte	0x280000000000007
	.8byte	_camlTest_basic__sub_1_13_code
	.data
	.align	3
	.8byte	4087
	.globl	_camlTest_basic__add_12
_camlTest_basic__add_12:
	.8byte	_caml_curry2
	.8byte	0x280000000000007
	.8byte	_camlTest_basic__add_0_12_code
	.data
	.align	3
	.8byte	2045
	.globl	_camlTest_basic__float152
_camlTest_basic__float152:
	.8byte	0x3ff0000000000000
	.data
	.align	3
	.8byte	2045
	.globl	_camlTest_basic__float154
_camlTest_basic__float154:
	.8byte	0x4000000000000000
	.data
	.align	3
	.8byte	2045
	.globl	_camlTest_basic__float157
_camlTest_basic__float157:
	.8byte	0x4008000000000000
	.data
	.align	3
	.8byte	2045
	.globl	_camlTest_basic__float161
_camlTest_basic__float161:
	.8byte	0x4024000000000000
	.data
	.align	3
	.8byte	2816
	.globl	_camlTest_basic__block88
_camlTest_basic__block88:
	.8byte	_caml_exn_Invalid_argument
	.8byte	_camlTest_basic__string86
	.data
	.align	3
	.8byte	4092
	.globl	_camlTest_basic__string86
_camlTest_basic__string86:
	.ascii	"index out of bounds"
	.space	4
	.byte	4
	.text
	.align	3
	.globl	_camlTest_basic__add_0_12_code
_camlTest_basic__add_0_12_code:
	.cfi_startproc
	add	x0, x0, x1
	sub	x0, x0, #1
	ret
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTest_basic__sub_1_13_code
_camlTest_basic__sub_1_13_code:
	.cfi_startproc
	sub	x0, x0, x1
	add	x0, x0, #1
	ret
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTest_basic__mul_2_14_code
_camlTest_basic__mul_2_14_code:
	.cfi_startproc
	orr	x2, xzr, #1
	sbfm	x1, x1, #1, #63
	sub	x0, x0, #1
	madd	x0, x0, x1, x2
	ret
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTest_basic__max_3_15_code
_camlTest_basic__max_3_15_code:
	.cfi_startproc
	sub	sp, sp, #32
	.cfi_adjust_cfa_offset 32
	.cfi_offset 30, -8
	str	lr, [sp, #24]
	str	x0, [sp, #0]
	str	x1, [sp, #8]
	adrp	x8, _caml_greaterthan@GOTPAGE
	ldr	x8, [x8, _caml_greaterthan@GOTPAGEOFF]
	bl	_caml_c_call
L126:
	orr	x1, xzr, x0
	ldr	x0, [sp, #0]
	ldr	x2, [sp, #8]
	subs	xzr, x1, #1
	b.ne	L122
	orr	x0, xzr, x2
	ldr	lr, [sp, #24]
	add	sp, sp, #32
	.cfi_adjust_cfa_offset -32
	ret
	.cfi_adjust_cfa_offset 32
L122:
	ldr	lr, [sp, #24]
	add	sp, sp, #32
	.cfi_adjust_cfa_offset -32
	ret
	.cfi_adjust_cfa_offset 32
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTest_basic__min_4_16_code
_camlTest_basic__min_4_16_code:
	.cfi_startproc
	sub	sp, sp, #32
	.cfi_adjust_cfa_offset 32
	.cfi_offset 30, -8
	str	lr, [sp, #24]
	str	x0, [sp, #0]
	str	x1, [sp, #8]
	adrp	x8, _caml_lessthan@GOTPAGE
	ldr	x8, [x8, _caml_lessthan@GOTPAGEOFF]
	bl	_caml_c_call
L137:
	orr	x1, xzr, x0
	ldr	x0, [sp, #0]
	ldr	x2, [sp, #8]
	subs	xzr, x1, #1
	b.ne	L133
	orr	x0, xzr, x2
	ldr	lr, [sp, #24]
	add	sp, sp, #32
	.cfi_adjust_cfa_offset -32
	ret
	.cfi_adjust_cfa_offset 32
L133:
	ldr	lr, [sp, #24]
	add	sp, sp, #32
	.cfi_adjust_cfa_offset -32
	ret
	.cfi_adjust_cfa_offset 32
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTest_basic__abs_5_17_code
_camlTest_basic__abs_5_17_code:
	.cfi_startproc
	subs	xzr, x0, #1
	b.ge	L144
	orr	x1, xzr, #2
	sub	x0, x1, x0
	ret
L144:
	ret
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTest_basic__sum_to_6_18_code
_camlTest_basic__sum_to_6_18_code:
	.cfi_startproc
	orr	x2, xzr, x0
	orr	x0, xzr, x1
	subs	xzr, x2, #1
	b.gt	L157
L155:
	ret
L157:
	add	x0, x0, x2
	sub	x0, x0, #1
	sub	x2, x2, #2
	subs	xzr, x2, #1
	b.gt	L157
	b	L155
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTest_basic__sum_array_7_19_code
_camlTest_basic__sum_array_7_19_code:
	.cfi_startproc
	orr	x1, xzr, x0
	ldr	x0, [x1, #-8]
	ubfm	x0, x0, #56, #55
	ubfm	x0, x0, #17, #63
	orr	x2, x0, #1
	sub	x3, x2, #2
	subs	xzr, x3, #1
	b.lt	L201
	orr	x0, xzr, #1
	orr	x4, xzr, #1
	subs	xzr, x4, x2
	b.cs	L197
L182:
	add	x5, x1, x4, lsl #2
	ldr	x5, [x5, #-4]
	add	x0, x0, x5
	sub	x0, x0, #1
	subs	xzr, x4, x3
	b.eq	L191
	add	x4, x4, #2
	subs	xzr, x4, x2
	b.cs	L197
	b	L182
L191:
	ret
L197:
	adrp	x0, _camlTest_basic__block88@GOTPAGE
	ldr	x0, [x0, _camlTest_basic__block88@GOTPAGEOFF]
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L201:
	orr	x0, xzr, #1
	ret
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTest_basic__fadd_8_20_code
_camlTest_basic__fadd_8_20_code:
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset 16
	.cfi_offset 30, -8
	str	lr, [sp, #8]
	orr	x2, xzr, x0
	ldr	x16, [x28, #0]
	sub	x27, x27, #16
	subs	xzr, x27, x16
	b.cc	L213
L212:
	add	x0, x27, #8
	movz	x3, #1277, lsl #0
	str	x3, [x0, #-8]
	ldr	d0, [x1, #0]
	ldr	d1, [x2, #0]
	fadd	d0, d1, d0
	str	d0, [x0, #0]
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L213:
	bl	_caml_call_gc
L211:
	b	L212
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTest_basic__fsub_9_21_code
_camlTest_basic__fsub_9_21_code:
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset 16
	.cfi_offset 30, -8
	str	lr, [sp, #8]
	orr	x2, xzr, x0
	ldr	x16, [x28, #0]
	sub	x27, x27, #16
	subs	xzr, x27, x16
	b.cc	L223
L222:
	add	x0, x27, #8
	movz	x3, #1277, lsl #0
	str	x3, [x0, #-8]
	ldr	d0, [x1, #0]
	ldr	d1, [x2, #0]
	fsub	d0, d1, d0
	str	d0, [x0, #0]
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L223:
	bl	_caml_call_gc
L221:
	b	L222
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTest_basic__fmul_10_22_code
_camlTest_basic__fmul_10_22_code:
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset 16
	.cfi_offset 30, -8
	str	lr, [sp, #8]
	orr	x2, xzr, x0
	ldr	x16, [x28, #0]
	sub	x27, x27, #16
	subs	xzr, x27, x16
	b.cc	L233
L232:
	add	x0, x27, #8
	movz	x3, #1277, lsl #0
	str	x3, [x0, #-8]
	ldr	d0, [x1, #0]
	ldr	d1, [x2, #0]
	fmul	d0, d1, d0
	str	d0, [x0, #0]
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L233:
	bl	_caml_call_gc
L231:
	b	L232
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTest_basic__fdiv_11_23_code
_camlTest_basic__fdiv_11_23_code:
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset 16
	.cfi_offset 30, -8
	str	lr, [sp, #8]
	orr	x2, xzr, x0
	ldr	x16, [x28, #0]
	sub	x27, x27, #16
	subs	xzr, x27, x16
	b.cc	L243
L242:
	add	x0, x27, #8
	movz	x3, #1277, lsl #0
	str	x3, [x0, #-8]
	ldr	d0, [x1, #0]
	ldr	d1, [x2, #0]
	fdiv	d0, d1, d0
	str	d0, [x0, #0]
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L243:
	bl	_caml_call_gc
L241:
	b	L242
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTest_basic__entry
_camlTest_basic__entry:
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset 16
	.cfi_offset 30, -8
	str	lr, [sp, #8]
	ldr	x16, [x28, #40]
	add	x16, x16, #376
	subs	xzr, sp, x16
	b.cc	L273
L274:
	movz	x1, #41, lsl #0
	movz	x0, #21, lsl #0
	bl	_camlTest_basic__max_3_15_code
L275:
	movz	x1, #41, lsl #0
	movz	x0, #21, lsl #0
	bl	_camlTest_basic__min_4_16_code
L276:
	movn	x0, #82, lsl #0
	bl	_camlTest_basic__abs_5_17_code
L277:
	orr	x1, xzr, #1
	movz	x0, #201, lsl #0
	bl	_camlTest_basic__sum_to_6_18_code
L278:
	adrp	x0, _camlTest_basic__Pmakearray390@GOTPAGE
	ldr	x0, [x0, _camlTest_basic__Pmakearray390@GOTPAGEOFF]
	adrp	x8, _caml_obj_dup@GOTPAGE
	ldr	x8, [x8, _caml_obj_dup@GOTPAGEOFF]
	bl	_caml_c_call
L279:
	bl	_camlTest_basic__sum_array_7_19_code
L280:
	adrp	x1, _camlTest_basic__float154@GOTPAGE
	ldr	x1, [x1, _camlTest_basic__float154@GOTPAGEOFF]
	adrp	x0, _camlTest_basic__float152@GOTPAGE
	ldr	x0, [x0, _camlTest_basic__float152@GOTPAGEOFF]
	bl	_camlTest_basic__fadd_8_20_code
L281:
	adrp	x1, _camlTest_basic__float152@GOTPAGE
	ldr	x1, [x1, _camlTest_basic__float152@GOTPAGEOFF]
	adrp	x0, _camlTest_basic__float157@GOTPAGE
	ldr	x0, [x0, _camlTest_basic__float157@GOTPAGEOFF]
	bl	_camlTest_basic__fsub_9_21_code
L282:
	adrp	x1, _camlTest_basic__float157@GOTPAGE
	ldr	x1, [x1, _camlTest_basic__float157@GOTPAGEOFF]
	adrp	x0, _camlTest_basic__float154@GOTPAGE
	ldr	x0, [x0, _camlTest_basic__float154@GOTPAGEOFF]
	bl	_camlTest_basic__fmul_10_22_code
L283:
	adrp	x1, _camlTest_basic__float154@GOTPAGE
	ldr	x1, [x1, _camlTest_basic__float154@GOTPAGEOFF]
	adrp	x0, _camlTest_basic__float161@GOTPAGE
	ldr	x0, [x0, _camlTest_basic__float161@GOTPAGEOFF]
	bl	_camlTest_basic__fdiv_11_23_code
L284:
	orr	x0, xzr, #1
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L273:
	movz	x16, #16
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L274
	.cfi_endproc
	.data
	.align	3
	.text
	.globl	_camlTest_basic__code_end
_camlTest_basic__code_end:
	.data
	.8byte	0
	.globl	_camlTest_basic__data_end
_camlTest_basic__data_end:
	.8byte	0
	.align	3
	.globl	_camlTest_basic__frametable
_camlTest_basic__frametable:
	.8byte	16
	.4byte	(L284 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L285 - .) + 0
	.align	3
	.4byte	(L283 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L286 - .) + 0
	.align	3
	.4byte	(L282 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L287 - .) + 0
	.align	3
	.4byte	(L281 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L288 - .) + 0
	.align	3
	.4byte	(L280 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L289 - .) + 0
	.align	3
	.4byte	(L279 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L290 - .) + 0
	.align	3
	.4byte	(L278 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L291 - .) + 0
	.align	3
	.4byte	(L277 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L292 - .) + 0
	.align	3
	.4byte	(L276 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L293 - .) + 0
	.align	3
	.4byte	(L275 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L294 - .) + 0
	.align	3
	.4byte	(L241 - .) + 0
	.2byte	18
	.2byte	2
	.2byte	3
	.2byte	5
	.byte	1
	.byte	0
	.align	3
	.4byte	(L231 - .) + 0
	.2byte	18
	.2byte	2
	.2byte	3
	.2byte	5
	.byte	1
	.byte	0
	.align	3
	.4byte	(L221 - .) + 0
	.2byte	18
	.2byte	2
	.2byte	3
	.2byte	5
	.byte	1
	.byte	0
	.align	3
	.4byte	(L211 - .) + 0
	.2byte	18
	.2byte	2
	.2byte	3
	.2byte	5
	.byte	1
	.byte	0
	.align	3
	.4byte	(L137 - .) + 0
	.2byte	33
	.2byte	2
	.2byte	0
	.2byte	8
	.align	2
	.4byte	(L295 - .) + 0
	.align	3
	.4byte	(L126 - .) + 0
	.2byte	33
	.2byte	2
	.2byte	0
	.2byte	8
	.align	2
	.4byte	(L296 - .) + 0
	.align	3
	.align	2
L296:
	.4byte	(L298 - .) + 0
	.4byte	4736176
	.align	2
L292:
	.4byte	(L299 - .) + 0
	.4byte	20457624
	.align	2
L289:
	.4byte	(L299 - .) + 0
	.4byte	21506312
	.align	2
L285:
	.4byte	(L299 - .) + 0
	.4byte	23603384
	.align	2
L287:
	.4byte	(L299 - .) + 0
	.4byte	22554800
	.align	2
L288:
	.4byte	(L299 - .) + 0
	.4byte	22030512
	.align	2
L295:
	.4byte	(L300 - .) + 0
	.4byte	5260464
	.align	2
L294:
	.4byte	(L299 - .) + 0
	.4byte	19409048
	.align	2
L286:
	.4byte	(L299 - .) + 0
	.4byte	23079088
	.align	2
L291:
	.4byte	(L299 - .) + 0
	.4byte	20981936
	.align	2
L293:
	.4byte	(L299 - .) + 0
	.4byte	19933336
	.align	2
L290:
	.4byte	(L299 - .) + 0
	.4byte	21516552
L297:
	.ascii	"test_basic.ml\0"
	.align	2
L299:
	.4byte	(L297 - .) + 0
	.ascii	"Test_basic\0"
	.align	2
L300:
	.4byte	(L297 - .) + 0
	.ascii	"Test_basic.min\0"
	.align	2
L298:
	.4byte	(L297 - .) + 0
	.ascii	"Test_basic.max\0"
	.align	3
