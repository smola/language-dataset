m4_include(`commons.m4')

_HEADER_HL1(`Reverse Engineering challenge #9.')
_TAGS(`9')

<p>Now that's easy. What does this code do?</p>

<p>Optimizing GCC 4.8.2:</p>

_PRE_BEGIN
.LC0:
	.string	"error!"
f:
	sub	rsp, 8
	movzx	eax, BYTE PTR [rdi]
	cmp	al, 89
	je	.L3
	jle	.L21
	cmp	al, 110
	je	.L6
	cmp	al, 121
	jne	.L2
.L3:
	mov	eax, 1
	add	rsp, 8
	ret
.L21:
	cmp	al, 78
	je	.L6
.L2:
	mov	edi, OFFSET FLAT:.LC0
	call	puts
	xor	edi, edi
	call	exit
.L6:
	xor	eax, eax
	add	rsp, 8
	ret
_PRE_END

<p>Optimizing GCC 4.9.3 for ARM64:</p>

_PRE_BEGIN
f:
        stp     x29, x30, [sp, -16]!
        add     x29, sp, 0
        ldrb    w0, [x0]
        cmp     w0, 89
        beq     .L6
        bls     .L20
        cmp     w0, 110
        beq     .L5
        cmp     w0, 121
        bne     .L2
.L6:
        mov     w0, 1
        ldp     x29, x30, [sp], 16
        ret
.L20:
        cmp     w0, 78
        beq     .L5
.L2:
        adrp    x0, .LC0
        add     x0, x0, :lo12:.LC0
        bl      puts
        mov     w0, 0
        bl      exit
.L5:
        mov     w0, 0
        ldp     x29, x30, [sp], 16
        ret
.LC0:
        .string "error!"
_PRE_END

<p>(ARM) Optimizing Keil 5.05 (Thumb mode):</p>

_PRE_BEGIN
f PROC
        PUSH     {r4,lr}
        LDRB     r0,[r0,#0]
        CMP      r0,#0x4e
        BEQ      |L0.36|
        CMP      r0,#0x59
        BEQ      |L0.32|
        CMP      r0,#0x6e
        BEQ      |L0.36|
        CMP      r0,#0x79
        BEQ      |L0.32|
        ADR      r0,|L0.40|
        BL       __2printf
        MOVS     r0,#0
        BL       exit
|L0.32|
        MOVS     r0,#1
        POP      {r4,pc}
|L0.36|
        MOVS     r0,#0
        POP      {r4,pc}
        ENDP

|L0.40|
        DCB      "error!\n",0
_PRE_END

<p>Optimizing Keil 5.05 for ARM mode generates nearly the same code, so it's omitted here.</p>

<p>Optimizing GCC 4.4.5 for MIPS:</p>

_PRE_BEGIN
$LC0:
        .ascii  "error!\000"
f:
        lui     $28,%hi(__gnu_local_gp)
        addiu   $sp,$sp,-32
        addiu   $28,$28,%lo(__gnu_local_gp)
        sw      $31,28($sp)
        lb      $2,0($4)
        li      $3,89                   # 0x59
        beq     $2,$3,$L4
        slt     $3,$2,90
        bne     $3,$0,$L9
        li      $3,78                   # 0x4e
        li      $3,110                  # 0x6e
        beq     $2,$3,$L3
        li      $3,121                  # 0x79
        bne     $2,$3,$L2
        nop
$L4:
        lw      $31,28($sp)
        li      $2,1                    # 0x1
        j       $31
        addiu   $sp,$sp,32

$L9:
        beq     $2,$3,$L3
        nop
$L2:
        lw      $25,%call16(puts)($28)
        lui     $4,%hi($LC0)
        jalr    $25
        addiu   $4,$4,%lo($LC0)

        lw      $28,16($sp)
        nop
        lw      $25,%call16(exit)($28)
        nop
        jalr    $25
        move    $4,$0

$L3:
        lw      $31,28($sp)
        move    $2,$0
        j       $31
        addiu   $sp,$sp,32
_PRE_END

_CHALLENGE_FOOTER()

