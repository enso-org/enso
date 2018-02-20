	.text
	.file	"main.c"
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:
	push	rbp
.Ltmp0:
	.cfi_def_cfa_offset 16
.Ltmp1:
	.cfi_offset rbp, -16
	mov	rbp, rsp
.Ltmp2:
	.cfi_def_cfa_register rbp
	sub	rsp, 64
	movabs	rax, 4
	mov	dword ptr [rbp - 4], 0
	mov	dword ptr [rbp - 8], edi
	mov	qword ptr [rbp - 16], rsi
	mov	rdi, rax
	call	malloc
	mov	qword ptr [rbp - 24], rax
	mov	dword ptr [rbp - 28], 0
	mov	rax, qword ptr [rbp - 16]
	mov	rdi, qword ptr [rax + 8]
	call	atoi
	mov	dword ptr [rbp - 32], eax
	mov	rsi, qword ptr [rbp - 24]
	mov	dword ptr [rsi], 0
	mov	dword ptr [rbp - 36], 0
.LBB0_1:                                # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_3 Depth 2
                                        #       Child Loop BB0_5 Depth 3
	mov	eax, dword ptr [rbp - 36]
	cmp	eax, dword ptr [rbp - 32]
	jge	.LBB0_12
# BB#2:                                 #   in Loop: Header=BB0_1 Depth=1
	mov	dword ptr [rbp - 40], 0
.LBB0_3:                                #   Parent Loop BB0_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_5 Depth 3
	mov	eax, dword ptr [rbp - 40]
	cmp	eax, dword ptr [rbp - 32]
	jge	.LBB0_10
# BB#4:                                 #   in Loop: Header=BB0_3 Depth=2
	mov	dword ptr [rbp - 44], 0
.LBB0_5:                                #   Parent Loop BB0_1 Depth=1
                                        #     Parent Loop BB0_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	mov	eax, dword ptr [rbp - 44]
	cmp	eax, dword ptr [rbp - 32]
	jge	.LBB0_8
# BB#6:                                 #   in Loop: Header=BB0_5 Depth=3
	mov	rax, qword ptr [rbp - 24]
	mov	ecx, dword ptr [rax]
	add	ecx, 1
	mov	dword ptr [rax], ecx
# BB#7:                                 #   in Loop: Header=BB0_5 Depth=3
	mov	eax, dword ptr [rbp - 44]
	add	eax, 1
	mov	dword ptr [rbp - 44], eax
	jmp	.LBB0_5
.LBB0_8:                                #   in Loop: Header=BB0_3 Depth=2
	jmp	.LBB0_9
.LBB0_9:                                #   in Loop: Header=BB0_3 Depth=2
	mov	eax, dword ptr [rbp - 40]
	add	eax, 1
	mov	dword ptr [rbp - 40], eax
	jmp	.LBB0_3
.LBB0_10:                               #   in Loop: Header=BB0_1 Depth=1
	jmp	.LBB0_11
.LBB0_11:                               #   in Loop: Header=BB0_1 Depth=1
	mov	eax, dword ptr [rbp - 36]
	add	eax, 1
	mov	dword ptr [rbp - 36], eax
	jmp	.LBB0_1
.LBB0_12:
	movabs	rdi, .L.str
	mov	rax, qword ptr [rbp - 24]
	mov	esi, dword ptr [rax]
	mov	al, 0
	call	printf
	movabs	rdi, .L.str
	mov	esi, dword ptr [rbp - 32]
	mov	dword ptr [rbp - 48], eax # 4-byte Spill
	mov	al, 0
	call	printf
	mov	esi, 0
	mov	dword ptr [rbp - 52], eax # 4-byte Spill
	mov	eax, esi
	add	rsp, 64
	pop	rbp
	ret
.Ltmp3:
	.size	main, .Ltmp3-main
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%d\n"
	.size	.L.str, 4


	.ident	"clang version 3.5.0 (tags/RELEASE_350/final)"
	.section	".note.GNU-stack","",@progbits
