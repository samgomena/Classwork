	.globl	Main_main
Main_main:
	pushl	%ebp
	movl	%esp,%ebp
	subl	$20,%esp
	movl	$0,%eax
	movl	%eax,-4(%ebp)
	movl	$1,%eax
	movl	%eax,-8(%ebp)
	movl	$1,%eax
	movl	%eax,-12(%ebp)
	movl	$10,%eax
	movl	%eax,-16(%ebp)
	jmp	l1
l0:
	movl	-4(%ebp),%eax
	pushl	%eax
	call	print
	addl	$4,%esp
	movl	-4(%ebp),%eax
	movl	%eax,-20(%ebp)
	movl	-8(%ebp),%eax
	movl	%eax,-4(%ebp)
	movl	-8(%ebp),%eax
	movl	-20(%ebp),%ebx
	addl	%ebx,%eax
	movl	%eax,-8(%ebp)
	movl	$1,%eax
	movl	-12(%ebp),%ebx
	addl	%ebx,%eax
	movl	%eax,-12(%ebp)
l1:
	movl	$10,%eax
	movl	-12(%ebp),%ebx
	cmpl	%eax,%ebx
	jle	l0
	movl	%ebp,%esp
	popl	%ebp
	ret
