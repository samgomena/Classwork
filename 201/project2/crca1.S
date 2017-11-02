	.global crcInit
	.global crcFast

	.data
table:
	.rept 256 
	.byte 0
	.endr

	.text

crcInit:
	pushq	%rbx
	pushq	%rbp
	pushq	%rdi		 	
	pushq	%rsi
	pushq	%rsp		# Got these from https://msdn.microsoft.com/en-us/library/6t169e9c.aspx
	pushq	%r12		# Not sure if spec was to save all of them regardless but did it anyways
	pushq	%r13
	pushq	%r14
	pushq	%r15
	
	xor	%rbx, %rbx	# dividend = 0
	mov	$0, %rdx	# Probably don't need this 
	mov	$table, %rax	# Put $table into register cuz not comfortable with indirect addressing

lewp:
	# -------------------------------------------
	#add	(%rax), %rdx	# Read from array
	#add	%rdx, (%rax)	# Write to array
	# -------------------------------------------
	
	mov	%rbx, %rcx	# remainder = dividend
	mov	$8, %r12	# Set bit to 8

lewp2:	
	# Control flow
	cmp	$0, %r12	# bit > 0
	jz	end
	
	# Logic	
	mov	%rcx, %r13	
	and	$0x80, %r13	# remainder & 128
	jz	isfalse
	
istrue:
	shl	$0x01, %cl	# remainder << 1
	xor	$0xD5, %cl	# remainder ^ POLYNOMIAL
	dec	%r12		# --bit
	jmp	lewp2

	
isfalse:
	shl	$0x01, %rcx	# remainder <<= 1
	# Change this to be conditional move statment
	dec	%r12		# --bit
	cmp	$0, %r12	# bit > 0
	jg	lewp2

end:
	movsbl	%cl, %edx	# Format char to be added to table
	mov	%edx, %edx	# Zero upper bits of register
	mov	%edx, (%rax)	# Add value to table
	inc	%rbx
	add	$4, %rax	# increment pointer to look at next item in $table
	cmp	$256, %rbx	# dividend < 256
	jl	lewp
	
	popq	%r15
	popq	%r14
	popq	%r13		 	
	popq	%r12
	popq	%rsp		# pop these bad boys
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%rbx

crcFast:		

	pushq	%rbx		# Push these so can use them
	pushq	%r12
	pushq	%r13
	
	movq	%rdi, %r12	# rdi = message
	movq	%rsi, %r13 	# rsi = strlen(message)

	movq	$0, %r9		# remainder
	movq	$0, %r10	# byte counter
	jmp	endl2

lewis:
	movq	(%r12), %rbx
	
	xorb	%r9b, %bl	# message[byte] ^ remainder
	movq	%rcx, %rcx
	movzbw	%bl, %cx
	movq	%r9, %r9 	# zero extend register
	mov	table(,%rcx, 4), %r9
	incq	%r10	
	incq	%r12

endl2:
	cmpq	%r13, %r10
	jl	lewis
	xorq	%rax, %rax	
	movw	%r9w, %ax	# put remainder in return register
	popq	%r13		# pop callee
	popq	%r12
	popq	%rbx
	ret	

