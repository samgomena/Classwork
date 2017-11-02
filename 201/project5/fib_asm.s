.global fibonacci 

fibonacci: 


    cmpq 	$0, %rdi
    je 		return_zero

    cmpq 	$1, %rdi
    je 		return_one
    
    #movq    %rdi, %rax

    decq 	%rdi
    pushq 	%rdi
    call 	fibonacci
    pushq   %rax   

    #subq 	$2, %rdi
    
    decq    %rdi
    pushq 	%rdi
    call 	fibonacci
    
    popq 	%rbx
    addq    %rbx, %rax
    
    jmp 	return


return_zero:
    movq    $0, %rax
    #ret

return_one:
    movq 	$1, %rax
    #ret 

return:
    ret
