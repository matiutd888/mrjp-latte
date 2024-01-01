.data
    .customlabel:
        .long f
		.long g

.text
f:
	push %ebp
	mov %esp, %ebp
	sub $0, %esp
	push $1
	call printInt
	add $4, %esp
	push $0
	pop %eax
	jmp label$freturn
label$freturn:
	mov %ebp, %esp
	pop %ebp
	ret


g:
	push %ebp
	mov %esp, %ebp
	sub $0, %esp
	push $2
	call printInt
	add $4, %esp
	push $0
	pop %eax
	jmp label$freturn
label$greturn:
	mov %ebp, %esp
	pop %ebp
	ret



.global main
main:
	push %ebp
	mov %esp, %ebp
	mov $0, %edi
	; mov .customlabel, %esi
    lea .customlabel, %esi
    push %esi
    mov (%esp), %esi


    call *(%esi,%edi,4)
    push $0
	pop %eax
	jmp label$mainreturn
label$mainreturn:
	mov %ebp, %esp
	pop %ebp
	ret


