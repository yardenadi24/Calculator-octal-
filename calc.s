section	.rodata
	format_string: db "%s", 10, 0
	format_decimal: db "%d", 10, 0
	format_decimalNNL: db "%d", 0
	format_binary: db "%x",10,0
	formal_mycalc: db "%s", 0
	format_link: db "Val: %d, Next: %d", 10, 0								; format string
	exit_format: db "%d", 10, 0 
    msg: db "calc: ", 0
	got_debug: db "got debug mode!", 10, 0
	got_stack_size: db "got stack size of %d (decimal) in octal", 10, 0
    debuggingMethod: dq "Debugging method", 10,0

	print_command_count: db "command counter in octal: %o", 10, 0

	errStack: dq "Error: Operand Stack Overflow", 0
	errInsu: dq "Error: Insufficient Number of Arguments on Stack", 0
	noerror: dq "finish without errors", 10,0

;---------- some usfull printing macros-----;

;push number to stack , dyStackPointer will piont to the number head at the end
%macro pushNumToOpStack 1				
	pushad
	mov edx,[stackSize]					;edx now contains Size*4
	shr edx,2							;devide edx by 4 => edx = size of stack (num of numbers it can contains)
	cmp dword[flowIndicator],edx
		je %%overflow1
		jg %%overflow1
	mov eax,%1							;eax contains the address of a 
	add byte[dyStackPointer],4			;points to the next free address in stack wich be the new stack top	
	mov ebx,[dyStackPointer]			;ebx now have the memory address of the empty top of the stack
	mov dword[ebx],eax					;insert to the top of the stack the head pointer

	jmp %%finish1
	%%overflow1:
	jmp overflow							;----------TO DO OVERFLOW MSG-------------;
	jmp %%end1
	
	%%finish1:
	mov ebx,flowIndicator
	mov ebx,[ebx]
	inc ebx
	mov dword[flowIndicator],ebx
	

	%%end1:
	popad				
%endmacro

;saves top of the stack in NextNode and pop from OPstack
%macro popFromStack 0							
	pushad
	cmp dword[flowIndicator],0
		je %%underFlow1
	jmp %%finish1

	%%underFlow1:
	jmp %%end1

	%%finish1:
	mov edx,[dyStackPointer]					;edx now contains the top head address
	mov edx, [edx]
	mov dword[nextNode],edx		                ;nextNode cotains the top item of the stack
	sub byte[dyStackPointer],4					;now Opstack points to the next top item (simulate pop)
	mov ebx,flowIndicator
	mov ebx,[ebx]
	dec ebx
	mov dword[flowIndicator],ebx

	%%end1:	
	popad
%endmacro

;print a link
%macro printLink 1      
	pushad
	mov dword eax, [%1] ; get the address from pointer to link
	mov edx,0           ; prepare edx to get Val of link
	mov byte dl, [eax]  ; dl no contains the val of the link
	mov ebx, [eax + 1]  ; ebx contains the pointer(address) to the next link
	push ebx            
	push edx
	push format_link	; pointer to str and pointer to format string
	call printf
	add esp, 12			; clean up stack after call
	popad
%endmacro

%macro NullToZero 1
pushad
mov ebx,%1
mov [ebx],0
popad
%endmacro

;----ebx point to the original node----;
;----eax point to the new link each time---;
%macro duplicateFirst 0 
pushad
mov ebx, [flowIndicator]
cmp ebx, 1						; will sent an error if there are less then 2 numbers in the stack
	jb insufficient
incCmdCounter					; increasing the command counter
mov ebx,[dyStackPointer]			;ebx points to top of stack
mov ebx,[ebx]						;ebx contains top stack number	
makeNode							;create New node to copy to
mov dword[eax+1],0					;set next on duplicated to 0x00 as defult		
sub edx,edx
mov byte dl,[ebx]					;dl contains node val
mov [eax],dl						;new node contains val now
mov ecx,[ebx+1]						;ecx contains Next node address (pointer to next node)
mov dword[prevNode],eax				;"prevNode" will hold point to the new node
mov dword[tempD],eax

%%loop1:						
cmp dword ecx,0x00					;checks if next node exists or are we at the end of the num
	je %%end1
mov ebx,[ebx+1]						;ebx points to his next node (original)
makeNode
mov dword[eax+1],0
mov edx,[prevNode]					;edx points to the prevNode
mov dword[edx+1],eax				;set the prev node to point to this node
sub edx,edx
mov byte dl,[ebx]					;dl contains node val	
mov [eax],dl						;new node contains val now	
mov ecx,[ebx+1]						;ecx contains Next node address (pointer to next node)
mov dword[prevNode],eax				;"prevNode" will hold point to the new node
jmp %%loop1

%%end1:
pushNumToOpStack [tempD]
popad
%endmacro

;insert a number to the operant stack
%macro insertNum 1
    pushad
    sub edx,edx                 ;clean edx before use
	sub eax, eax
    mov edx,%1                  ;edx now contains the address of buff[0]

    mov ecx,edx                 ;ecx will be our pointer to the buff[i]

    mov eax,[edx]               ;edx now contains buff[0]
	
	sub edx, edx
	mov byte dl, al
    sub edx,'0'                 ;edx now contains Decimal of buff[0]
    makeNode                    ;eax contains now address to node
    mov [eax],dl           		;add to the first Byte of eax the value of the digit
    mov dword [eax+1],0         ;sets next field of the node to null(0)
    mov dword [nextNode],eax    ;nextNode now points to the start of the node
    inc ecx                     ;now ecx points to buff[i+1]
    
    %%loopNum:
    cmp byte [ecx],0x00
        je %%end1
    sub edx,edx
    movzx edx,byte[ecx]
    sub edx,'0'
    mov ebx,[nextNode]
    makeNode
    mov [eax],dl 
    mov dword [eax+1],ebx 
    mov dword [nextNode],eax
    inc ecx

    jmp %%loopNum
    %%end1:

    ;--finished creating the link now we can push to OPstack--;

    pushNumToOpStack [nextNode]		;push the address of the head to the opstack
    popad
%endmacro 

%macro printCalc 0
	pushad
	push msg 			; call printf with 2 arguments -  
	call printf
	add esp, 4			; clean up stack after call
	popad
%endmacro

%macro getString 0	
	pushad
	push buff
	call gets
	add esp, 4
	popad
%endmacro

%macro pushRegs 0
	Push ECX
	Push EDX
	Push EBX
	Push ESP
	Push EBP
	Push ESI
	Push EDI
%endmacro

%macro popRegs 0
    Pop EDI
    Pop ESI
    Pop EBP
    Pop ESP
	Pop EBX
    Pop EDX
	Pop ECX
%endmacro

%macro makeNode 0		
	pushRegs
	push 5
    call malloc			
    add esp, 4
	popRegs
%endmacro

%macro checkOp 1		
	cmp byte [buff], %1
%endmacro

%macro incCmdCounter 0
add dword [commandCounter], 1
%endmacro

%macro incFlowCounter 0
pushad
mov ebx, [flowIndicator]
inc ebx
mov [flowIndicator], ebx
popad
%endmacro

; -------------------------------------- DOESNT FREE THE WHOLE LIST
%macro freeListFromStack 0		
	pushad
	cmp dword[flowIndicator],0
		je %%end1
	move eax, [dyStackPointer]
	freeList eax
	%%end1:
	popad
%endmacro	

%macro freeList 1
 	pushad
	mov eax,[%1]				;suppose we get list head pointer , eax = address to the head of list
	mov dword[nextNode],eax		;var "next node" now contains the address to the head
	%%loop1:
	mov ebx,[eax+1]				;ebx points to head.Next
	freeLink2 eax
	cmp dword ebx,0x00			;checks if node.next equals null
		je %%end1
	mov dword[nextNode],ebx		;nextNode now contains address of the prev node.next
	mov eax,ebx
	jmp %%loop1
	%%end1:	
	popad
%endmacro

%macro freeLink2 1
pushad
push dword %1
call free
add esp, 4
popad
%endmacro

%macro freeLink 1
pushad
push dword [%1]
call free
add esp, 4
popad
%endmacro

%macro moveToNextLink 2
pushad
mov eax, [%1]
jmp %%start
%%gotNull:
mov byte [eax], 0
mov byte [%2], 1
jmp %%end
%%start:
cmp dword [eax + 1], 0x00
	je %%gotNull
mov ebx, [eax + 1]
freeLink %1
mov dword [%1], ebx
%%end:
popad
%endmacro

; ------ debugging methods that will be deleted ----- ;

;prints entire number from stack node by node
%macro printListFromStack 0		
	pushad
	cmp dword[flowIndicator],0
		je %%end1
	mov eax, [dyStackPointer]
	printList eax
	%%end1:
	popad
%endmacro	

%macro printList2 1
    pushad
	mov eax,[%1]				;suppose we get list head pointer , eax = address to the head of list
	mov dword[nextNode],eax		;var "next node" now contains the address to the head

	%%loop1:
	printLink nextNode			;prints node 
	mov ebx,[eax+1]				;ebx points to head.Next
	cmp dword ebx,0x00			;checks if node.next equals null
		je %%end1
	mov dword[nextNode],ebx		;nextNode now contains address of the prev node.next
	mov eax,ebx
	jmp %%loop1

	%%end1:	
	popad
%endmacro

%macro printList 1
    pushad
	mov eax,[%1]				;suppose we get list head pointer , eax = address to the head of list
	
	mov dword[nextNode],eax		;var "next node" now contains the address to the head

	%%loop1:
	printLink nextNode			;prints node 
	mov ebx,[eax+1]				;ebx points to head.Next
	cmp dword ebx,0x00			;checks if node.next equals null
		je %%end1
	mov dword[nextNode],ebx		;nextNode now contains address of the prev node.next
	mov eax,ebx
	jmp %%loop1

	%%end1:	
	popad
%endmacro

%macro printBuffDecimal 1
	pushad
	mov eax, [%1]
	push eax
	push format_decimal	; pointer to str and pointer to format string
	call printf
	add esp, 8			; clean up stack after call
	popad
%endmacro

%macro printVar 1								;prints the value of inside the Var on Decimal
	pushad
	mov eax,%1
	mov eax,[eax]
	push eax
	push format_decimal
	call printf
	add esp, 8
	popad
%endmacro

;checking with debug:
%macro debugPrints 0
	pushad
	push debuggingMethod	; pointer to str and pointer to format string
	call printf
	add esp, 4			; clean up stack after call
	popad
%endmacro

%macro printNoError 0
	pushad
	push noerror			; pointer to str and pointer to format string
	push format_string
	call printf
	add esp, 8				; clean up stack after call
	popad
%endmacro

;print register as decimal
%macro printReg 1
	pushad
	mov eax,%1
	push eax
	push format_decimal	; pointer to str and pointer to format string
	call printf
	add esp, 8			; clean up stack after call
	popad
%endmacro
%macro printRegB 1
	pushad
	mov eax,%1
	push eax
	push format_binary	; pointer to str and pointer to format string
	call printf
	add esp, 8			; clean up stack after call
	popad
%endmacro

%macro popAndPrint 0	;********CLEAN*******;
pushad
cmp dword [flowIndicator], 0
	je insufficient


popFromStack
mov dword ebx,[nextNode]
mov dword[prevNode],ebx		
cmp dword[ebx+1],0x00			; the list contains only 1 link so no need to reverse
	je %%endOfrev

sub edx,edx
mov byte dl,[ebx]


makeNode
mov [eax],edx
mov dword[eax+1],0x00

mov dword[prevNode],eax			;saves the new RevNode as prev to future pointer
mov ecx,[ebx+1]
mov dword[nextNode],ecx			;node = node.next
mov ebx,[nextNode]				;ebx = node

%%loop1:

sub edx,edx						;set edx to node value
mov byte dl,[ebx]

makeNode
mov [eax],edx					;new node has now the value
mov ecx,[prevNode]
mov [eax+1],ecx					;set pointer reverse
mov dword[prevNode],eax

cmp dword[ebx+1],0x00
	je %%endOfrev

mov ecx,[ebx+1]
mov dword[nextNode],ecx
mov ebx,[nextNode]
jmp %%loop1

%%endOfrev:						;now the number is reversed and we can make print it link by link 
mov ebx,[prevNode]

%%loop2:
cmp dword[ebx+1],0x00
	je %%lastPrint

printLinkVal ebx,format_decimalNNL
mov ebx,[ebx+1]
jmp %%loop2

%%lastPrint:
printLinkVal ebx,format_decimal

popad
%endmacro

%macro printLinkVal 2      
	pushad
	mov dword eax,[%1] 		; get the address from pointer to link
	mov edx, 0           	; prepare edx to get Val of link
	mov byte dl, al  		; dl now contains the val of the link       
	push edx
	push %2 
	call printf
	add esp, 8				; clean up stack after call
	popad
%endmacro

%macro nOCodeFun 0
pushad
mov ebx, [flowIndicator]
cmp ebx, 1						; will sent an error if there are less then 2 numbers in the stack
	jb insufficient
incCmdCounter					; increasing the command counter
popFromStack					; nextNode contains the poped number head
mov dword[numOfDigits],3		; starting count at 1
mov edx,[nextNode]				; edx points to the number head
%%loop1:
cmp dword[edx+1],0x00		;check if there is next node
	je %%end1

mov edx,[edx+1]				;edx points to the next node in chain
add dword[numOfDigits],3	;inc counter
jmp %%loop1


%%end1:
mov edx,[numOfDigits]		;edx contains now num of bits
mov ecx,[numOfDigits]
shr edx,3					;div by 8
mov ebx,edx	
shl edx,3					;mul by 8
cmp dword edx,ecx			;if ecx>edx we need to round up
jge %%finish1
shr edx,3					
inc edx						;if there is carry inc by one result
%%finish1:
decToOctAndPush edx
popad
%endmacro

%macro decToOctAndPush 1
pushad
mov ebx,%1			;stores value to convert
mov eax,7
mov [decToOcRet],ebx
cmp eax,ebx
	jge %%end1
xor eax,eax
mov eax,%1
xor edx,edx
mov ebx,8
div ebx					;now the answer is eax.edx
mov ecx,eax
makeNode
mov byte[eax],cl
mov dword[eax+1],0x00
mov [nextNode],eax
makeNode
mov byte[eax],dl
mov ebx,[nextNode]
mov [eax+1],ebx
mov [nextNode],eax
pushNumToOpStack [nextNode]
jmp %%endFin1
%%end1:
mov ecx,[decToOcRet]
makeNode
mov byte[eax],cl
mov dword[eax+1],0x00
mov [nextNode],eax
pushNumToOpStack [nextNode]
%%endFin1:
popad
%endmacro

;--------------------------end macros----------------;

section .bss		
 	buff:    		        resb 63
	nextNode:		        resb 63
	prevNode:		        resb 63
    constStackPointer:      resd 1
    dyStackPointer:         resd 1
    stackSize:		        resd 1
	debug_mode:				resd 1
    flowIndicator: 	        resd 1
	commandCounter: 		resd 1
	numOfDigits:            resd 1
	nodeToAdd1:		        resb 63
	nodeToAdd2:		        resb 63
	sum:		        	resb 63
	carry:					resb 63
	ind1:					resb 1
	ind2:					resb 1
	ind3:					resb 1
	sumHead:				resb 63
	tempD:					resb 4
	decToOcRet:				resb 1
	val1:					resb 5
	val2:					resb 5

section .text
	align 16
	global main
	extern printf
 	extern fprintf 
  	extern fflush
  	extern malloc 
  	extern calloc 
  	extern free 
  	extern gets 
  	extern getchar 
  	extern fgets 
  	extern stdout
  	extern stdin
  	extern stderr

main:
mov ebp, esp

mov ecx, [esp+4]            	;ecx is now argc
mov ebx, [esp+8]            	;ebx is now argv
mov dword [constStackPointer],0     
mov dword [stackSize],5     	;defult stack size    
mov dword [flowIndicator],0 
mov dword [commandCounter],0 
mov dword [buff], 0

add ebx,4                   	; skipping arg[0]
dec ecx                     	; argv[0] is the name of prog

parsing:						; this loop will run ECX times
	cmp ecx,0                   ;check if there are no args
	jz endParse

	mov edx, [ebx] 				; edx = argv[i]
    mov edx, [edx] 				; edx = argv[i][0]

								; ------ getting the debug mode ------ ;

	and edx, 0x00FFFFFF 		; making the first byte of the argument '\0'
    cmp edx, 0x0000642D 		; comparing to "-d\0\0"
	jz gotDebug

	; ----- got stack size ------ ;
	gotStackSize:
		pushad

		; ----- atoi ----- ;
		xor eax, eax 			; zero a "result so far"
		mov edx, [ebx]			; moving the string buffer to edx
		.top:
		movzx ecx, byte [edx] 	; get a character
		inc edx 				; ready for next one
		cmp ecx, '0' 			
		jb .done
		cmp ecx, '7'
		ja .done
		sub ecx, '0' 			; "convert" character to number
		imul eax, 8 			; multiply "result so far" by eight
		add eax, ecx 			; add in current digit
		jmp .top 				; until done
		.done:
		mov dword [stackSize], eax
		; --- printing message --- ;
		pushad
		push eax
		push got_stack_size
		call printf
		add esp, 8					; clean up stack after call
		popad						; popad because of the printf
		popad						; popad because we pushed at the start of the func
		add ebx,4                   ; now pointing to argv[2]
		dec ecx
		jmp parsing

	; ----- got debug ----- ;
	gotDebug:
		pushad
		mov dword [debug_mode], 1	;	initializing the debug mode flag
		pushad
		push got_debug				; pointer to str and pointer to format string
		call printf
		add esp, 4					; clean up stack after call
		popad
		popad
		ded1:
		add ebx,4                   ; now pointing to argv[2]
		dec ecx
		jmp parsing

endParse:

xor ebx,ebx                         ;cleans ebx
mov eax,[stackSize]
shl eax, 2
mov [stackSize], eax
push eax                            ;push stacksize
call malloc                         ;malloc - allocates stackSize * (byte size)
add esp,4                           ;clean stack
mov dword[constStackPointer],eax    ;saves the address to the HeapStack(op stack) in stack pointer
mov dword[dyStackPointer],eax

loop_start:
    printCalc
    getString               ;var "buff" now points to the input number

checkInput:
	checkOp 'q'					; quit command
		je end
	checkOp '+'					; addition command
		je addition
	checkOp 'p'					; pop-and-print command
		je popPrint
	checkOp 'd'					; duplicate command
		je duplicate
	checkOp '&'					; bitwise AND command
		je bitwiseAndFun
	checkOp 'n'					; number of bytes the number is taking
		je nOCode
	checkOp ''
		je loop_start	

;---------- if we got here it is a number --------;

   	insertNum buff  ;parse the buff as number to linked list
	jmp loop_start

; ---------- errors ---------- ;

insufficient:
	pushad
	push errInsu								; call printf with 2 arguments -  
	push format_string							; pointer to str and pointer to format string
	call printf
	add esp, 8									; clean up stack after call
	popad
	call loop_start

overflow:
	pushad
	push errStack								; call printf with 2 arguments -  
	push format_string							; pointer to str and pointer to format string
	call printf
	add esp, 8									; clean up stack after call
	popad
	call loop_start

end:

	; freeing the stack (free(stacksize))
	; ----- printing the commandCounter ----- ;
	pushad
	mov eax, commandCounter
	push dword [eax]
	push print_command_count
	call printf
	add esp, 8									; clean up stack after call
	popad
	jmp cleanStack

	; ----- cleaning the stack ----- ;

cleanList:
	popFromStack
	freeList nextNode

cleanStack:
	cmp dword[flowIndicator],0
		jg cleanList

call freeLink constStackPointer				; cleans the stack itself

exit:
	; ---------- ending the program ---------- ;
	mov esp, ebp
    mov eax, 0 								
    ret
			
nOCode:
nOCodeFun
jmp loop_start
popPrint:
popAndPrint
jmp loop_start
duplicate:
duplicateFirst
jmp loop_start






addition:
	pushad
	; ----- checks to see if addition is possible ----- ;
	mov ebx, [flowIndicator]
	cmp ebx, 2						; will sent an error if there are less then 2 numbers in the stack
		jb insufficient
	incCmdCounter					; increasing the command counter
	
	; --- initializing variables --- ;

	mov dword [carry], 0			
	mov byte [ind1], 0
	mov byte [ind2], 0
	mov byte [ind3], 0					; initializing eax
	popFromStack					; now the head of the last number will be in nextNode
	mov dword eax, nextNode
	mov eax,[eax]
	mov dword [nodeToAdd1], eax		; the first number to add is in nodeToAdd1
	popFromStack
	mov dword eax, nextNode
	mov eax,[eax]
	mov dword [nodeToAdd2], eax		; the first number to add is in nodeToAdd1
	mov dword [sum], 0x00

	; --- starting the add loop --- ;
	jmp .loop_start

	; if(node1 == null && nude2 == null && carry == 0)
	.checkCarry:
	cmp dword [carry], 0
		je .done
	jmp .add

	.check2:
	cmp byte [ind2], 1
		je .checkCarry
	jmp .add
	
	.gotCarry:
	sub eax, 8
	mov dword [carry], 1
	jmp .backFromCarry

	.loop_start:
	cmp byte [ind1], 1
		je .check2				; list 1 is empty

	.add:
	
	mov eax, [nodeToAdd1]
	mov  eax, [eax]				; the value of node1 in eax
	mov byte[val1], al
	mov ebx, [nodeToAdd2]
	mov  ebx, [ebx]				; the value of node2 in ebx
	mov byte [val2], bl
	mov ecx, [carry]			; carry in ecx
	
	.keep:
	sub eax,eax
	sub ebx,ebx
	mov al,[val1]
	mov bl,[val2]
	add eax, ebx
	add eax, ecx
	mov dword [carry], 0

	cmp eax, 7
		jg .gotCarry
	jmp .backFromCarry

	.gotFirst:
	mov byte [ind3],1
	mov [sumHead], eax
	mov dword[sum],eax
	mov dword[eax+1],0x00
	jmp .resume

	.backFromCarry:
	mov ebx, eax
	makeNode
	cmp byte [ind3], 0
		je .gotFirst

	mov edx,[sum]
	mov dword [edx + 1], eax
	mov dword [eax+1],0x00
	mov dword[sum],eax
	.resume:
	mov byte [eax], bl

	moveToNextLink nodeToAdd1, ind1
	moveToNextLink nodeToAdd2, ind2
	jmp .loop_start

	.done:
	freeLink nodeToAdd1
	freeLink nodeToAdd2

	.endOfAddition:
	pushNumToOpStack [sumHead]
	popad
	jmp loop_start


bitwiseAndFun:
	pushad
	; ----- checks to see if AND is possible ----- ;
	mov ebx, [flowIndicator]
	cmp ebx, 2						; will sent an error if there are less then 2 numbers in the stack
		jb insufficient
	incCmdCounter					; increasing the command counter
	
	; --- initializing variables --- ;
			
	mov byte [ind1], 0
	mov byte [ind2], 0
	mov byte [ind3], 0					; initializing eax
	popFromStack					; now the head of the last number will be in nextNode
	mov dword eax, nextNode
	mov eax,[eax]
	mov dword [nodeToAdd1], eax		; the first number to add is in nodeToAdd1
	popFromStack
	mov dword eax, nextNode
	mov eax,[eax]
	mov dword [nodeToAdd2], eax		; the first number to add is in nodeToAdd1
	mov dword [sum], 0x00

	; --- starting the add loop --- ;
	jmp .loop_start

	.check2:
	cmp byte [ind2], 1
		je .done
	jmp .add
	
	.loop_start:
	cmp byte [ind1], 1
		je .check2				; list 1 is empty

	.add:

	mov eax, [nodeToAdd1]
	mov  eax, [eax]				; the value of node1 in eax
	mov byte[val1], al
	mov ebx, [nodeToAdd2]
	mov  ebx, [ebx]				; the value of node2 in ebx
	mov byte [val2], bl
	

	sub eax,eax
	sub ebx,ebx
	mov al,[val1]
	mov bl,[val2]
	and eax, ebx

	jmp .backFromCarry

	.gotFirst:
	mov byte [ind3],1
	mov [sumHead], eax
	mov dword[sum],eax
	mov dword[eax+1],0x00
	jmp .resume

	.backFromCarry:
	mov ebx, eax
	makeNode
	cmp byte [ind3], 0
		je .gotFirst

	mov edx,[sum]
	mov dword [edx + 1], eax
	mov dword [eax+1],0x00
	mov dword[sum],eax
	.resume:
	mov byte [eax], bl

	moveToNextLink nodeToAdd1, ind1
	moveToNextLink nodeToAdd2, ind2
	jmp .loop_start

	.done:
	freeLink nodeToAdd1
	freeLink nodeToAdd2

	.endOfAddition:
	pushNumToOpStack [sumHead]
	popad
	jmp loop_start