[org 0x0100]
jmp start
clrScr:
; subroutine to clear the screen
clrscr: push es
push ax
push cx
push di
mov ax, 0xb800
mov es, ax ; point es to video base
xor di, di ; point di to top left column
mov ax, 0x0720 ; space char in normal attribute
mov cx, 2000 ; number of screen locations
cld ; auto increment mode
rep stosw ; clear the whole screen
pop di
pop cx
pop ax
pop es
ret
; helper subroutines
;to push all registers to store
pushAll:
	push ax
	push bx
	push cx
	push dx
	push di
	push si
	push es
	push ds
	ret
; to pop all registers to store back
popAll:
	pop ds
	pop es
	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	ret
	
; sub routine to get the screen location
;location = ( hypos(y) * 80 + epos(x) ) * 2
; this subRoutine returns final answer in ax
; coming stack : ret yPos xPost
getScreenLocation:
	push bp
	mov bp, sp
	push bx
	push es
	
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov ax, 0
	mov al, 80 ; load al with columns per row
	mov bx, [bp + 4]
	mull bl ; multiply with y position
	add ax, [bp + 6] ; add x position
	shl ax, 1 ; turn into byte offset
	
	pop es
	pop bx
	pop bp
	ret 4
	
drawScreen:
	call pushAll	
		
;drawing PlayerA Paddle
	mov ax, 30 ; x position
	mov bx, 0 ;y position
	push ax
	push bx
	call getScreenLocation
	mov di, ax
	mov bx, 0xb800
	mov es, ax
	mov cx, 20
	mov ax, 0x7020
	rep stosw
	
	;drawing PlayerB Paddle
	mov ax, 30 ; x position
	mov bx, 24 ;y position
	push ax
	push bx
	call getScreenLocation
	mov di, ax
	mov bx, 0xb800
	mov es, ax
	mov cx, 20
	mov ax, 0x7020
	rep stosw
	
	call popAll	
	ret

start:
	call clrScr
	call drawScreen
	
	
end:
	mov ax, 0x4c00
	int 0x21