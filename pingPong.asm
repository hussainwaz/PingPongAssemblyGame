[orf 0x0100]
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




start:
	call clrScr
	
	
end:
	mov ax, 0x4c00
	int 0x21