[org 0x0100]
jmp start

isPlayerATurn:dw 0
isPlayerBTurn:dw 0
ballLocation:dw 0
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
	mul byte [bp + 4] ; multiply with y position
	add ax, [bp + 6] ; add x position
	shl ax, 1 ; turn into byte offset
	
	pop es
	pop bx
	pop bp
	ret 4
	
movePaddleLeft:
	pusha
	cmp word [isPlayerATurn], 1
	;here move playerA paddle in 0th row Left by 1 cell
	je moveALeft
	cmp word [isPlayerBTurn], 1
	;here move playerB paddle in 24th row Left by 1 cell
	je moveBLeft

moveALeft:
	mov ax, 0xb800
	mov es, ax
	mov di, 3840
	cmp word [es:di], 0x7020
	jne moveLeft1
	jmp endF

moveLeft1:
	push es
	pop ds
	mov si, 3842
	mov cx, 78
	rep movsw
	mov word [es:di], 0x0720

	jmp endF

	
moveBLeft:
	mov ax, 0xb800
	mov es, ax
	mov di, 0
	cmp word [es:di], 0x7020
	jne moveLeft2
	jmp endF

moveLeft2:
	push es
	pop ds
	mov si, 3842
	mov cx, 78
	rep movsw
	mov word [es:di], 0x0720
	jmp endF
	
	
endF
	popa
	ret
	
movePaddleRight:
	pusha
	cmp word [isPlayerATurn], 1
	; here move playerA paddle in 0th row Right by 1 cell
	
	cmp word [isPlayerBTurn], 1
	; here move playerB paddle in 24th row Right by 1 cell
	
	popa
	ret
	
	; keyboard interrupt service routine
kbisr:
	push ax
	push es
	mov ax, 0xb800
	mov es, ax ; point es to video memory
	in al, 0x60 ; read a char from keyboard port
	cmp al, 0x4B ; is the key left Arrow
	jne nextcmp ; no, try next comparison
	; here move paddle left
	call movePaddleLeft
	jmp nomatch ; leave interrupt routine
nextcmp:
	cmp al, 0x4D ; is the key right Arrow
	jne nomatch ; no, leave interrupt routine
	;here move paddle right
nomatch:
	mov al, 0x20
	out 0x20, al ; send EOI to PIC
	pop es
	pop ax
	iret 

	
drawScreen:
	pusha
		
;drawing PlayerA Paddle
	mov ax, 30 ; x position
	mov bx, 0 ;y position
	push ax
	push bx
	call getScreenLocation
	mov di, ax
	mov bx, 0xb800
	mov es, bx
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
	; mov bx, 0xb800
	; mov es, bx
	mov cx, 20
	mov ax, 0x7020
	rep stosw
	
showBall:
	;drawing PlayerB Paddle
	mov ax, 40 ; x position
	mov bx, 23 ;y position
	push ax
	push bx
	call getScreenLocation
	mov di, ax
	mov word [ballLocation], di
	mov word [es:di], 0x072A
	rep stosw
	popa
	ret
setTurns:
	cmp word [ballLocation], 1920
	jbe setATurn
	mov word [isPlayerBTurn], 1
	mov word [isPlayerATurn], 0
	jmp return
setATurn:
	mov word [isPlayerATurn], 1
	mov word [isPlayerBTurn], 0
	
return:
	ret
gameRunner:
	pusha
	mov ax, 0xb800
	mov es, ax
	;here moving ball
	
	;checks for moving ball
	;1- initially one row and one column up right direction
	;2- after hitting player A row move ball back oppositely(right down diagnally)
	
	;initially ball location will be 3760
	;when it will hit first row [160-218] move it down diagnally Right
	cmp word [ballLocation], 218
	jbe moveDownRight
	jmp upRight
moveDownRight:
	mov di, [ballLocation]
	mov word [es:di], 0x0720
	add word [ballLocation], 158
	call setTurns
	
	mov di, [ballLocation]
	mov word [es:di], 0x072A
	
	jmp EndF1
upRight:
	mov di, [ballLocation]
	mov word [es:di], 0x0720
	sub word [ballLocation], 158
	call setTurns
	
	mov di, [ballLocation]
	mov word [es:di], 0x072A
	
EndF1:
	mov al, 0x20
	out 0x20, al
	popa
	iret
start:
	call clrScr
	call drawScreen
	xor ax, ax
	mov es, ax
	cli
	mov word [es:8*4], gameRunner
	mov [es:8*4+2], cs
	mov word [es:9*4], kbisr ; store offset at n*4
	mov [es:9*4+2], cs ; store segment at n*4+2
	sti
	
label1:
	jmp label1
end:
	mov ax, 0x4c00
	int 0x21