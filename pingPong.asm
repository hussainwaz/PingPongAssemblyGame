[org 0x0100]
jmp start

isPlayerATurn:dw 0
isPlayerBTurn:dw 0
ballLocation:dw 0
ballMovingUp: dw 1
ballMovingDown: dw 0

oldTimer:dd 0
oldKeyBoard:dd 0

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
	jmp endF
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
	mov si, 2
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
	; here move playerB paddle in 24th row Right by 1 cel
	jmp movARight
	cmp word [isPlayerBTurn], 1
	
	; here move playerA paddle in 0th row Right by 1 cell
	jmp movBRight
	jmp endFr
	
movBRight:
	mov ax, 0xb800
	mov es, ax
	mov di, 158
	cmp word [es:di], 0x7020
	jne movRight2
	jmp endFr

movRight2:
	push es
	pop ds
	mov si, 156
	mov cx, 78
	std
	rep movsw
	
	mov word [es:di], 0x0720
	jmp endFr
	
movARight:
	mov ax, 0xb800
	mov es, ax
	mov di, 3998
	cmp word [es:di], 0x7020
	jne moveRight3
	jmp endFr

moveRight3:
	push es
	pop ds
	mov si, 3996
	mov cx, 78
	std
	rep movsw
	
	mov word [es:di], 0x0720
	jmp endFr
	
		
endFr:

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
	call movePaddleRight
nomatch:
	mov al, 0x20
	out 0x20, al ; send EOI to PIC
	pop es
	pop ax
	iret 

	
drawScreen:
	pusha
	push ds
	push cs
	pop ds
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
	pop ds
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
	push ds
	push cs
	pop ds
	
	mov ax, 0xb800
	mov es, ax
	;here moving ball
	
	;checks for moving ball
	;1- initially one row and one column up right direction
	;2- after hitting player A row move ball back oppositely(right down diagnally)
	
	;initially ball location will be 3760
	;when it will hit first row [160-218] move it down diagnally Right
	cmp word [ballMovingUp], 1
	je ballWasMovingUp
	jmp ballWasMovingDown
	
ballWasMovingUp:
	cmp word [ballLocation], 218
	jbe moveDownRight
	jmp upRight

ballWasMovingDown:
	cmp word [ballLocation], 3680
	jbe moveDownRight
	jmp upRight


moveDownRight:
	mov word [ballMovingDown], 1
	mov word [ballMovingUp], 0
	mov di, [ballLocation]
	mov word [es:di], 0x0720
	add word [ballLocation], 162
	call setTurns
	
	mov di, [ballLocation]
	mov word [es:di], 0x072A
	
	jmp EndF1
	
upRight:
	mov word [ballMovingDown], 0
	mov word [ballMovingUp], 1
	mov di, [ballLocation]
	mov word [es:di], 0x0720
	sub word [ballLocation], 158
	call setTurns
	
	mov di, [ballLocation]
	mov word [es:di], 0x072A
	
EndF1:
	mov al, 0x20
	out 0x20, al
	pop ds
	popa
	iret
start:
	call clrScr
	call drawScreen
	xor ax, ax
	mov es, ax
	
	;saving old interups
	mov ax, [es:8*4]
	mov [oldTimer], ax
	
	mov ax, [es:8*4 + 2]
	mov [oldTimer + 2], ax
	
	mov ax, [es:9*4]
	mov [oldKeyBoard], ax
	mov ax, [es:9*4 + 2]
	mov [oldKeyBoard + 2], ax
	
	
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