[org 0x0100]
jmp start

isPlayerATurn: dw 0
isPlayerBTurn: dw 0
ballLocation: dw 0
ballLocAdd: dw 158
ballMovingUp: dw 1
ballMovingDown: dw 0

; new variables for 4 directions
ballMovingUpRight: dw 1
ballMovingUpLeft: dw 0
ballMovingDownRight: dw 0
ballMovingDownLeft: dw 0


playerAScore: db 0
playerBScore: db 0

printnum:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov ax, [bp+4] ; load number in ax
	mov bx, 10 ; use base 10 for division
	mov cx, 0 ; initialize count of digits
nextdigit:
	mov dx, 0 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30 ; convert digit into ascii value
	push dx ; save ascii value on stack
	inc cx ; increment count of values
	cmp ax, 0 ; is the quotient zero
	jnz nextdigit ; if no divide it again
	mov di, 500 ; point di to top left column
nextpos:
	pop dx ; remove a digit from the stack
	mov dh, 0x07 ; use normal attribute
	mov [es:di], dx ; print char on screen
	add di, 2 ; move to next screen location
	loop nextpos ; repeat for all digits on stack
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2
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
	
	
collisionWithLeftSide:
	push bx
	
	mov ax, [cs:ballLocation];screen location
	mov bx, 160
	mov dx, 0
	div bx
	cmp dx, 0
	je collsionL
	mov ax, 0
	jmp retC1
collsionL:
	mov ax, 1
	
retC1:
	pop bx
	ret
	
; collisionWithRightSide:
	; push bx
	; mov ax, [cs:ballLocation];screen location
	; mov bx, 158
	; mov dx, 0
	
	; div bx
	; cmp dx, 0
	; je collsionR
	; mov ax, 0
	; jmp retC2
; collsionR:
	; mov ax, 1
	
; retC2:
	; pop bx
	; ret
collisionWithRightSide:
    push bx
    push dx
    mov dx, 0
    mov ax, [cs:ballLocation] 
    mov bx, 2 
    div bx
    mov bx, 80
    div bx
    cmp dx, 79
    je collsionR
    
    mov ax, 0
    jmp retC2

collsionR:
    mov ax, 1
    
retC2:
    pop dx
    pop bx
    ret

movePaddleLeft:
	pusha
	
	cmp word [cs:isPlayerATurn], 1	;here move playerA paddle in 0th row Left by 1 cell
	je moveALeft
	cmp word [cs:isPlayerBTurn], 1	;here move playerB paddle in 24th row Left by 1 cell
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
	mov cx, 79
	rep movsw
	mov di, 3998	
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
	mov cx, 79
	rep movsw
	mov di, 158
	mov word [es:di], 0x0720
	jmp endF
	
	
endF
	popa
	ret
	
movePaddleRight:
	pusha

	cmp word [cs:isPlayerATurn], 1	; here move playerB paddle in 24th row Right by 1 cel
	je movARight
	cmp word [cs:isPlayerBTurn], 1 ; here move playerA paddle in 0th row Right by 1 cell
	je movBRight
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
	mov cx, 79
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
	mov cx, 79
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


setNextLocation:
	cmp word [cs:ballMovingDownLeft], 1
	je movDL
	cmp word [cs:ballMovingDownRight], 1
	je movDR
	
	cmp word [cs:ballMovingUpLeft], 1
	je movUL	
	cmp word [cs:ballMovingUpRight], 1
	je movUR
movDL:
	mov word [cs:ballLocAdd], 158
	jmp retSetBallLoc
movDR:
	mov word [cs:ballLocAdd], 162
	jmp retSetBallLoc
movUL:
	mov word [cs:ballLocAdd], 162
	jmp retSetBallLoc
movUR:
	mov word [cs:ballLocAdd], 158
	jmp retSetBallLoc
retSetBallLoc:
	ret
	
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
	mov word [cs:ballLocation], di
	mov word [es:di], 0x072A
	rep stosw
	popa
	ret
	
	
setTurns:
	cmp word [cs:ballMovingUpLeft], 1
	je setBTurn
	cmp word [cs:ballMovingUpRight], 1
	je setBTurn
	cmp word [cs:ballMovingDownLeft], 1
	je setATurn
	cmp word [cs:ballMovingDownRight], 1
	je setATurn
	
	jmp return
setBTurn:
	mov word [cs:isPlayerBTurn], 1
	mov word [cs:isPlayerATurn], 0
	jmp return
	
setATurn:
	mov word [cs:isPlayerATurn], 1
	mov word [cs:isPlayerBTurn], 0	
	jmp return
	
return:
	ret
	
	
checkCollision:
	pusha
	call collisionWithLeftSide
	cmp ax, 1
	je leftColl
	call collisionWithRightSide
	cmp ax, 1
	je rightColl
	jmp retColl
leftColl:
	cmp word [cs:ballMovingDownLeft], 1
	je moveBallDownRight1
	cmp word [cs:ballMovingUpLeft], 1
	je moveBallUpRight1
	
moveBallDownRight1:
	mov word [cs:ballMovingDownRight], 1
	mov word [cs:ballMovingDownLeft], 0
	mov word [cs:ballMovingUpLeft], 0
	mov word [cs:ballMovingUpRight], 0
	call setNextLocation
	jmp retColl
moveBallUpRight1:
	mov word [cs:ballMovingDownRight], 0
	mov word [cs:ballMovingDownLeft], 0
	mov word [cs:ballMovingUpLeft], 0
	mov word [cs:ballMovingUpRight], 1
	call setNextLocation
	jmp retColl
rightColl:
	cmp word [cs:ballMovingDownRight], 1
	je moveBallDownLeft1
	cmp word [cs:ballMovingUpRight], 1
	je moveBallUpLeft1
	
moveBallDownLeft1:
	mov word [cs:ballMovingDownRight], 0
	mov word [cs:ballMovingDownLeft], 1
	mov word [cs:ballMovingUpLeft], 0
	mov word [cs:ballMovingUpRight], 0
	call setNextLocation
	jmp retColl
moveBallUpLeft1:
	mov word [cs:ballMovingDownRight], 0
	mov word [cs:ballMovingDownLeft], 0
	mov word [cs:ballMovingUpLeft], 1
	mov word [cs:ballMovingUpRight], 0
	call setNextLocation
	jmp retColl

retColl:
	popa
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
	call checkCollision
	cmp word [cs:ballMovingUpRight], 1
	je ballWasMovingUpRight
	
	cmp word [cs:ballMovingUpLeft], 1
	je ballWasMovingUpLeft
	
	cmp word [cs:ballMovingDownLeft], 1
	je ballWasMovingDownLeft
	
	cmp word [cs:ballMovingDownRight], 1
	je ballWasMovingDownRight
	
	
ballWasMovingUpRight:
	cmp word [cs:ballLocation], 318
	jbe moveDownRight
	jmp upRight
	
ballWasMovingUpLeft:
	cmp word [cs:ballLocation], 318
	jbe moveDownLeftPortal
	jmp upLeft

ballWasMovingDownRight:
	cmp word [cs:ballLocation], 3680
	jge upRightPortal
	jmp moveDownRight
	
ballWasMovingDownLeft:
	cmp word [cs:ballLocation], 3680
	jge upLeftPortal
	jmp moveDownLeft

upLeftPortal:
	jmp upLeft
	
upRightPortal:
	jmp upRight
	
moveDownLeftPortal:
	jmp moveDownLeft
	
moveDownRight:
	mov word [cs:ballMovingDownRight], 1
	mov word [cs:ballMovingDownLeft], 0
	mov word [cs:ballMovingUpLeft], 0
	mov word [cs:ballMovingUpRight], 0
	
	mov word [cs:isPlayerATurn], 1
	mov word [cs:isPlayerBTurn], 0
	mov di, [cs:ballLocation]
	mov word [es:di], 0x0720
	call setNextLocation
	push ax
	mov ax, [cs:ballLocAdd]
	add word [cs:ballLocation], ax
	pop ax
	call setTurns
	
	mov di, [cs:ballLocation]
	mov word [es:di], 0x072A
	
	jmp EndF1
	
moveDownLeft:
	mov word [cs:ballMovingDownRight], 0
	mov word [cs:ballMovingDownLeft], 1
	mov word [cs:ballMovingUpLeft], 0
	mov word [cs:ballMovingUpRight], 0
	
	mov word [cs:isPlayerATurn], 1
	mov word [cs:isPlayerBTurn], 0
	mov di, [cs:ballLocation]
	mov word [es:di], 0x0720
	call setNextLocation
	push ax
	mov ax, [cs:ballLocAdd]
	add word [cs:ballLocation], ax
	pop ax
	call setTurns
	
	mov di, [cs:ballLocation]
	mov word [es:di], 0x072A
	
	jmp EndF1
	
	
	
upRight:
	
	mov word [cs:ballMovingDownRight], 0
	mov word [cs:ballMovingDownLeft], 0
	mov word [cs:ballMovingUpLeft], 0
	mov word [cs:ballMovingUpRight], 1
	
	mov word [cs:isPlayerBTurn], 1
	mov word [cs:isPlayerATurn], 0
	
	mov di, [cs:ballLocation]
	mov word [es:di], 0x0720
	call setNextLocation
	push ax
	mov ax, [cs:ballLocAdd]
	sub word [cs:ballLocation], ax
	pop ax
	call setTurns
	
	mov di, [cs:ballLocation]
	mov word [es:di], 0x072A
	jmp EndF1
	
upLeft:

	mov word [cs:ballMovingDownRight], 0
	mov word [cs:ballMovingDownLeft], 0
	mov word [cs:ballMovingUpLeft], 1
	mov word [cs:ballMovingUpRight], 0
	
	mov word [cs:isPlayerBTurn], 1
	mov word [cs:isPlayerATurn], 0
	
	mov di, [cs:ballLocation]
	mov word [es:di], 0x0720
	call setNextLocation
	push ax
	mov ax, [cs:ballLocAdd]
	sub word [cs:ballLocation], ax
	pop ax
	call setTurns
	
	mov di, [cs:ballLocation]
	mov word [es:di], 0x072A
	jmp EndF1
	
	
EndF1:
	mov al, 0x20
	out 0x20, al
	popa
	iret
	
	
printScore:
	mov ax, 36; column(x)
	push ax
	mov ax, 12; row (y)
	push ax
	call getScreenLocation
	
    mov di, ax
	
    mov ax, 0xb800
    mov es, ax    
     
    mov ax, 0x072D  ; '-'
    mov [es:di], ax
    add di, 2
	mov ax, 0x072D  ; '-'
    mov [es:di], ax
    add di, 2
	mov ax, 0x072D  ; '-'
    mov [es:di], ax
    add di, 2
	
    mov ax, 0x0753  ; 'S'
    mov [es:di], ax
    add di, 2 
    
    mov ax, 0x0763  ; 'c'
    mov [es:di], ax
    add di, 2
    
    mov ax, 0x076F  ; 'o'
    mov [es:di], ax
    add di, 2
    
    mov ax, 0x0772  ; 'r'
    mov [es:di], ax
    add di, 2
    
    mov ax, 0x0765  ; 'e'
    mov [es:di], ax
    add di, 2
    
    mov ax, 0x072D  ; '-'
    mov [es:di], ax
    add di, 2
	mov ax, 0x072D  ; '-'
    mov [es:di], ax
    add di, 2
	mov ax, 0x072D  ; '-'
    mov [es:di], ax
    add di, 2
	
	mov ax, 34; column(x)
	push ax
	mov ax, 13; row (y)
	push ax
	call getScreenLocation
	mov di, ax
	
	;here print Player A: 
	mov ax, 0x0750  ; 'P'
    mov [es:di], ax
    add di, 2 
    
    mov ax, 0x076C  ; 'l'
    mov [es:di], ax
    add di, 2
    
    mov ax, 0x0761  ; 'a'
    mov [es:di], ax
    add di, 2
    
    mov ax, 0x0779  ; 'y'
    mov [es:di], ax
    add di, 2
    
    mov ax, 0x0765  ; 'e'
    mov [es:di], ax
    add di, 2
    
    mov ax, 0x0772  ; 'r'
    mov [es:di], ax
    add di, 2
    
    
    mov ax, 0x0720  ; ' ' (space to separate "A")
    mov [es:di], ax
    add di, 2
    
    mov ax, 0x0741  ; 'A'
    mov [es:di], ax
    add di, 2 
	 
    mov ax, 0x073A  ; ':'
    mov [es:di], ax
    add di, 2
	mov ax, 0x0720  ; ' ' (space)
    mov [es:di], ax
    add di, 2
	mov byte al, [playerAScore]  ; Player A Score
	add al, 48; convert to asscii
    mov [es:di], ax
    add di, 2 

	mov ax, 34; column(x)
	push ax
	mov ax, 14; row (y)
	push ax
	call getScreenLocation
	mov di, ax
	

	;here print Player B: 
	mov ax, 0x0750  ; 'P'
    mov [es:di], ax
    add di, 2 
    
    mov ax, 0x076C  ; 'l'
    mov [es:di], ax
    add di, 2
    
    mov ax, 0x0761  ; 'a'
    mov [es:di], ax
    add di, 2
    
    mov ax, 0x0779  ; 'y'
    mov [es:di], ax
    add di, 2
    
    mov ax, 0x0765  ; 'e'
    mov [es:di], ax
    add di, 2
    
    mov ax, 0x0772  ; 'r'
    mov [es:di], ax
    add di, 2
    
    
    mov ax, 0x0720  ; ' ' (space to separate "A")
    mov [es:di], ax
    add di, 2
    
    mov ax, 0x0742  ; 'B'
    mov [es:di], ax
    add di, 2 
	 
    mov ax, 0x073A  ; ':'
    mov [es:di], ax
    add di, 2
	mov ax, 0x0720  ; ' ' (space)
    mov [es:di], ax
    add di, 2
	mov byte al, [playerBScore]  ; Player B Score
	add al, 48; convert to asscii
	
    mov [es:di], ax
    add di, 2 
    ret


start:
	call clrScr
	call drawScreen
	xor ax, ax
	mov es, ax
	
	; ;saving old interups
	; mov ax, [es:8*4]
	; mov [cs:oldTimer], ax
	
	; mov ax, [es:8*4 + 2]
	; mov [oldTimer + 2], ax
	
	; mov ax, [es:9*4]
	; mov [oldKeyBoard], ax
	; mov ax, [es:9*4 + 2]
	; mov [oldKeyBoard + 2], ax
	
	
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