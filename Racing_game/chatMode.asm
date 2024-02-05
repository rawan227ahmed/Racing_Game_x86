PUBLIC chatting
;EXTRN Player1Name: 19 DUP(BYTE)
;EXTRN Player2Name: 19 DUP(BYTE)
.model small
.data
Value db ?
sx db ?
sy db ?
rx db ?
ry db ?
player1 db "player1",'$'
player2 db "player2",'$'
printspace db 800 Dup(' '),'$'
endchatting1 db "- To end chatting with ",'$'
endchatting2 db " Press F3",'$'
.stack 64
.code
backspacesender proc far
push cx
push ax
mov dl,sx
cmp dl,0d
je nobackspacesender
dec dl
mov sx,dl
mov dh,sy
mov ah,2
int 10h
mov dl,' '
int 21h
nobackspacesender:
pop ax
pop cx 
ret
backspacesender endp
backspacereciever proc far
push cx
push ax
mov dl,rx
cmp dl,0d
je nobackspacereciever
dec dl
mov rx,dl
mov dh,ry
mov ah,2
int 10h
mov dl,' '
int 21h 
nobackspacereciever:
pop ax
pop cx 
ret
backspacereciever endp
scrollsender proc far
push cx
push ax
mov dl,0
mov dh,1d
mov ah,2
int 10h
mov ah,9h
mov dx,offset printspace
int 21h
mov dl,0
mov sx,dl
mov dh,1d
mov sy,dh
mov ah,2
int 10h
pop ax
pop cx
ret
scrollsender endp
scrollreciever proc far
push cx
push ax
mov dl,0
mov dh,13d
mov ah,2
int 10h
mov ah,9h
mov dx,offset printspace
int 21h
mov dl,0
mov rx,dl
mov dh,13d
mov ry,dh
mov ah,2
int 10h
pop ax
pop cx
ret
scrollreciever endp
sender Proc FAR
mov ah,01h
int 16h
jz skip222
mov ah,0h
int 16h
mov Value,al
jmp bridgeforskip2
skip222:
jmp skip2
bridgeforskip2:
;check for f3
cmp ax,3d00h;f3
jne skip1
mov cx,0h
jmp senddata
skip1:
; check for enter
cmp ax,1c0dh;enter
JNE skip3
mov dh,sy
cmp dh,10d
jne noscrollsender
call scrollsender
jmp senddata
noscrollsender:
mov ah,0
mov sx,ah
mov ah,sy
inc ah
mov sy,ah
jmp senddata
skip3:
;check for backspace
cmp ax,0e08h
jne nobackspaces 
call backspacesender
jmp senddata
nobackspaces:
; check for printable character between 32d,127d
cmp al,32d
JB skip2
cmp al,127d
JA skip2
mov dl,al
;mov Value,al
mov ah,2h
int 21h
;change cursor
mov ah,3h
mov bh,0h
int 10h
cmp dh,11d
jne noscrollsenderw
call scrollsender
jmp senddata
noscrollsenderw:
mov sx,dl
mov sy,dh
;;;;;
senddata:
mov dx , 3FDH		; Line Status Register
AGAIN:  In al , dx 			;Read Line Status
AND al , 00100000b
JZ AGAIN
;If empty put the VALUE in Transmit data register
mov dx , 3F8H		; Transmit data register
mov  al,Value
out dx , al 
skip2:
ret
sender endp
;___________________________________________________________________________________________
reciever Proc FAR
;Check that Data Ready
mov dx , 3FDH		; Line Status Register
in al , dx 
AND al , 1
JZ skip5
;If Ready read the VALUE in Receive data register
mov dx , 03F8H
in al , dx 
mov VALUE , al
;print
;check f3
cmp al,00h
jne checkenter
mov cx,0d
jmp skip5
checkenter:
;check enter
cmp al,13d
JNE skip4
mov dh,ry
cmp dh,22d
jne noscrollreciever
call scrollreciever
jmp skip5
noscrollreciever:
mov ah,0d
mov rx,ah
mov ah,ry
add ah,1d
mov ry,ah
jmp skip5
skip4:
;check for backspace
cmp al,08h
jne nobackspacer
call backspacereciever
jmp skip5
nobackspacer:
mov dl,Value
mov ah,2h
int 21h
;change cursor
mov ah,3h
mov bh,0h
int 10h
cmp dh,23d
jne noscrollrecieverw
call scrollreciever
jmp skip5
noscrollrecieverw:
mov rx,dl
mov ry,dh
skip5:
ret
reciever endp
;_____________________________________________________________________________________________
chatting PROC FAR
;darw chat mode screen
mov ax,0003h;to clearscreen
int 10h
mov ah,0
mov al,03h
int 10h
;darw line ; dh=y
mov ah,2
mov dh,11d
mov dl,0d
int 10h
drawhalve:
mov ah,2
mov cl,dl
mov dl,'_'
int 21h
mov ah,2
mov dl,cl
inc dl
int 10h
cmp dl,80d
JNE drawhalve
;draw status bar
mov ah,2
mov dh,23d
mov dl,0d
int 10h
drawstatus:
mov ah,2
mov cl,dl
mov dl,'_'
int 21h
mov ah,2
mov dl,cl
inc dl
int 10h
cmp dl,80d
JNE drawstatus
; prnt status for me
mov ah,2
mov dh,24d
mov dl,0d
int 10h
mov ah,9h
mov dx,offset endchatting1
int 21h
mov dx,offset player2
int 21h
mov dx,offset endchatting2
int 21h
; print names for me
mov ah,2
mov dh,0d
mov dl,1d
int 10h
mov ah,9h
mov dx,offset player1
int 21h
mov ah,2
mov dl," "
int 21h
mov ah,2
mov dl,':'
int 21h
;-----------------
mov ah,2
mov dh,12d
mov dl,1d
int 10h
mov ah,9h
mov dx,offset player2
int 21h
mov ah,2
mov dl," "
int 21h
mov ah,2
mov dl,':'
int 21h
; intial cursor
mov ah,0
mov sx,ah
mov rx,ah
mov ah,1d
mov sy,ah
mov ah,13d
mov ry,ah
chat:
;send
mov ah,2
mov dh,sy
mov dl,sx
int 10h
call sender
mov ah,2
mov dh,ry
mov dl,rx
int 10h
call reciever
cmp cx,0
JNE chat
;exit program
mov ax,0003h;to clearscreen
int 10h
;mov ah,4ch
;int 21h
ret
chatting ENDP
END chatting