PUBLIC DRAW
;_________________________________________________________________________________________________________________________________
printTimer MACRO
    ;set cursor position
    mov ah,2h
    mov dh,0d
    mov dl,26d
    int 10h
    ;-------------print minute
    mov al,RemainMin
    add al,30h ; minute
    mov ah,0eh
    mov bx,000Ah
    int 10h
    mov ah,2h;move cursor
    mov dh,0d
    add dl,1d
    int 10h
    ;--------------print :
    mov al,':'
    mov ah,0eh
    mov bx,0004h
    int 10h
    mov ah,2h;move cursor
    mov dh,0d
    add dl,1d
    int 10h
    ;-------------print second
    mov al,RemainSec
    mov ah,0d
    mov bh,10d
    div bh
    mov cl,ah ;save reminder in cl
    add al,30h ;first digit
    mov ah,0eh
    mov bx,000Ah
    int 10h
    mov ah,2h;move cursor
    mov dh,0d
    add dl,1d
    int 10h
    mov al,cl;second digit
    add al,30h
    mov ah,0eh
    mov bx,000Ah
    int 10h
ENDM printTimer
;_____________________________________________________________________________________________
;                     Score code
;____________________________________________________________________________________________
;----------------------------------------------
calcindex MACRO row,column,index
mov ax,row
mov bx,320d
MUL bx
mov dx,column
add ax,dx
mov index,ax
ENDM calcindex
;---------------------------calc tracklong -----------------------
CalcTrackLong MACRO
 mov ax,tracklength
 mov bx,tracknumber
 MUL bx
 add ax,1d
 mov tracklong,ax
ENDM CalcTrackLong
;----------------------------------------------- search for point in path in case it is not found al will have the track size value
searchpoint MACRO score ; 0 up, 1 right ,2 down
LOCAL notfound
LOCAL vertical
LOCAL loop1
LOCAL found1
LOCAL next1
LOCAL calcleft
LOCAL skip13
LOCAL found
LOCAL loop2
LOCAL found2
LOCAL calcl
LOCAL calcup
LOCAL notfound1
LOCAL upchangeindex
LOCAL skipupchange
LEA si,points
LEA di,kind
mov ax,0d
mov currentpath,ax
mov score,ax
; loop on the path untill you found the index
notfound:
;-------------------------------- read points of rectangle and its type
mov ah,[di]
mov bx,[si]
add si,2d
mov dx,[si]
add si,2d
cmp ah,1d
jne vertical
;---------------------------------- the rectangle is horizontal
mov cx,index ; move index to cx car index
mov ax,bx 
add ax,trackwidth*320 ;move to ax stop pixel 
add dx,1d
;------------------------------------ loop on the pixels of rectangle
loop1:
cmp cx,bx
jc next1 ; not in this rectangle so move to the next
cmp cx,dx
jc found1 ;found so stop looping 
add bx,320d
add dx,320d
cmp bx,ax
jne loop1
;--------------------- end of loop
;------------------- cala length
next1:
mov ax,score
add ax,tracklength
mov score,ax
jmp far ptr skip13 ; go to next rectangle
found1:
mov ax,score
sub cx,bx
add cx,CarSize
add ax,cx
mov score,ax
jmp far ptr found
;------------------------------------- vertical rectangle check
vertical:
mov cx,index
mov al,[di]
cmp al,0d
je upchangeindex
sub bx,trackwidth-1
sub dx,trackwidth-1
jmp skipupchange
upchangeindex:
mov ax,bx
mov bx,dx
mov dx,ax
sub bx,320d
skipupchange:
mov ax,bx
add ax,trackwidth
;add dx,320d
;---------------- loop on the pixels of rectangle
loop2:
cmp cx,bx
jc calcl ; calc length and go to next
cmp cx,ax
jc found2
add bx,320d
add ax,320d
cmp bx,dx
jne loop2
;-------------------------------------- end of loop
;------------------------------------- end of vertical rectangle check
;--------------------------- birdge and calc length
calcl:
mov ax,score
add ax,tracklength
mov score,ax
jmp skip13
found2:
mov ah,[di]
cmp ah,0d ;the direction is up
je calcup
sub dx,bx
mov ax,dx
mov dx,0d
mov bx,320d
div bx
mov cx,tracklength
sub cx,ax
add cx,CarSize
mov ax,score
add ax,cx
mov score,ax
jmp far ptr found
calcup:
sub dx,bx
mov ax,dx
mov dx,0d
mov bx,320d
div bx
add ax,1d
mov cx,score
add cx,ax
mov score,cx
jmp  found
notfound1:
jmp far ptr notfound
skip13:
;-------------------------- end of birdge
add di,1d
mov bx,tracknumber
mov ax,currentpath
add ax,1d
mov currentpath,ax
cmp ax,bx
jne notfound1
found:
ENDM searchpoint
;-----------------------------------------------------------------
calcscore MACRO pscore ;must be called after search point directly
LOCAL nochange
LOCAL fullpercent
mov bx,tracknumber
mov ax,currentpath
cmp ax,bx
je nochange
mov cx,tracklong
mov ax,score
cmp ax,cx
jnc fullpercent
mov bx,100d
MUL bx ; the result stored in dx ax
mov bx,tracklong
div bx ; store the result in ax
mov pscore,al
jmp nochange
fullpercent:
mov al,100d
mov pscore,al
nochange:
ENDM calcscore
;-----------------------------------------------------
printscorestatus MACRO score1,score2
    mov ah,0eh
    mov bx,0000h
    mov al,' '
    int 10h
    mov ah,2h;set cursor position
    mov dh,0d
    mov dl,31d
    int 10h
    ; score of player 1
    printscore score1
    mov ah,2h;move cursor
    mov dh,0d
    add dl,1d
    int 10h
    mov al,'/'
    mov ah,0eh
    mov bx,0004h
    int 10h
    mov ah,2h;move cursor
    mov dh,0d
    add dl,1d
    int 10h
    ; score of player 2
    printscore score2
ENDM printscorestatus
;-----------------------------------------------------this is for graphical mode
printscore macro score
    mov al,score
    mov ah,0d
    mov bh,100d
    div bh
    mov cl,ah; save reminder in cl
    add al,30h ; first digit
    mov ah,0eh
    mov bx,000Ah
    int 10h
    mov ah,2h;move cursor
    mov dh,0d
    add dl,1d
    int 10h
    mov ah,0d
    mov al,cl
    mov bh,10d
    div bh
    mov cl,ah ; save remider in cl 
    add al,30h;second digit
    mov ah,0eh
    mov bx,000Ah
    int 10h
    mov ah,2h;move cursor
    mov dh,0d
    add dl,1d
    int 10h
    mov al,cl
    add al,30h ;third digit
    mov ah,0eh
    mov bx,000Ah
    int 10h
    mov ah,2h;move cursor
    mov dh,0d
    add dl,1d
    int 10h
    mov al,'%'
    mov ah,0eh
    mov bx,000Ah
    int 10h
endm printscore
;------------------------------this is for text mode-----------------------
printscoretext macro score
    mov al,score
    mov ah,0d
    mov bh,100d
    div bh
    mov cl,ah; save reminder in cl
    add al,30h ; first digit
    mov ah,2h
    mov dl,al
    int 21h
    mov ah,0d
    mov al,cl
    mov bh,10d
    div bh
    mov cl,ah ; save remider in cl 
    add al,30h;second digit
    mov ah,2h
    mov dl,al
    int 21h
    mov al,cl
    add al,30h ;third digit
    mov ah,2h
    mov dl,al
    int 21h
    mov dl,'%'
    mov ah,2h
    int 21h
endm printscoretext
;_____________________________score show and calc___________________________
scorebar MACRO 
calcindex Car1Y,Car1X,index
searchpoint score
calcscore score1
calcindex Car2Y,Car2X,index
searchpoint score
calcscore score2
printscorestatus score1,score2
endm scorebar
;___________________________________________________________________________________________________________________________________
CheckIfWithinTrack MACRO X, Y, OldX, OldY
    LOCAL InTrack
    LOCAL NotInTrack
    LOCAL CC1
    LOCAL CC2
    LOCAL finish

    MOV AX, 0A000H
    MOV ES, AX

    ;;left-upper corner
    MOV AX, Y
    MOV BX, ScreenWidth
    IMUL BX
    ADD AX, X
    MOV DI, AX
    MOV BX, ScreenWidth - CarSize
    MOV DX, CarSize

    CC1:            
                        MOV        CX,CarSize
    CC2:            
                        CMP        BYTE PTR ES:[DI], Pink
                        JE         NotInTrack
                        CMP        BYTE PTR ES:[DI],Black
                        JE         NotInTrack

                        ADD DI,1
                        LOOP       CC2

                        ADD        DI, BX
                        DEC        DX
                        JNZ        CC1
       JMP InTrack

    NotInTrack:
    MOV AX, OldX
    MOV X, AX
    MOV AX, OldY
    MOV Y, AX
    
    InTrack:
ENDM
;____________________________________________________________________________________________________________________________________
CheckCollision MACRO X, Y, OldX, OldY, otherCar ;;if two cars collide, each goes to its previous position and its speed is decreased
    LOCAL NoCollision
    LOCAL Collide
    LOCAL CC1
    LOCAL CC2
    LOCAL doNothing1
    
    MOV AX, Y
    MOV BX, ScreenWidth
    IMUL BX
    ADD AX, X
    MOV DI, AX
    MOV BX, ScreenWidth - CarSize
    MOV DX, CarSize

    CC1:            
                        MOV        CX,CarSize
    CC2:            
                        CMP        BYTE PTR ES:[DI], otherCar
                        JE         Collide
                        ADD        DI,1
                        LOOP       CC2

                        ADD        DI, BX
                        DEC        DX
                        JNZ        CC1
       JMP NoCollision

    Collide:
    MOV AX, OldX
    MOV X, AX
    MOV AX, OldY
    MOV Y, AX
    
    ;;Decrease car2 speed
    mov Car1PowerUp,orange 
    ActivatePowerUp Car1Speed,Car2Speed,Car1PowerUp,CancelObstale1,EndActivatedPowerUp1,powerUpActivated1,Car1X,Car1Y

    ;;Drcrease car1 speed
    mov Car2PowerUp,orange 
    ActivatePowerUp Car2Speed,Car1Speed,Car2PowerUp,CancelObstale2,EndActivatedPowerUp2,powerUpActivated2,Car2X,Car2Y
  
    NoCollision:
 ENDM
;_____________________________________________________________________________________________________________________________
CheckObsCollision MACRO X, Y, OldX, OldY,thisCar,CancelObstale
    LOCAL NoCollision2
    LOCAL Collide2
    LOCAL CC12
    LOCAL CC22
    LOCAL doNothing
    LOCAL Other 
    LOCAL this

    MOV AX, Y
    MOV BX, ScreenWidth
    IMUL BX
    ADD AX, X
    MOV DI, AX
    MOV BX, ScreenWidth - CarSize
    MOV DX, CarSize

    CC12:            
                        MOV        CX,CarSize
    CC22:            
                        CMP        BYTE PTR ES:[DI],ObstacleColor
                        JE         Collide2
                        ADD        DI,1
                        LOOP       CC22

                        ADD        DI, BX
                        DEC        DX
                        JNZ        CC12
       JMP NoCollision2

    Collide2:
    CMP CancelObstale,1
    JNE doNothing
    JMP NoCollision2
    

    doNothing:
    MOV AX, OldX
    MOV X, AX
    MOV AX, OldY
    MOV Y, AX

    CMP thisCar,1
    JE this  
    JMP FAR PTR Other 

    this:
    mov Car1PowerUp,ObstacleColor
    ActivatePowerUp Car1Speed,Car2Speed,Car1PowerUp,CancelObstale1,EndActivatedPowerUp1,powerUpActivated1,Car1X,Car1y
    jmp NoCollision2

     Other:
    mov Car2PowerUp,ObstacleColor
    ActivatePowerUp Car2Speed,Car1Speed,Car2PowerUp,CancelObstale2,EndActivatedPowerUp2,powerUpActivated2,Car2X,Car2Y

    NoCollision2:
 ENDM
;____________________________________________________________________________________________________________________________________
DrawPowerUp MACRO  Xc ,Yc, colr
                    MOV           DX,Yc
                    SUB           DX,1             ;starting row
                    MOV           AL,colr
                    MOV           AH,0ch             
   drawrows:
                    MOV           CX,Xc            
                    SUB           CX,1             ;starting column
                    MOV           BX,Xc            ;ending column
                    ADD           BX,1
   drawrow:
                    INT           10h         
                    INC           CX         
                    CMP           CX,BX
                    JNE           drawrow
                    MOV           BX,Yc            ;ending column
                    ADD           BX,1         
                    INC           DX
                    CMP           DX,BX
                    JNE           drawrows
                    EN:
ENDM DrawPowerUp
;____________________________________________________________________________________________________________________________________
CollectPowerUp MACRO  powerup, X, Y ;; assume that if a car passed through 2 powerups, it only collects one
                    LOCAL CheckOnLeftX  ;; assume that if a car passed through a powerup and it already has one, it will be collected and the old one will be discarded
                    LOCAL CheckOnRightX
                    LOCAL CheckOnUppery
                    LOCAL CheckOnLowery
                    LOCAL CheckNext
                    LOCAL Collect
                    LOCAL noPowerUp
                                        

        MOV CX, PowerUpNum           ;;loop on powerups array
        MOV SI, 0
        MOV DI, 0
        MOV AX, X
        MOV DX, CarSize
        MOV BX, Y

        CheckOnLeftX:
        CMP PoWerUpx[DI], AX
        JAE CheckOnRightX
        JMP CheckNext
        ;; 
        CheckOnRightX:
        ADD DX, AX 
        CMP PoWerUpx[DI],DX
        MOV DX,CarSize
        JBE CheckOnUpperY
        JMP CheckNext
        ;;
        CheckOnUppery:
        CMP PowerUpY[DI],BX
        JAE CheckOnLowerY
        JMP CheckNext
        ;;
        CheckOnLowery:
        ADD DX, BX
        CMP PowerUpY[DI],DX
        MOV DX,CarSize
        JBE Collect        ;;now we're sure that the car has collected the one in the array[SI,DI] with PowerUpColour[DI]
        JMP CheckNext      ;;Dummy jump
        ;;
        ;;powerup[SI][DI] doesn't exist in this (x,y) position
        ;;check on the next powerup
        CheckNext:
        ADD SI,1
        ADD DI,2
        LOOP CheckOnLeftX
        JMP noPowerUp

        Collect:
        MOV AL,PoWerUpcolor[SI]
        MOV powerup, AL
        ;;removing the powerup from the list == its x and y are in an invalid location
        MOV WORD PTR PoWerUpx[DI], 0
        MOV WORD PTR PowerUpY[DI], 0

        noPowerUp:
        ENDM
;___________________________________________________________________________________________________________________________________
ActivatePowerUp MACRO  thisSpeed,otherSpeed,PoWerUp,CancelObstale,EndActivatedPowerUp,powerUpActivated,X,Y
                LOCAL N1
                LOCAL N2
                LOCAL N3
                LOCAL N4
                LOCAL N9
        CMP PoWerUp,Black
        JE N9

        mov        ah, 2ch
        int        21h
        ADD        DH,5
        MOV        AL,DH
        MOV        AH,0
        MOV        BL,60d
        DIV        BL
        MOV        EndActivatedPowerUp,AH
        MOV        CL,PoWerUp
        MOV        powerUpActivated,CL
        
        CMP  PoWerUp,Green    ;increase this car speed 
        JNE  N1
        INC  thisSpeed
N1:
        CMP  PoWerUp,Orange  ;decrease  other car speed 
        JNE N2
        cmp otherSpeed,1
        JE  N2
        DEC  otherSpeed
N2:
        CMP  PowerUp,White   ;Generate an obstacle behind the player 
        JNE N3
        MOV DI,ObstaclesNum
        ADD DI,ObstaclesNum
        MOV AX,X
        MOV ObstacleX[DI],AX
        MOV AX,Y
        MOV ObstacleY[DI],AX
        ADD  ObstaclesNum,1
        MOV EndActivatedPowerUp,Black
N3:
        CMP PowerUp,Purple   ;go throw an obstacle
        JNE N4
        MOV CancelObstale,1  ;we need to check on it before decreacing the speed after collisions 
N4:
        CMP PowerUp,ObstacleColor   ;decrement speed after collision 
        JNE N9
        cmp ThisSpeed,1
        JE  N9
        DEC thisSpeed 
N9:
        
        MOV        PoWerUp,Black
 ENDM
;___________________________________________________________________________________________________________________________________
 DeactivatePowerUp MACRO  thisSpeed,otherSpeed,PoWerUp,CancelObstale,EndActivatedPowerUp,powerUpActivated
                LOCAL N5
                LOCAL N6
                LOCAL N7
                LOCAL N8
        CMP        powerUpActivated,black
        JE         N8
        MOV        EndActivatedPowerUp,70d

        CMP  powerUpActivated,Green    ;increase this car speed 
        JNE  N5
        DEC  thisSpeed
N5:
        CMP  powerUpActivated,Orange  ;decrease  other car speed 
        JNE  N6
        INC  otherSpeed
N6:
        CMP powerUpActivated,Purple   ;go throw an obstacle
        JNE N7
        MOV CancelObstale,0  ;we need to check on it before decreacing the speed after collisions 
N7:
        CMP powerUpActivated,ObstacleColor   
        JNE N8
        INC thisSpeed
N8:

        MOV        powerUpActivated,black
 ENDM
;____________________________________________________________________________________________________________________________________
CheckIfPowerUpInTrack Macro X,Y

    MOV AX, 0A000H
    MOV ES, AX

    CheckUL:
    MOV AX,Y 
    SUB AX,1 

    MOV BX,X
    SUB BX,1

    MOV CX,ScreenWidth
    MUL CX

    ADD BX,AX
    CMP BYTE PTR ES:[BX], Gray
    JNE OutOfTrack

    CheckLR:
    MOV AX,Y 
    ADD AX,1 

    MOV BX,X
    ADD BX,1 

    MOV CX,ScreenWidth
    MUL CX

    ADD BX,AX
    CMP BYTE PTR ES:[BX], Gray
    JE InTrack

    OutOfTrack:
    mov BX,0
    InTrack:
ENDM
;____________________________________________________________________________________________________________________________________
DrawSquare MACRO X,Y,Colour,Size
               LOCAL Row
               LOCAL Col
               MOV   AX, 0A000H
               MOV   ES, AX
               MOV   AX, 320D
               MOV   BX,Y
               IMUL  BX
               ADD   AX,X

               MOV   DX,Size
               MOV   BX, 320 - Size
               MOV   DI,AX

    Row:       
               MOV   CX,Size

    Col:       
               MOV   BYTE PTR ES:[DI],Colour
               INC   DI
               LOOP  Col
 
               ADD   DI, BX
               DEC   DX
               JNZ   Row
ENDM
;______________________________________________________________________________________________________
.286
.MODEL SMALL
.STACK 264
.data
    leftLimit equ 5
    rightLimit equ  320-46
    upLimit equ 45+9
    downLimit equ 124
    
    ;carwidth EQU 20d
    trackwidth        EQU 45d
    tracklength       EQU 30d
    rectnum           EQU 5
    line1x            DW  10D
    line1y            DW  10D
    line2x            DW  10D
    line2y            DW  10D
    linexmax          DW  ?
    lineymax          DW  ?
    lastmove          db  10                            
    ; 0 top
    ; 1 right
    ; 2 down
    ; 3 left
    color             DB  Pink
    fillColor         db  Gray
    cnt               db  0
    points DW 60 dup(?);
    kind DB  30 dup(?);
    tracknumber DW 0
    ; from 1 to 3 and from 2 to 4
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    KeyList           DB  128 dup(0)

    ScreenWidth       EQU 320

    ScreenHeight      EQU 200
    ScreenSize        EQU ScreenHeight * ScreenWidth

    GameScreenHeight  EQU 150
    GameScreenCenterY EQU 85

    StatsBarHeight    EQU 10
    StatsBarCenterY   EQU 5

    InlineChatHeight  EQU 40
    InlineChatCentreY EQU 300


    Black             EQU 00
    Blue              EQU 01
    Green             EQU 02
    Red               EQU 04
    Orange            EQU 06
    White             EQU 15
    Gray              EQU 07
    Purple            EQU 05
    Pink              EQU 13
    CarSize           EQU 10
    DefSpeed          EQU 2
    ;;X and Y here represents the upper-left corner of the square
    Car1X_Old         DW  25
    Car1Y_Old         DW  25
    Car1X             DW  20
    Car1Y             DW  25
    Car1Speed         DW  DefSpeed
    Car1Colour        EQU Blue
    CancelObstale1    DB  0
    Car1PowerUp       DB  Black
    EndActivatedPowerUp1 DB 70d
    powerUpActivated1  DB     0
    COlidedObs1        DB     1

    Car2X_Old         DW  25
    Car2Y_Old         DW  25
    Car2X             DW  35
    Car2Y             DW  25
    Car2Speed         DW  DefSpeed
    Car2Colour        EQU Red
    CancelObstale2    DB  0
    Car2PowerUp       DB  Black
    EndActivatedPowerUp2 DB 70d
    powerUpActivated2  DB     0
    COlidedObs2        DB     2


    
    PowerUpNum        DW  0
    PoWerUpx          DW  15 DUP(0)
    PoWerUpY          DW  15 DUP(0)
    PoWerUpcolor      DB  15 DUP(?)

    
    ObstacleX        DW  10 DUP(0)
    ObstacleY        DW  10 DUP(0)
    ObstaclesNum     DW  5 
    ObstacleColor    EQU  23d
    ObstacleSize     EQU  5
    
    EndTimeSec        DB  0
    EndTimeMin        DB  ?
    RemainMin         DB  ?
    RemainSec         DB  ?
    ShowPowerUpT      DB  50

;__________________________________________________score data______________________________________
score1 db ?
score2 db ?
score dw ?
currentpath dw ?
tracklong dw ?
index dw ?
row dw ?
column dw ?
player1 db "player1 score is ",'$'
player2 db "player2 score is ",'$'
winner1 db "winner is player1",'$'
winner2 db "winner is player2",'$'
.code

;___________________________________________________________________
generateFirstRandom PROC 
     mov si,200h
     loopFrand:  ;loop to slow down
        mov ah, 2ch
        int 21h
        mov ah, 0
        mov al, dl  ;;micro seconds?
        mov bl, 2
        div bl
        ;;; ah = rest               
        dec si
        cmp si,0
        jnz loopFrand      
        RET
generateFirstRandom ENDP
generateRandom PROC 
     mov si,200h
     looprand:  ;loop to slow down

        mov ah, 2ch
        int 21h
        mov ah, 0
        mov al, dl  ;;micro seconds?
        mov bl, 3
        div bl
        ;;; ah = rest               
        dec si
        cmp si,0
        jnz looprand      
        RET
generateRandom ENDP


; ||||||||||| Calculating DI ||||||||||||
CalcLine1Di PROC 
   mov ax,line1y
   mov bx,320d
   mul bx
   add ax,line1x
   mov di,ax 
   RET 
CalcLine1Di ENDP


CalcLine2Di PROC 
   mov ax,line2y
   mov bx,320d
   mul bx
   add ax,line2x
   mov di,ax  
   RET
CalcLine2Di ENDP

CalcXgret PROC
   mov bx,line1x
   mov dx,line2x
   cmp bx,dx
   jl lesserx

      mov linexmax,bx
      jmp xgretEnd
   lesserx:
      mov linexmax,dx

   xgretEnd:
   mov dx,linexmax
   RET
CalcXgret ENDP

CalcYgret PROC
   mov bx,line1y
   mov dx,line2y
   cmp bx,dx
   jg greetery
   
      mov lineymax,dx
      jmp ygretEnd
   greetery:
      mov lineymax,bx
   ygretEnd:

   RET
CalcYgret ENDP

CalcYlesser PROC
   mov bx,line1y
   mov dx,line2y
   cmp bx,dx
   jl lessery
   
      mov lineymax,dx
      jmp ylessEnd
   lessery:
      mov lineymax,bx
   ylessEnd:

   RET
CalcYlesser ENDP

CalcLineMaxDi PROC 
   mov ax,lineymax
   mov bx,320d
   mul bx
   add ax,linexmax
   mov di,ax  
   RET
CalcLineMaxDi ENDP

; ||||||||||| END DI ||||||||||||


DrawRightDown PROC 

call CalcLine1Di
mov bx,di
mov cx, trackwidth
mov al,color
rep STOSB

mov cx,trackwidth
mov al,color
loop3:
   mov es:[di],al
   add di,320d
loop loop3

mov cnt,trackwidth
dec cnt

momoloop:
    add bx,320d
    mov di,bx
    mov cx,trackwidth
    mov al,fillColor
    exa:
      mov es:[di],al
      inc di
      loop exa
    dec cnt
    cmp cnt,0
    jg momoloop

mov bx,trackwidth
add line1x,bx
add line1y,bx


RET
DrawRightDown ENDP


DrawDownRight PROC 

call CalcLine2Di
mov dx,di

mov cx,trackwidth
mov al,color
loop4:
   mov es:[di],al
   add di,320d
loop loop4

mov cx, trackwidth
mov al,color
rep STOSB

mov cnt,trackwidth
dec cnt

lab:
    add dx,1d
    mov di,dx
    mov cx,trackwidth
    mov al,fillColor
   loop5x:
      mov es:[di],al
      add di,320d
      loop loop5x
    dec cnt
    cmp cnt,0
    jne lab

mov bx,trackwidth
add line2x,bx
add line2y,bx

;mov lastmove,1
RET
DrawDownRight ENDP

;///////// Draw UP \\\\\\\\



DrawRightUp PROC 


call CalcLine2Di
mov dx,di
mov cx, trackwidth
mov al,color
rep STOSB

mov cx,trackwidth
mov al,color
loop120:
   mov es:[di],al
   sub di,320d
loop loop120

mov cnt,trackwidth
dec cnt

la1:
    sub dx,320d
    mov di,dx
    mov cx,trackwidth
    mov al,fillColor
    rep STOSB
    dec cnt
    cmp cnt,0
    jne la1

mov bx,trackwidth
add line2x,bx
sub line2y,bx

RET
DrawRightUp ENDP



DrawUp PROC 

jmp l32
l31:
    call DrawRightUp
    mov lastmove,0

l32:  
   call CalcYlesser
   mov dx,lineymax
   sub dx,tracklength
   cmp dx,upLimit
   jl bkupskip
   cmp lastmove,2
   je bkupskip
   
   cmp lastmove,1
       je l31


call CalcLine1Di
mov bx,offset points
add bx,tracknumber
add bx,tracknumber
add bx,tracknumber
add bx,tracknumber
mov [bx],di

jmp bkdone
bkupskip:
jmp skipup
bkdone:

mov dx,di
mov cx,tracklength
mov al, color
loop20:
   mov es:[di],al
   sub di,320d
loop loop20

mov [bx+2],di
mov cnt,trackwidth
dec cnt

lzz:
    add dx,1d
    mov di,dx
    mov cx,tracklength
    mov al,fillColor
   loop3x:
      mov es:[di],al
      sub di,320d
      loop loop3x
    dec cnt
    cmp cnt,0
    jne lzz

sub line1y,tracklength

call CalcLine2Di


mov cx,tracklength
mov al,color
loop21:
   mov es:[di],al
   sub di,320d
loop loop21

sub line2y,tracklength

mov lastmove,0
mov bx, offset kind
add bx, tracknumber
push ax
mov al,0d
mov [bx],al
pop ax

inc tracknumber
skipup:

RET
DrawUp ENDP


DrawUpRight PROC 

call CalcLine1Di
mov dx,di
mov cx,trackwidth
mov al,color
loop110:
   mov es:[di],al
   sub di,320d
loop loop110
mov cx, trackwidth
mov al,color
rep STOSB

mov cnt,trackwidth
dec cnt

laa:
    add dx,1d
    mov di,dx
    mov cx,trackwidth
    mov al,fillColor
   loop4x:
      mov es:[di],al
      sub di,320d
      loop loop4x
    dec cnt
    cmp cnt,0
    jne laa


mov bx,trackwidth
add line1x,bx
sub line1y,bx

RET
DrawUpRight ENDP




;///////// Draw Right \\\\\\\\

DrawRight PROC 

jmp l23
l21:
    call DrawDownRight
    mov lastmove,1
    jmp l23
l22:   
    call DrawUpRight
    mov lastmove,1
    jmp l23


mov lastmove,1

l23: 
   call CalcXgret
   add dx,tracklength
   cmp dx,rightLimit
   jg bkskipright
    cmp lastmove,2
       je l21
    cmp lastmove,0
       je l22


call CalcLine1Di
mov bx,offset points
add bx,tracknumber
add bx,tracknumber
add bx,tracknumber
add bx,tracknumber
mov [bx],di
mov dx,di
add dx,tracklength
mov [bx+2],dx

jmp donebk
bkskipright:
   jmp skipRight
donebk:

mov dx,di
mov cx,tracklength
mov al, color
rep STOSB

add line1x,tracklength

mov cnt,trackwidth
dec cnt
lll:
    add dx,320d
    mov di,dx
    mov cx,tracklength
    mov al,fillColor
    rep STOSB
    dec cnt
    cmp cnt,0
    jne lll




call CalcLine2Di


mov dx,di

mov cx,tracklength
mov al,color
rep STOSB

add line2x,tracklength

mov lastmove,1
mov bx,offset kind
add bx, tracknumber
push ax
mov al,1d
mov [bx],al
pop ax


inc tracknumber
skipRight:


RET
DrawRight ENDP
;;;;;



;///////// Draw Down \\\\\\\\

DrawDown PROC 


jmp l12
l11:
    call DrawRightDown
    mov lastmove,2

l12:    
   call CalcYgret
   mov dx,lineymax
   add dx,tracklength
   cmp dx,downLimit
   jg bkskipdown
   cmp lastmove,0
   je bkskipdown
   cmp lastmove,1
       je l11


call CalcLine1Di
mov bx,offset points
add bx,tracknumber
add bx,tracknumber
add bx,tracknumber
add bx,tracknumber
mov [bx],di



mov dx,di


jmp donerightbk
bkskipdown:
   jmp skipdown
donerightbk:

mov cx,tracklength
mov al, color
loop1:
   mov es:[di],al
   add di,320d
loop loop1

mov [bx+2],di

add line1y,tracklength



call CalcLine2Di


mov dx,di

mov cx,tracklength
mov al,color
loop2:
   mov es:[di],al
   add di,320d
loop loop2

mov cnt,trackwidth
dec cnt
lxx:
    add dx,1d
    mov di,dx
    mov cx,tracklength
    mov al,fillColor
   loop2x:
      mov es:[di],al
      add di,320d
      loop loop2x
    dec cnt
    cmp cnt,0
    jne lxx

add line2y,tracklength

mov lastmove,2
mov bx,offset kind
add bx, tracknumber
push ax
mov al,2d
mov [bx],al
pop ax
inc tracknumber
skipdown:
RET
DrawDown ENDP

Draw PROC FAR
                    MOV         AX,@DATA                                 
                     MOV         DS,AX

                    ;  MOV         AX, 0A000H                             ;Comment this sentence before Integrating files
                    ;  MOV         ES, AX

                    ;  MOV         AX,13H                                 ;switch to video mode    
                    ;  INT         10H

                    ; CALL ClearScreen     
                    
tryagain:
mov tracknumber,0
mov ax,0003h;to clearscreen
int 10h
mov ax,0a000h;to graphics screen
mov es,ax
mov ax,0013h; 320*200 screen
int 10h
jmp bkpoint3
try3:
jmp tryagain
bkpoint3:

;top outline border
mov di,3200d;row 5
mov al,2h
mov cx,320
rep STOSB
;bottom outline border
; mov di,51200d;row 159
; mov al,2h
; mov cx,320
; rep STOSB

jmp bkpoint2
try2:
jmp try3
bkpoint2:


mov line1x,5d
mov line1y,15d
mov line2x,5d
mov line2y,15d

call generateFirstRandom ;firstRandom
cmp ah,0 ;Go right
je firstRight

add line1x, trackwidth
call DrawDown
jmp next
firstRight:
   add line2y, trackwidth
   call DrawRight
next:


jmp bkpoint1
try1:
jmp try2
bkpoint1:

mov cx,100d

generateTrack:
   dec cx
   push cx
   call generateRandom
   cmp ah,0 ;Go Up
   je goUp
   cmp ah,1 ;Go Right
   je goRight
   ;///Go Down\\\\
   call DrawDown
   jmp finish
   goUp:
      call DrawUp
      jmp finish
   goRight:
      call DrawRight
   finish:
   pop cx
    loop generateTrack 

cmp lastmove,1
je finish2
cmp lastmove,2
je finish1
call CalcLine1Di
mov cx,trackwidth
mov al,0bh
rep STOSB
; delete this if does not work
call CalcLine1Di
sub di,320d
mov cx,trackwidth
mov al,0bh
rep STOSB
;-------------
jmp ending
finish1:
call CalcLine2Di
mov cx,trackwidth
mov al,0bh
rep STOSB
; delete this if does not work
call CalcLine2Di
add di,320d
mov cx,trackwidth
mov al,0bh
rep STOSB
;-------------
jmp ending
finish2:
call CalcLine1Di
mov cx,trackwidth
mov al,0bh
verti:
   mov es:[di],al
   add di,320d
   loop verti
;delete this if dose not work
call CalcLine1Di
add di,1d
mov cx,trackwidth
mov al,0bh
vertj:
   mov es:[di],al
   add di,320d
   loop vertj
;---------------------------

ending:

                        CalcTrackLong
                        scorebar
                        CALL        GenerateObstacles
                        CALL        GeneratePowerUps
                        CALL        generateRandomN
                        MOV         PowerUpNum,6

                   
                        mov ah,2ch 
                        int 21h
                        Mov EndTimeSec,DH
                        ADD CL,2
                        MOV        AL,CL
                        MOV        AH,0
                        MOV        BL,60d
                        DIV        BL
                        MOV      EndTimeMin,CL 

    ;-----------------------------------------------
                        MOV        AX, 3509h                              ; DOS.GetInterruptVector
                        INT        21h                                    ; -> ES:BX
                        PUSH       ES
                        PUSH       BX                                     ; (1)

                        mov        dx, offset Int09
                        PUSH       DS
                        MOV        AX,CS
                        MOV        DS,AX
                        mov        AX, 2509h                              ; DOS.SetInterruptVector
                        int        21h
                        POP        DS                                     ;dah kda zy ma howa
    ;------------------------------------------------
;------------------------------------------------Game Loop------------------------------------------------;
gameLoop:    
                        CMP score1,100d 
                        JE  ThereIsWinner
                        CMP score2,100d 
                        JE  ThereIsWinner
                        CALL UpdateTimer
                        CMP RemainMin,0
                        JNE continue  
                        CMP RemainSec,0
                        JNE  continue
                        ThereIsWinner:
                        JMP  FAR PTR EndGame  
continue:
                        MOV AH,ShowPowerUpT
                        CMP RemainSec,AH
                        JNE NoMore
                        CMP PowerUpNum,15
                        JE  NoMore
                        INC PowerUpNum
                        SUB ShowPowerUpT,10
                        CMP ShowPowerUpT,0 
                        JNE NoMore
                        ADD ShowPowerUpT,50
NoMore:                     
                        CALL       DrawPowerUpS
                        CALL       ReDrawObs
                        CALL       DrawCar1
                        CALL       DrawCar2
                        scorebar
                        call       sleepSomeTime
                        call       sleepSomeTime
                        call       sleepSomeTime

    ;;saving current car position before moving the car
                        MOV        AX, Car1X
                        MOV        Car1X_Old, AX
                        MOV        AX, Car1Y
                        MOV        Car1Y_Old, AX
                        MOV        AX, Car2X
                        MOV        Car2X_Old, AX
                        MOV        AX, Car2Y
                        MOV        Car2Y_Old, AX


                        CMP        [KeyList+3EH],1                        ;F4 key to exit the game 
                        JNE        C0
                        JMP        EndGame

    C0:                 
                        CMP        [KeyList+50H],1                        ;DOWN arrow
                        JNE        C1
                        CALL       Move1Down

    C1:                 
                        CMP        [KeyList+4BH],1                        ;Left arrow
                        JNE        C2
                        CALL       Move1Left

    C2:                 
                        CMP        [KeyList+4DH],1                        ;Right arrow
                        JNE        C3
                        CALL       Move1Right

    C3:                 
                        CMP        [KeyList+48H],1                        ;UP arrow
                        JNE        C4
                        CALL       Move1Up
 

    C4:                 
                        CMP        [KeyList+1FH],1                        ;S
                        JNE        C5
                        CALL       Move2Down

    C5:                 
                        CMP        [KeyList+1EH],1                        ;A
                        JNE        C6
                        CALL       Move2Left


    C6:                 
                        CMP        [KeyList+20H],1                        ;D
                        JNE        C7
                        CALL       Move2Right

    C7:
                        CMP         [KeyList+11h],1                        ;W
                        JNE          C8
                        CALL        Move2Up
    C8:
                        CMP         [KeyList+35h],1                        ;? to activate car1PowerUP 
                        JE          AC1
                        JMP         C9
                        AC1:
                        ActivatePowerUp Car1Speed,Car2Speed,Car1PowerUp,CancelObstale1,EndActivatedPowerUp1,powerUpActivated1,Car1X,Car1Y
    C9:
                        CMP         [KeyList+10h],1                        ;Q to activate car2PowerUP 
                        JE          AC2 
                        JMP         C10
                        AC2:   
                        ActivatePowerUp Car2Speed,Car1Speed,Car2PowerUp,CancelObstale2,EndActivatedPowerUp2,powerUpActivated2,Car2X,Car2Y
                        
    C10:             
                        mov ah,2ch 
                        int 21h
                        cmp  dh,EndActivatedPowerUp1
                        JE   deAct1
                        cmp  dh,EndActivatedPowerUp2
                        JE   deAct2
                        JNE  here

 deAct1:                 
                        DeactivatePowerUp Car1Speed,Car2Speed,Car1PowerUp,CancelObstale1,EndActivatedPowerUp1,powerUpActivated1
                        cmp  dh,EndActivatedPowerUp2
                        JE   deAct2
 here:                       jmp break
 deAct2:                 
                         DeactivatePowerUp Car2Speed,Car1Speed,Car2PowerUp,CancelObstale2,EndActivatedPowerUp2,powerUpActivated2

 break: JMP gameLoop
 EndGame:
    ;-------------------------------------------------
    ;slow program for second
    mov bx,100d
    slowprogramAftergamefinish:
    call sleepSomeTime
    dec bx
    jnz slowprogramAftergamefinish
    ; change to text mode
    mov ah,0h
    mov al,03h
    int 10h
    ; print player one score set curser position in 11 row, 30 column
    mov ah,2h
    mov dh,11d
    mov dl,30d
    mov bh,0
    int 10h
    mov ah,9h
    mov dx,offset player1
    int 21h
    printscoretext score1
    ;print player two score set curser position in 12 row, 30 column
    mov ah,2h
    mov dh,12d
    mov dl,30d
    mov bh,0
    int 10h
    mov ah,9h
    mov dx,offset player2
    int 21h
    printscoretext score2
    ; print winner set curser position in 13 row, 30 column
    mov ah,2h
    mov dh,13d
    mov dl,30d
    mov bh,0
    int 10h
    cmp score1,100d
    je player1winner
    cmp score2,100d
    jne nowinner
    mov ah,9h
    mov dx,offset winner2
    int 21h
    jmp nowinner
    player1winner:
    mov ah,9h
    mov dx,offset winner1
    int 21h
    nowinner:
    ;slow program for five second
    mov bx,270d
    slowprogramAfterShowstatus:
    call sleepSomeTime
    dec bx
    jnz slowprogramAfterShowstatus
    ;-------------------------------------------------
                        POP        dx
                        POP        ds                                     ; (1) bardo zi ma howa
                        MOV        ax, 2509h                              ; DOS.SetInterruptVector
                        INT        21h

                       ; MOV        ax, 4C00h                              ; DOS.Terminate
                        ;INT        21h
    ;-------------------------------------------------
    ret
Draw ENDP

UpdateTimer  PROC 
                           
        mov ah,2ch 
        int 21h
        mov AL,EndTimeMin
        SUB AL,CL
        MOV RemainMin,AL
        MOV AH,EndTimeSec
        CMP AH,DH
        JB  H
        SUB AH,DH
        MOV RemainSec,AH
        JMP Quit
H:
        ADD AH,60d
        SUB AH,DH
        MOV RemainSec,AH
        DEC RemainMin
Quit:
        printTimer
        RET
UpdateTimer  ENDP 

INT09 PROC
                        push       ax
                        PUSH       bx
                        in         al, 60h
                        mov        ah, 0
                        mov        bx, ax
                        and        bx, 127                                ; 7-bit scancode goes to BX
                        shl        ax, 1                                  ; 1-bit press/release goes to AH
                        xor        ah, 1                                  ; -> AH=1 Press, AH=0 Release
                        mov        [KeyList+bx], ah
                        mov        al, 20h                                ; The non specific EOI (End Of Interrupt)
                        out        20h, al
                        pop        bx
                        POP        ax
                        iret
INT09 ENDP
    ;;__________________________________DrawCar__________________________________;;

DrawCar1 PROC

    ;;determining the memory location based on the new car location
                        DrawSquare Car1X_Old, Car1Y_Old, Gray, CarSize
                        MOV        AX, ScreenWidth
                        MOV        BX,Car1Y
                        IMUL       BX
                        ADD        AX, Car1X
                        MOV        DX, CarSize
                        MOV        BX, ScreenWidth - CarSize
                        MOV        DI,AX

    Car1Row:            
                        MOV        CX,CarSize

    Car1Col:            
                        MOV        BYTE PTR ES:[DI], Car1Colour
                        INC        DI
                        LOOP       Car1Col

                        ADD        DI, BX
                        DEC        DX
                        JNZ        Car1Row
                        RET
DrawCar1 ENDP

DrawCar2 PROC
                        DrawSquare Car2X_Old, Car2Y_Old, Gray, CarSize
                        MOV        AX, 0A000H
                        MOV        ES, AX
                        MOV        AX, ScreenWidth
                        MOV        BX,Car2Y
                        IMUL       BX
                        ADD        AX, Car2X

                        MOV        DX, CarSize
                        MOV        BX, ScreenWidth - CarSize
                        MOV        DI,AX

    Car2Row:            
                        MOV        CX,CarSize

    Car2Col:            
                        MOV        BYTE PTR ES:[DI], Car2Colour
                        INC        DI
                        LOOP       Car2Col
 
                        ADD        DI, BX
                        DEC        DX
                        JNZ        Car2Row
                        RET
DrawCar2 ENDP

    ;;__________________________________MoveCar__________________________________;;

    ;;upon Moving the car, we need to:
    ;;Check if the new position is within the track
    ;;Check if there are any obstacles
    ;;Check if there's a powerup
    

Move1Down PROC
                        MOV        AX, Car1Y
                        MOV        Car1Y_Old, AX
                        MOV        AX,Car1Y
                        ADD        AX,Car1Speed
                        MOV        Car1Y,AX
    CheckIfWithinTrack Car1X, Car1Y, Car1X_Old, Car1Y_Old
    CheckCollision Car1X, Car1Y, Car1X_Old, Car1Y_Old,Car2Colour
    CheckObsCollision Car1X, Car1Y, Car1X_Old, Car1Y_Old,COlidedObs1,CancelObstale1
    CollectPowerUp     Car1PowerUp, Car1X, Car1Y
                        RET
Move1Down ENDP

Move1Up PROC
                        MOV        AX, Car1Y
                        MOV        Car1Y_Old, AX

                        MOV        AX,Car1Y
                        SUB        AX,Car1Speed
                        MOV        Car1Y,AX
    CheckIfWithinTrack Car1X, Car1Y, Car1X_Old, Car1Y_Old
    CheckCollision Car1X, Car1Y, Car1X_Old, Car1Y_Old,Car2Colour
    CheckObsCollision Car1X, Car1Y, Car1X_Old, Car1Y_Old,COlidedObs1,CancelObstale1
    CollectPowerUp     Car1PowerUp, Car1X, Car1Y
                        RET
Move1Up ENDP

Move1Right PROC
                        MOV        AX, Car1X
                        MOV        Car1X_Old, AX

                        MOV        AX,Car1X
                        ADD        AX,Car1Speed
                        MOV        Car1X,AX
    CheckIfWithinTrack Car1X, Car1Y, Car1X_Old, Car1Y_Old
    CheckCollision Car1X, Car1Y, Car1X_Old, Car1Y_Old,Car2Colour
    CheckObsCollision Car1X, Car1Y, Car1X_Old, Car1Y_Old,COlidedObs1,CancelObstale1
    CollectPowerUp     Car1PowerUp, Car1X, Car1Y
                        RET
Move1Right ENDP

Move1Left PROC
                        MOV        AX, Car1X
                        MOV        Car1X_Old, AX

                        MOV        AX,Car1X
                        SUB        AX,Car1Speed
                        MOV        Car1X,AX
    CheckIfWithinTrack Car1X, Car1Y, Car1X_Old, Car1Y_Old
    CheckCollision Car1X, Car1Y, Car1X_Old, Car1Y_Old,Car2Colour
    CheckObsCollision Car1X, Car1Y, Car1X_Old, Car1Y_Old,COlidedObs1,CancelObstale1
    CollectPowerUp     Car1PowerUp, Car1X, Car1Y
                        RET
Move1Left ENDP
    ;___

Move2Down PROC
                        MOV        AX, Car2Y
                        MOV        Car2Y_Old, AX
                        MOV        AX,Car2Y
                        ADD        AX,Car2Speed
                        MOV        Car2Y,AX
    CheckIfWithinTrack Car2X, Car2Y, Car2X_Old, Car2Y_Old
    CheckCollision Car2X, Car2Y, Car2X_Old, Car2Y_Old,Car1Colour
    CheckObsCollision Car2X, Car2Y, Car2X_Old, Car2Y_Old,COlidedObs2,CancelObstale2
    CollectPowerUp     Car2PowerUp, Car2X, Car2Y
                        RET
Move2Down ENDP

Move2Up PROC
                        MOV        AX, Car2Y
                        MOV        Car2Y_Old, AX
                        MOV        AX,Car2Y
                        SUB        AX,Car2Speed
                        MOV        Car2Y,AX
    CheckIfWithinTrack Car2X, Car2Y, Car2X_Old, Car2Y_Old
    CheckCollision Car2X, Car2Y, Car2X_Old, Car2Y_Old,Car1Colour
    CheckObsCollision Car2X, Car2Y, Car2X_Old, Car2Y_Old,COlidedObs2,CancelObstale2
    CollectPowerUp     Car2PowerUp, Car2X, Car2Y
                        RET
Move2Up ENDP

Move2Right PROC
                        MOV        AX, Car2X
                        MOV        Car2X_Old, AX
                        MOV        AX,Car2X
                        ADD        AX,Car2Speed
                        MOV        Car2X,AX    
                        CheckIfWithinTrack Car2X, Car2Y, Car2X_Old, Car2Y_Old
                        CheckCollision Car2X, Car2Y, Car2X_Old, Car2Y_Old,Car1Colour
                        CheckObsCollision Car2X, Car2Y, Car2X_Old, Car2Y_Old,COlidedObs2,CancelObstale2
                        CollectPowerUp     Car2PowerUp, Car2X, Car2Y
                        RET
Move2Right ENDP

Move2Left PROC
                        MOV        AX, Car2x
                        MOV        Car2X_Old, AX
                        MOV        AX,Car2X
                        SUB        AX,Car2Speed
                        MOV        Car2X,AX
     CheckIfWithinTrack Car2X, Car2Y, Car2X_Old, Car2Y_Old
     CheckCollision Car2X, Car2Y, Car2X_Old, Car2Y_Old,Car1Colour
     CheckObsCollision Car2X, Car2Y, Car2X_Old, Car2Y_Old,COlidedObs2,CancelObstale2
     CollectPowerUp     Car2PowerUp, Car2X, Car2Y
                        RET
Move2Left ENDP



    ;___
sleepSomeTime proc
                        push       cx
                        push       dx
                        mov        cx, 0
                        mov        dx,20000                               ; 20ms
                        mov        ah, 86h
                        int        15h                                    ; param is cx:dx (in microseconds)
                        pop        dx
                        pop        cx
                        ret
sleepSomeTime endp
ClearScreen PROC
                        mov        ax,0600h
                        mov        bh,07
                        mov        cx,0
                        mov        dx,184FH
                        int        10h
                        RET
ClearScreen ENDP
   ;___________________________________________________________________________________________________________________________________________
    ;GeneratePowerUps
    ;___________________________________________________________________________________________________________________________________________
generateRandomY PROC
                       mov        si,800d
   RandomY:                                            ;loop to slow down

                       mov        ah, 2ch
                       int        21h
                       dec        si
                       cmp        si,0
                       jnz        RandomY
                       MOV        AX,DX
                       XOR        DX,DX
                       mov        CX, ScreenHeight-18
                       DIV        CX
                       RET
generateRandomY ENDP

generateRandomN PROC
                       mov        si,100d
   RandomN:                                            ;loop to slow down

                       mov        ah, 2ch
                       int        21h
                       MOV        AX,DX
                       XOR        DX,DX
                       mov        CX,6
                       DIV        CX
                       ADD        DX,5
                       dec        si
                       cmp        si,0
                       jnz        RandomN
                       RET
generateRandomN ENDP

generateRandomX PROC
                       mov        si,800d
   RandomX:                                            ;loop to slow down

                       mov        ah, 2ch
                       int        21h
                       dec        si
                       cmp        si,0
                       jnz        RandomX
                       MOV        AX,DX
                       XOR        DX,DX
                       mov        CX,320
                       DIV        CX
                       RET
generateRandomX ENDP

GeneratePowerUps PROC
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GetRandomColumns;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     MOV         DI,0
    Generate:       
                     CALL        generateRandomX
                     MOV         PoWerUpx[DI],DX
                     ADD         DX,2
                     CALL        generateRandomY
                     ADD         DX,2
                     MOV         PoWerUpY[DI],DX
                     ;check if x,y is in track else jmp Generate without increasing di
                     CheckIfPowerUpInTrack  PoWerUpx[DI],PoWerUpy[DI]
                     cmp         BX,0
                     JE          Generate
                     CMP         DI,12
                     JBE         next2
                     CMP         PoWerUpx[DI],160
                     JB          Generate
next2:
                     ADD         DI,2                                   ;get the next word
                     CMP         DI,30
                     JNE         Generate
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GetRandomColors;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     MOV         DI,0
                     MOV         AH, 00h                                ; interrupts to get system time
                     INT         1AH                                    ; CX:DX now hold number of clock ticks since midnight
                     MOV         AX, DX
                     XOR         DX, DX
                     MOV         CX, 4
                     ADD         AX,BX
                     DIV         CX                                     ; here dx contains the remainder of the division - from 0 to 320
    GenerateColor:   
                     ADD         DL,1
                     MOV         AL,DL
                     MOV         AH,00
                     MOV         CL,4h
                     DIV         CL
                     MOV         PoWerUpcolor[DI],AH
                     ;____
                     CMP         PoWerUpcolor[DI],0
                     JE          colorGreen
                     CMP         PoWerUpcolor[DI],1
                     JE          colorOrange
                     CMP         PoWerUpcolor[DI],2
                     JE          colorWhite
                     CMP         PoWerUpcolor[DI],3
                     JE          colorPurple
    colorGreen:       
                     mov         PoWerUpcolor[DI],Green
                     JMP         next1
    colorOrange:     
                     MOV         PoWerUpcolor[DI],Orange
                     JMP         next1
    colorWhite:      
                     MOV         PoWerUpcolor[DI],White
                     JMP         next1
    colorPurple:     
                     MOV         PoWerUpcolor[DI],Purple
                     ;____
    next1:            INC         DI
                     CMP         DI,15
                     JNE         GenerateColor
                     RET
    GeneratePowerUps                 ENDP

DrawPowerUpS PROC
                     MOV         DI,0
                     mov         SI,0
    DrawAgain:                  
                     CMP         PoWerUpx[DI],0
                     JE          N
                     CMP         PoWerUpy[DI],0
                     JE          N
                     DrawPowerUp PoWerUpx[DI],PoWerUpY[DI],PoWerUpcolor[SI]
    N:               
                     ADD         DI,2
                     inc         SI
                     CMP         SI,PowerUpNum
                     JNZ         DrawAgain
                     RET
    DrawPowerUpS               ENDP
;------------------------------------------------
CheckIfObstacleInTrack MACRO X,Y
    MOV AX, 0A000H
    MOV ES, AX

    CheckUL1:
    MOV AX,Y
    MOV BX,X

    MOV CX,ScreenWidth
    MUL CX

    ADD BX,AX
    CMP BYTE PTR ES:[BX], Gray
    JNE OutOfTrack2

    CheckLR1:
    MOV AX,Y
    MOV BX,X
    ADD BX,obstacleSize
    ADD AX,obstacleSize

    MOV CX,ScreenWidth
    MUL CX

    ADD BX,AX
    CMP BYTE PTR ES:[BX], Gray
    JE InTrack2

    OutOfTrack2:
    mov BX,0
    InTrack2:
    ENDM
GenerateObstacles PROC
                     MOV         DI,0
    Generate2:       
                     CALL        generateRandomX
                     MOV         ObstacleX[DI],DX
                     CALL        generateRandomY
                     MOV         ObstacleY[DI],DX

                     CheckIfObstacleInTrack ObstacleX[DI],ObstacleY[DI]
                     CMP         BX,0
                     JE          Generate2 
                     PUSH        DI
                     DrawSquare ObstacleX[DI],ObstacleY[DI],ObstacleColor,ObstacleSize
                     POP         DI
                     ADD         DI,2
                     CMP         DI,10
                     JNE         Generate2
                     RET
GenerateObstacles                ENDP

ReDrawObs      PROC
                     MOV         DI,0
                     MOV         CX,0
    Redraw:       
                     PUSH        DI
                     PUSH        CX
                     DrawSquare ObstacleX[DI],ObstacleY[DI],ObstacleColor,ObstacleSize
                     POP         CX
                     POP         DI
                     ADD         DI,2
                     ADD         CX,1
                     CMP         CX,ObstaclesNum
                     JNE         Redraw
    RET 
ReDrawObs      ENDP

END Draw

