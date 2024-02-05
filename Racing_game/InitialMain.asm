;;Initial Screen: Somia Elshemy
;;Main Screen:  Rawan Ahmed
;;Last Version 2/12/2023;;
;_______________________________________________________________________________Data Segment_______________________________________________________________________________________________

.MODEL SMALL
.STACK 256
.DATA
    PressToCont    DB 'Press Enter To Continue' , '$'
    ;____________________NAME DATA____________________;
    Name1Msg       DB 'Player1 Name: (Name Must start with A Char)', '$'
    Name2Msg       DB 'Player2 Name: (Name Must start with A Char)', '$'
    NameCharErr    DB 'Name Must start with A Char, Enter your name again', '$'
    NameLenghtErr  DB 'Name Must Not Exceed 15 Chars'
    Player1Name    DB 30, ? , 30 DUP('$') ;;Buffer to check if the size is larger than 15
    Player2Name    DB 30, ? , 30 DUP('$') ;;Buffer to check if the size is larger than 15
    ;____________________INITIAL POINTS DATA____________________;
    InitPointsMsg  DB 'Enter Your Initial Points:' , '$'
    InitPointsErr1 DB 'Oops, You should write a number not charecters.. Enter Your points again', '$'
    InitPointsErr2 DB 'Initial Points Must Not Exceed 99,, Enter it again', '$'
    InitPoint1     DB 3, ? , 3 DUP('$')
    InitPoint2     DB 3, ? , 3 DUP('$')
    ;____________________MainScreen Args____________________;
    CurrentPlayer DB 1  ;just for phase 1
    player1       DB "player1$"
    player2       DB "player2$"
    MainMSG1      DB "To start Chatting Press f1$"
    MainMSG2      DB "To start the Game Press f2$"
    MainMSG3      DB "To End the Program Press ESC$"
    ChatScreen    DB "Welcome To Chat Mode$"
    GameScreen    DB "Welcome to Game Mode$"
    ChatInvition  DB 0,"Sent a chat invitation$ "
    GameInvition  DB 0,"Sent a game invitation$ "
    ;_________________________________________________________________________________Code Segment_____________________________________________________________________________________________
.CODE
    ;____________________ClearScreen____________________;
ClearScreen Macro   i887u8
                                     mov         ax,0600h
                                     mov         bh,07
                                     mov         cx,0
                                     mov         dx,184FH
                                     int         10h
                                    ENDM ClearScreen

    ;____________________ClearRow____________________;

ClearRow MACRO row
                                  MOV         AX,0600H
                                  MOV         BH,07
                                  MOV         CL,0
                                  MOV         CH,row
                                  MOV         DL,79
                                  MOV         DH,row
                                  INT         10H
                                ENDM    ClearRow

    ;____________________FillBackGground____________________;
FillBackGground MACRO color                                         ;for (320*200 mode)
                         MOV         CX,0                           ;starting column
                         MOV         DX,0                           ;starting row
                         MOV         AL,color
                         MOV         AH,0ch
    fill_column:         
    fill_rows:           INT         10h
                         INC         CX
                         CMP         CX,200
                         JNZ         fill_rows
                         INC         DX
                         CMP         DX,320
                         JNZ         fill_column
                        ENDM

    ;____________________SetCursor______________________;
SetCursor MACRO X,Y,page
                         MOV         BH,page
                         MOV         DL,X                           ;row number
                         MOV         DH,Y                           ;column number
                         MOV         AH,2
                         INT         10h
                         ENDM        SetCursor
    ;____________________ShowMessage____________________;
ShowMessage MACRO message
                         LEA         DX,message                     ;display msg interrup
                         MOV         Ah,9
                         INT         21h
                         ENDM        ShowMessage

    ;____________________ShowChar____________________;
ShowChar MACRO charAscii
                         MOV         AH,2
                         MOV         DL,charAscii
                         INT         21h
                         ENDM        ShowChar

    ;____________________CheckBuffer____________________;
CheckBuffer MACRO
                         MOV         AH,01h                         ;AX = 0 if no scan code is available
                         INT         16h                            ;AH = scan code
    ;AL = ASCII character or zero if special function key
                         ENDM        CheckBuffer

    ;____________________ReadChar(echo)____________________;
ReadCharE MACRO
                        MOV AH,01
                        INT 21H
                        ENDM
    ;____________________ReadChar(echo)____________________;
ReadChar MACRO
                        MOV AH,07
                        INT 21H
                        ENDM
    ;____________________ReadKey(Noecho)____________________;
ReadKey MACRO 
                    MOV AH,00h       ;AX = 0 if no scan code is available
                    INT 16h          ;AH = scan code
                                     ;AL = ASCII character or zero if special function key
ENDM ReadKey

    ;______________________________________________________________________________________________________________________________________________________________________________
    ;Ask For Player1 Data
    ;______________________________________________________________________________________________________________________________________________________________________________

AskForP1Data PROC

                         MOV DH,06
                         MOV DL,09
                         SetCursor   DL,DH,0
                         ShowMessage Name1Msg

                         MOV DH,08
                         MOV DL,09
                         SetCursor   DL,DH,0
                         ShowMessage InitPointsMsg

                         MOV DH,10
                         MOV DL,09
                         SetCursor   DL,DH,0
                         ShowMessage PressToCont
                         RET
AskForP1Data ENDP

    ;______________________________________________________________________________________________________________________________________________________________________________
    ;Ask For Player2 Data
    ;______________________________________________________________________________________________________________________________________________________________________________
AskForP2Data PROC
                        ClearScreen
                         MOV DH,06
                         MOV DL,09
                         SetCursor   DL,DH,0
                         ShowMessage Name2Msg

                         MOV DH,08
                         MOV DL,09
                         SetCursor   DL,DH,0
                         ShowMessage InitPointsMsg

                         MOV DH,10
                         MOV DL,09
                         SetCursor   DL,DH,0
                         ShowMessage PressToCont
                         RET

AskForP2Data ENDP

    ;______________________________________________________________________________________________________________________________________________________________________________
    ;Get Player1 Data
    ;______________________________________________________________________________________________________________________________________________________________________________

GetP1Data PROC

    ;Player1 Name;
    ;;;re-initializing the name to $ if the user entered more than 15 chars;;;

    reInitialize1:
    MOV BX, OFFSET Player1Name+2
    MOV CX, 30
    
    FillDollar:
    MOV BYTE PTR [BX], '$'
    INC BX
    LOOP FillDollar
    

    InvalidName1:         
                         ClearRow    07
                         MOV         DL,09
                         MOV         DH,07
                         SetCursor   DL,DH,0
    ;replace the following code with readString Macro;
                         MOV         AH,0Ah
                         MOV         DX,OFFSET Player1Name
                         INT         21h
    ;Stop here

    CheckIfNotChar1:     
                         MOV         CX,65                          ;A
                         CMP         CL, BYTE PTR Player1Name[2]
                         JA          InvalidName1
                         MOV         CX,122                         ;z
                         CMP         CL, BYTE PTR Player1Name[2]
                         JB          InvalidName1 

    CheckOnSize1:
                        MOV AL, Player1Name[1]
                        CMP AL,16
                        JAE reInitialize1


                        

    ;Player1 initial points;
        ;Read second digit
        InvalidPoint1:         
                         ClearRow    09
                         MOV         DL,09
                         MOV         DH,09
                         SetCursor   DL,DH,0
                         ReadCharE ;AL has the char
        CheckIfNotNumber1:
                        CMP AL,48
                        JB InvalidPoint1 
                        CMP AL,57
                        JA InvalidPoint1
         MOV BL,10
         MUL BL 
         MOV BL,AL      ;BL = Digit*10   (2nd digit)
        ;Read first digit
        InvalidPoint2:         
                         MOV         DL,10
                         MOV         DH,09
                         SetCursor   DL,DH,0
                         ReadCharE
        CheckIfNotNumber2:
                        CMP AL,48
                        JB InvalidPoint1 
                        CMP AL,57
                        JA InvalidPoint1
        CheckIfEnter:
        ReadChar
        CMP AL,13
        JNZ CheckIfEnter

RET
GetP1Data ENDP

    ;______________________________________________________________________________________________________________________________________________________________________________
    ;Get Player2 Data
    ;______________________________________________________________________________________________________________________________________________________________________________

GetP2Data PROC

    ;Player2 Name;

    ;;;re-initializing the name to $ if the user entered more than 15 chars;;;
    reInitialize2:
    MOV BX, OFFSET Player2Name+2
    MOV CX, 30
    
    FillDollar2:
    MOV BYTE PTR [BX], '$'
    INC BX
    LOOP FillDollar2
    

    InvalidName2:         
                         ClearRow    07
                         MOV         DL,09
                         MOV         DH,07
                         SetCursor   DL,DH,0
    ;replace the following code with readString Macro;
                         mov         AH,0Ah
                         mov         DX,OFFSET Player2Name
                         int         21h
    ;Stop here

    CheckIfNotChar2:     
                         MOV         CX,65                          ;A
                         CMP         CL, BYTE PTR Player2Name[2]
                         JA          InvalidName2
                         MOV         CX,122                         ;z
                         CMP         CL, BYTE PTR Player2Name[2]
                         JB          InvalidName2

    CheckOnSize2:
                        MOV AL, Player2Name[1]
                        CMP AL,16
                        JAE reInitialize2


    ;Player2 initial points;
        ;Read second digit
        InvalidPoint3:         
                         ClearRow    09
                         MOV         DL,09
                         MOV         DH,09
                         SetCursor   DL,DH,0
                         ReadCharE ;AL has the char
        CheckIfNotNumber3:
                        CMP AL,48
                        JB InvalidPoint3
                        CMP AL,57
                        JA InvalidPoint3
         MOV BL,10
         MUL BL 
         MOV BL,AL      ;BL = Digit*10   (2nd digit)
        ;Read first digit
        InvalidPoint4:         
                         MOV         DL,10
                         MOV         DH,09
                         SetCursor   DL,DH,0
                         ReadCharE
        CheckIfNotNumber4:
                        CMP AL,48
                        JB InvalidPoint3
                        CMP AL,57
                        JA InvalidPoint3
        CheckIfEnter2:
        ReadChar
        CMP AL,13
        JNZ CheckIfEnter2
RET
GetP2Data ENDP


;______________________________________________________________________________________________________________________________________________________________________________
                                                                    ;InitialScreen
;______________________________________________________________________________________________________________________________________________________________________________

    InitialScreen PROC
                         ClearScreen
                         CALL        AskForP1Data
                         CALL        GetP1Data
                         CALL        AskForP2Data
                         CALL        GetP2Data
        RET
    InitialScreen ENDP

;______________________________________________________________________________________________________________________________________________________________________________
                                                                            ;MainScreen;
;______________________________________________________________________________________________________________________________________________________________________________

MainScreen PROC
                  ClearScreen                    ;Printing options
                  SetCursor   09,06,0
                  ShowMessage MainMSG1           ;chatting option MSG
                  SetCursor   09,08,0
                  ShowMessage MainMSG2           ;game option MSG
                  SetCursor   09,10,0
                  ShowMessage MainMSG3           ;exit option MSG
                  SetCursor   0,22,0
                  MOV         CX,80
    DrawDashLine:                                ;Draw status bar
                  ShowChar    '-'
                  LOOP        DrawDashLine
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ReadAgain:    
                  CMP         CurrentPlayer,1       ;print current player and toggle
                  JNE         player2Choice
                  PUSHF
                  MOV         CurrentPlayer,2
                  SetCursor   09,04,0
                  ShowMessage player1
                  POPF
                  JE          Read
    player2Choice:                                  
                  MOV         CurrentPlayer,1
                  SetCursor   09,04,0
                  ShowMessage player2
    Read:                                           ;read the pressed key
                  ReadKey
                  CMP         AH,3BH                ;if f1 pressed
                  JE          ChatInvite
                  CMP         AH,3CH                ;if f2 pressed
                  JE          GameInvite
                  CMP         AL,1BH                ;if ESC pressed
                  JNE        ignore
                  JMP FAR PTR CloseProgram
                  ignore:
                  JMP         ReadAgain
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ChatInvite: 
                  ClearRow 23
                  SetCursor   0,23,0                ;else send an invition to other player  
                  MOV         GameInvition,0                                   
                  CMP         ChatInvition,1        ;if their is a Chat Invition jump to chat mode
                  JE          toChatMode
                  CMP CurrentPlayer,2
                  JNE printPlayer2Name
                  ShowMessage Player1
                  ShowChar ' '
                  JE printC
    printPlayer2Name:
                  ShowMessage player2 
                  ShowChar ' '            
    printC:
                  ShowMessage ChatInvition+1
                  MOV         ChatInvition,1 
                  JMP         ReadAgain
    toChatMode:
                jmp ChatMode 
    GameInvite: 
                  MOV ChatInvition,0
                  CMP         GameInvition,1        ;if their is a game Invition jump to chat mode
                  JE          GameMode
                  ClearRow 23  
                  SetCursor   0,23,0               
                  CMP CurrentPlayer,2               ;else send an invition to other player
                  JNE printPlayer2Name2 
                  ShowMessage Player1
                  ShowChar ' '
                  JE printG
    printPlayer2Name2: 
                  ShowMessage Player2
                  ShowChar ' '
    printG:              
                  ShowMessage GameInvition+1
                  MOV         GameInvition,1
                  JMP         ReadAgain
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ChatMode:     
                  ClearScreen
                  ShowMessage ChatScreen
                  jmp         endproc
    CloseProgram: 
                  ClearScreen
                  MOV         AH, 4CH
                  MOV         AL, 01
                  INT         21H
                  jmp         endproc
    GameMode:     
                  ClearScreen
                  ShowMessage GameScreen
                  jmp         endproc
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    endproc:      
                  RET
MainScreen ENDP

    ;_________________________________________________________________________Main PROC_____________________________________________________________________________________________________


MAIN PROC FAR
                         MOV         AX, @DATA
                         MOV         DS, AX

                        CALL InitialScreen
                        CALL MainScreen 

RET
MAIN ENDP
END MAIN