TITLE Bitmap Encryption     (bitmapEncrypt.asm)

; Program Description:
;       Encrypt an image and decrypt it using a key. The type of
;       images supported are the following:
;           - Bitmap (.bmp)
;
;
; Date Created:    3  January, 2006
; Last Modified:   17 January, 2006


;.686
;.MODEL FLAT, STDCALL
;.STACK
; Constants & Symbols


INCLUDE Irvine32.inc
;; Program Data Storage
.DATA
; Messages:
PromptStart     BYTE        "-  Please Select one of the following:", 0dh, 0AH,
                            "-    1.  Encrypt an Image.", 0dh, 0AH,
                            "-    2.  Decrypt an Image.", 0dh, 0AH,
                            "-    0.  Exit program.", 0dh,2 DUP(0AH),
                            "-  NOTE: Must be *.bmp", 0dh, 3 DUP(0AH),
                            "-  > Your Choice: ", 0h
PromptInput     BYTE        "-  -> Please enter:  Input Image Name: ", 0h
PromptOutput    BYTE        "-  -> Please enter: Output Image Name: ", 0h

PromptProgress  BYTE        "-  --> In progress... ", 0h
PromptComplete  BYTE        "Complete! ", 0dh, 0Ah,
                            "-    ** Press any key to run again....", 0h
PromptErrorR    BYTE        0Dh, 0Ah,"-    Error: Could not read file. ", 0Dh, 0Ah,
                            "-       ** Press any key to run again....", 0h
PromptErrorW    BYTE        0Dh, 0Ah,"-    Error: Could not write file. ", 0Dh, 0Ah,
                            "-       ** Press any key to run again....", 0h

; Variables:
Body            Db          4999999 Dup(0)      ; 9.0 mb
Input           Db          16 Dup(0)
Col1            Db          4 Dup(0)
Col2            Db          4 Dup(0)
Row             Db          4 Dup(?)
Key             Db          "Masdesign", 171 Dup(0)
Header          Db          54 Dup(0)
INFILE          Db          32 Dup(0)
OUTFILE         Db          32 Dup(0)
NobytesWrite    DD          ?
Filehandle      DD          ?
Bytecount       DD          ?
Smat            DB          01h, 02h, 04h, 08h,
                            10h, 20h, 40h, 80h,
                            1bh, 36h, 30 DUP(0)
bufsize         =           SizeOf Body


;; Start of Program Code
.CODE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Procedure Name: MAIN
; What it do: Starts the program.
; Input: =
; Output: =
main PROC

Start:
; Draw a box around the program
    call clrscr
    call DrawBox

; Display Propmt01 Message
    lea  EDX, PromptStart
    call WriteString

    Choice:
    ; Reading the user choice
        xor  EAX, EAX
        call ReadChar
    ; Exit Program
    ChooseNone:
        cmp  EAX, '0'
        jb   Choice
    ; Start of Choose0
    Choose0:
        cmp  EAX, '0'
        ja   Choose1
    jmp  EndProgram
    ; // END OF Choose0
lea  EDX, PromptInput
    ; Start of Choose1
    Choose1:
        cmp  EAX, '1'
        ja   Choose2
        call WriteChar
        call crlf

        ;Maximum file length
        mov  ECX, 128

        lea  EDX, PromptInput
        call WriteString
        ; Hook-up the variable for the image name.
        lea  EDX, INFILE
        call ReadString

        ;call readProc

        ;Set the values of the header and body to the buffer
        ;call DividHeadBody
        ;//

        lea  EDX, PromptOutput
        call WriteString
        ; Hook-up the variable for the image name.
        lea  EDX, OUTFILE
        call ReadString

        lea  EDX, PromptProgress
        call WriteString

        ;lea  ESI, buffer
        call Cipher
        ;Get the values of the header and body to the buffer
        ;call MergeHeadBody
        ;//

        ;call waitmsg
        ;call writeProc
        lea  EDX, PromptComplete
        call WriteString

        call ReadChar
    jmp  Start
    ; // END OF Choose 1

; Start of Choose2
Choose2:
        lea  EDX, PromptStart
        cmp  EAX, '2'
        ja   Choice
        call WriteChar
        call crlf

        ;Maximum file length
        mov  ECX, 128

        lea  EDX, PromptInput
        call WriteString
        ; Hook-up the variable for the image name.
        lea  EDX, INFILE
        call ReadString

        ;call readProc

        ;Set the values of the header and body to the buffer
        ;call DividHeadBody
        ;//

        lea  EDX, PromptOutput
        call WriteString
        ; Hook-up the variable for the image name.
        lea  EDX, OUTFILE
        call ReadString

        lea  EDX, PromptProgress
        call WriteString

        ;lea  ESI, body
        call InvCipher
        ;Get the values of the header and body to the buffer
        ;call MergeHeadBody
        ;//

        ;call waitmsg
        ;call writeProc
        lea  EDX, PromptComplete
        call WriteString

        call ReadChar
    jmp  Start
; // END OF Choose 2

Quit::
    call ReadChar
    jmp  Start
EndProgram:

main ENDP

;;;;;;;;;;;;;;; EXTERNAL PROCEDURES ;;;;;;;;;;;;;;;

;--------------------------------------------------
; DrawBox:  Draw a colored box in the DOS-WINDOW.
; Receives:  none
; Returns:   none
;--------------------------------------------------
DrawBox PROC USES EDX EAX
    mov  EAX, 0FFFFFFFFh
    call SetTextColor
    xor  EDX, EDX

DrawTopOut:
    xor  DL, DL
    DrawTopIn:
        call GoToXY
        mov  EAX, "-"
        call WriteChar
        cmp  DL, 70
        inc  DL
        jbe  DrawTopIn
    cmp  DH, 2
    inc  DH
    jbe  DrawTopOut

    xor  DL, DL
    mov  DH, 20
DrawBottom:
        call GoToXY
        mov  EAX, "-"
        call WriteChar
        cmp  DL, 70
        inc  DL
        jbe  DrawBottom

    xor  EDX, EDX
DrawRight:
        call GoToXY
        mov  EAX, "-"
        call WriteChar
        cmp  DH, 20
        inc  DH
        jbe  DrawRight

    mov  DL, 70
    xor  DH, DH
DrawLeft:
        call GoToXY
        mov  EAX, "-"
        call WriteChar
        cmp  DH, 20
        inc  DH
        jbe  DrawLeft

    ;//Writes the program title
    mov  DH, 1
    mov  DL, 25
    call GoToXY
    lea  EDX, ProgramTitle
    call WriteString

    ;//Start from here
    xor  DL, DL
    mov  DH, 4
    call GoToXY
    call crlf
    ret
ProgramTitle    BYTE        "Project Image Encryption", 0h
DrawBox ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++


;--------------------------------------------------
; Copy16Block Procedure
; Copy one Col1 to another.
; Receives: [ESI] = Array 1
; Returns:  [EDI] = Array2
;--------------------------------------------------
Copy16Block PROC USES EAX ECX ESI EDI
    mov ECX, 4
LoopCopy:
        mov  AL, BYTE PTR [ESI]
        mov  BYTE PTR [EDI], AL
        inc  EDI
        inc  ESI
    loop LoopCopy
    ret
Copy16Block ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++

;--------------------------------------------------
; writeProc Procedure
; Writes to a File.
; Receives: nothing
; Returns: nothing
;--------------------------------------------------
writefileProc PROC uses EAX EDX
    pushad
        INVOKE CreateFile,
	  ADDR OUTFILE, GENERIC_WRITE, DO_NOT_SHARE, NULL,
	  CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0

	mov fileHandle,eax		; save file handle
	.IF eax == INVALID_HANDLE_VALUE
	  mov  edx,OFFSET PromptErrorW		; Display error message
	  call WriteString
	  jmp  Quit
	.ENDIF

	INVOKE WriteFile,		; write text to file
	    fileHandle,		; file handle
	    ADDR body,		; buffer pointer
	    NobytesWrite,		; number of bytes to write
	    ADDR byteCount,		; number of bytes written
	    0		; overlapped execution flag

	INVOKE CloseHandle, fileHandle

    popad
    ret
writefileProc ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++

;--------------------------------------------------
; readProc Procedure
; Reads from a File.
; Receives: nothing
; Returns: nothing
;--------------------------------------------------
readfileProc PROC uses EAX EDX
    pushad
        INVOKE CreateFile,
	  ADDR INFILE, GENERIC_READ, DO_NOT_SHARE, NULL,
	  OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0

	mov fileHandle,eax		; save file handle
	.IF eax == INVALID_HANDLE_VALUE
	  mov  edx,OFFSET PromptErrorR		; Display error message
	  call WriteString
	  jmp  Quit
	.ENDIF

	INVOKE ReadFile,		; write text to file
	    fileHandle,    		; file handle
	    ADDR body,		; Body pointer
	    bufSize,	      	; number of bytes to write
	    ADDR byteCount,		; number of bytes written
	    0		; overlapped execution flag
	INVOKE CloseHandle, fileHandle

	mov esi,byteCount		; insert null terminator
	mov body[esi],0		; into buffer
	mov NobytesWrite, esi

    popad


        ret
readfileProc ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++



;//END OF;;;;;; EXTERNAL PROCEDURES ;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;; CIPHER PROCEDURES ;;;;;;;;;;;;;;;;
;---------------------------------------------------
; Cipher Procedure:
;       Encryting the image.
; Receives: ESI - Address of Body Array
; Returns:
;---------------------------------------------------
Cipher PROC
        xor ESI, ESI
        call READfilePROC

        lea EDI, INPUT
        mov ECX, NobytesWrite
        shr ECX, 4
        mov ESI,54

For_Out:
        push ECX

        pushad
        mov  ECX, 16
        LoopCopy:
            mov  AL, BYTE PTR body[ESI+ECX-1]
            mov  INPUT[ECX-1], AL
            loop LoopCopy
        popad


        push ESI
        mov ESI,0
        call AddRoundKey

        mov ECX, 9
For_In: inc ESI
        call SubBytes
        call ShiftRows
        call MixColumn1s
        call AddRoundKey
        Loop For_In

        call SubBytes
        call ShiftRows
        mov ESI, 10
        call AddRoundKey
        pop ESI


        ; Copy the value of the 16 block input to the body
        pushad
        mov  ECX, 16
        LoopWrite:
            mov  AL, INPUT[ECX-1]
            mov  BYTE PTR body[ESI+ECX-1], AL
            loop LoopWrite
        popad
        ; End of the copying

        add ESI,16
        pop ECX
        LOOP For_Out

        call writefilePROC

        ret
Cipher ENDP
;+++++++++++++++++++++++++++++++++++++++++++++++++++

;---------------------------------------------------
; InvCipher: The inverse of cipher.
; Receives: a BMP file
; Returns:  an encypted version.
;---------------------------------------------------
InvCipher PROC

        xor ESI, ESI
        call READfilePROC

        lea EDI, INPUT
        mov ECX, NobytesWrite
        shr ECX, 4
        mov ESI,54

For_Out:
        push ECX
        
        ; Copy 16 bytes from the body to the input 16 block
        pushad
        mov  ECX, 16
        LoopCopy:
            mov  AL, BYTE PTR body[ESI+ECX-1]
            mov  INPUT[ECX-1], AL
            loop LoopCopy
        popad
        ; End of the copying

        push ESI

        mov ESI, 10
        call AddRoundKey

        mov ECX, 9
For_In: dec ESI
        call INVShiftRows
        call INVSubBytes
        call AddRoundKey
        call INVMixColumn1s
        Loop For_In

        call INVShiftRows
        call INVSubBytes
        mov ESI, 0
        call AddRoundKey
        pop ESI

        ; Copy the value of the 16 block input to the body
        pushad
        mov  ECX, 16
        LoopWrite:
            mov  AL, INPUT[ECX-1]
            mov  BYTE PTR body[ESI+ECX-1], AL
            loop LoopWrite
        popad
        ; End of the copying

        add ESI,16
        pop ECX
        LOOP For_Out

        call writefilePROC

        ret
InvCipher ENDP
;+++++++++++++++++++++++++++++++++++++++++++++++++++


;----------------------------------------------------------
; KeyExpansion Procedure
; Takes the cipher key, and expands on it, generating a new key.
; This follows the routine as described in the AES.
; Receives:
; Returns:
;----------------------------------------------------------
KeyExpansion PROC
;KeyExpansion(byte key[4*Nk], word w[Nb*(Nr+1)], Nk)
;begin
;word temp
;i = 0
;while (i < Nk)
    ;w[i] = word(key[4*i], key[4*i+1], key[4*i+2], key[4*i+3])
    ;i = i+1
;end while
         pushad
         xor ebp, ebp
         xor EBX, EBX
         mov EDX, 10
RoundKey0:
         push EBX
         lea ESI, KEY[ebp]
         add ESI, 12
         lea EDI, Col1
         call Copy16Block

         mov ECX,8
RoundKey1:
         push ECX
         mov ECX, 4
         mov ESI, 3
         clc

;temp = w[i-1]
;if (i mod Nk = 0)
;temp = SubWord(RotWord(temp)) xor Rcon[i/Nk]
;else if (Nk > 6 and i mod Nk = 4)
;temp = SubWord(temp)
;end if
;w[i] = w[i-Nk] xor temp
;i = i + 1
;end while
RoundKey2:rcl Col1[ESI], 1
         dec ESI
         Loop RoundKey2
         adc COL1[3], 0
         pop ECX
         Loop RoundKey1

         mov ECX, 4
         lea ESI, Col1
RoundKey3:
         lea EDI, SBox
         xor EAX,EAX
         mov AL, BYTE PTR [ESI]
         add EDI, EAX
         mov bl, BYTE PTR [EDI]
         mov [ESI], bl
         inc ESI
         Loop RoundKey3

         lea ESI, KEY[ebp]
         lea EDI, COL2
         call Copy16Block

         lea ESI, Col1
         lea EDI, Col1
         mov ECX ,4
RoundKey4:    
         mov AL,BYTE PTR[ESI]
         xor BYTE PTR[EDI], AL
         inc ESI
         inc EDI
         loop RoundKey4

         pop EBX
         lea ESI,Smat[EBX]
         push EBX
         lea EDI,Col2
         mov AL,BYTE PTR [ESI]
         xor BYTE PTR[EDI],al

         lea ESI, Col2
         lea EDI, KEY[ebp+16]
         call Copy16Block

         mov ECX,3
         lea EAX, KEY[ebp+4]
         lea EBX, KEY[ebp+16]

RoundKey5:    mov ESI, EBX
         lea EDI, COL1
         call Copy16Block

         mov ESI, EAX
         lea EDI, COL1
         push ECX
         push EAX

         mov ECX ,4
RoundKey6: 
         mov AL,BYTE PTR[ESI]
         xor BYTE PTR[EDI], AL
         inc ESI
         inc EDI
         loop RoundKey6

         pop EAX
         pop ECX
         lea ESI, COL1
         mov EDI, EBX
         add EDI, 4
         call Copy16Block

         add EAX, 4
         add EBX, 4
         Loop RoundKey5

         pop EBX
         inc EBX
         add ebp, 16
         dec EDX
         cmp EDX,0
         JNE RoundKey0
         popad
         ret
KeyExpansion ENDP
;+++++++++++++++++++++++++++++++++++++++++++++++++++


;---------------------------------------------------
; AddRoundKey:  Transformation in the Cipher and
;               Inverse Cipher in which a Round
;               Key is added to the State using an
;               XOR operation. The length of a Round
;               Key equals the size of the State
;               (i.e., for Nb = 4, the Round Key
;               length equals 128 bits/16 bytes).
; Receives:  none
; Returns:   none
;---------------------------------------------------
AddRoundKey PROC
         pushad
         imul ESI, 16
         lea EDI, INPUT
         mov ECX,16
RKL:     mov AL, BYTE PTR Key[ESI]
         xor BYTE PTR [EDI], AL
         inc ESI
         inc EDI
         Loop RKL
         popad
         ret
AddRoundKey ENDP
;+++++++++++++++++++++++++++++++++++++++++++++++++++

;----------------------------------------------------------
; ShiftRows Procedure
; Takes the result of SubBytes PROC and shifts the 2nd, 3rd
; and 4th row (row1, row2 & row3) once, twice and thrice
; respectively.
; Receives:
; Returns:
;----------------------------------------------------------
ShiftRows PROC
         pushad
         xor EAX, EAX
         mov ESI, 1
         xor EDI, EDI
         mov ECX, 4
ROW1:    mov AL,BYTE PTR INPUT[ESI]
         mov BYTE PTR ROW[EDI],al
         add ESI,4
         inc EDI
         Loop ROW1

         mov ECX,8
ROW11:   push ECX
         mov ECX, 4
         mov ESI, 3
         clc
ROW12:   rcl ROW[ESI], 1
         dec ESI
         Loop ROW12
         adc ROW[3], 0
         pop ECX
         Loop ROW11

         xor EAX, EAX
         mov ESI, 1
         xor EDI, EDI
         mov ECX, 4
ROW13:   mov AL,BYTE PTR ROW[EDI]
         mov BYTE PTR INPUT[ESI],al
         add ESI,4
         inc EDI
         Loop ROW13

         xor EAX, EAX
         mov ESI, 2
         xor EDI, EDI
         mov ECX, 4
ROW2:    mov AL,BYTE PTR INPUT[ESI]
         mov BYTE PTR ROW[EDI],al
         add ESI,4
         inc EDI
         Loop ROW2

         mov ECX,16
ROW21:   push ECX
         mov ECX, 4
         mov ESI, 3
         clc
ROW22:   rcl ROW[ESI], 1
         dec ESI
         Loop ROW22
         adc ROW[3], 0
         pop ECX
         Loop ROW21

         xor EAX, EAX
         mov ESI, 2
         xor EDI, EDI
         mov ECX, 4
ROW23:   mov AL,BYTE PTR ROW[EDI]
         mov BYTE PTR INPUT[ESI],al
         add ESI,4
         inc EDI
         Loop ROW23

         xor EAX, EAX
         mov ESI, 3
         xor EDI, EDI
         mov ECX, 4
ROW3:    mov AL,BYTE PTR INPUT[ESI]
         mov BYTE PTR ROW[EDI],al
         add ESI,4
         inc EDI
         Loop ROW3

         mov ECX,24
ROW31:   push ECX
         mov ECX, 4
         mov ESI, 3
         clc
ROW32:   rcl ROW[ESI], 1
         dec ESI
         Loop ROW32
         adc ROW[3], 0
         pop ECX
         Loop ROW31

         xor EAX, EAX
         mov ESI, 3
         xor EDI, EDI
         mov ECX, 4
ROW33:   mov AL,BYTE PTR ROW[EDI]
         mov BYTE PTR INPUT[ESI],al
         add ESI,4
         inc EDI
         Loop ROW33
         popad
         ret
ShiftRows ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++


;----------------------------------------------------------
; InvShiftRows Procedure
; Return the Rows as they were before ShiftRows PROC
; Receives:
; Returns:
;----------------------------------------------------------
InvShiftRows PROC
         pushad
         xor EAX, EAX
         mov ESI, 1
         xor EDI, EDI
         mov ECX, 4
ROW1:    mov AL,BYTE PTR INPUT[ESI]
         mov BYTE PTR ROW[EDI],al
         add ESI,4
         inc EDI
         Loop ROW1

         mov ECX,8
ROW11:   push ECX
         mov ECX, 4
         mov ESI, 0
         clc
ROW12:   rcr ROW[ESI], 1
         inc ESI
         Loop ROW12
         JNC Skip1
         add ROW[0], 80h
Skip1:   pop ECX
         Loop ROW11

         xor EAX, EAX
         mov ESI, 1
         xor EDI, EDI
         mov ECX, 4
ROW13:   mov AL,BYTE PTR ROW[EDI]
         mov BYTE PTR INPUT[ESI],al
         add ESI,4
         inc EDI
         Loop ROW13

         xor EAX, EAX
         mov ESI, 2
         xor EDI, EDI
         mov ECX, 4
ROW2:    mov AL,BYTE PTR INPUT[ESI]
         mov BYTE PTR ROW[EDI],al
         add ESI,4
         inc EDI
         Loop ROW2

         mov ECX,16
ROW21:   push ECX
         mov ECX, 4
         mov ESI, 0
         clc
ROW22:   rcr ROW[ESI], 1
         inc ESI
         Loop ROW22
         JNC Skip2
         add ROW[0], 80h
Skip2:   pop ECX
         Loop ROW21

         xor EAX, EAX
         mov ESI, 2
         xor EDI, EDI
         mov ECX, 4
ROW23:   mov AL,BYTE PTR ROW[EDI]
         mov BYTE PTR INPUT[ESI],al
         add ESI,4
         inc EDI
         Loop ROW23

         xor EAX, EAX
         mov ESI, 3
         xor EDI, EDI
         mov ECX, 4
ROW3:    mov AL,BYTE PTR INPUT[ESI]
         mov BYTE PTR ROW[EDI],al
         add ESI,4
         inc EDI
         Loop ROW3

         mov ECX,24
ROW31:   push ECX
         mov ECX, 4
         mov ESI, 0
         clc
ROW32:   rcr ROW[ESI], 1
         inc ESI
         Loop ROW32
         JNC Skip3
         add ROW[0], 80h
Skip3:   pop ECX
         Loop ROW31

         xor EAX, EAX
         mov ESI, 3
         xor EDI, EDI
         mov ECX, 4
ROW33:   mov AL,BYTE PTR ROW[EDI]
         mov BYTE PTR INPUT[ESI],al
         add ESI,4
         inc EDI
         Loop ROW33
         popad
         ret
InvShiftRows ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++

;----------------------------------------------------------
; SubBytes Procedure
;
; Receives: ESI = Address of the Array
; Returns:  [ESI]16 bytes = After SubBytes is applied
;----------------------------------------------------------
SubBytes PROC
         pushad
         lea EBX, SBox  ; XLAT recieves EBX(The Array) and AL(The Index)
         lea ESI, Input
         mov ECX, 16
         mov EDX, 0
TIFA:    mov AL, [ESI]  ; AL is the index of the byte in Sbox
         xlat           ; AL now will contain the contents of SBox at the previous value of AL
         mov [ESI], AL  ; Substitute
         inc EDX
         loop TIFA
         popad
         ret
;------------------------------------------------------------------------------------------------------------------------
;Table Name/Index   0     1     2     3     4     5     6     7     8     9     A     B     C     D     E     F         -
;------------------------------------------------------------------------------------------------------------------------
    Sbox       DB 063h, 07ch, 077h, 07bh, 0f2h, 06bh, 06fh, 0c5h, 030h, 001h, 067h, 02bh, 0feh, 0d7h, 0abh, 076h;   0   -
               DB 0cah, 082h, 0c9h, 07dh, 0fah, 059h, 047h, 0f0h, 0adh, 0d4h, 0a2h, 0afh, 09ch, 0a4h, 072h, 0c0h;   1   -
               DB 0b7h, 0fdh, 093h, 026h, 036h, 03fh, 0f7h, 0cch, 034h, 0a5h, 0e5h, 0f1h, 071h, 0d8h, 031h, 015h;   2   -
               DB 004h, 0c7h, 023h, 0c3h, 018h, 096h, 005h, 09ah, 007h, 012h, 080h, 0e2h, 0ebh, 027h, 0b2h, 075h;   3   -
               DB 009h, 083h, 02ch, 01ah, 01bh, 06eh, 05ah, 0a0h, 052h, 03bh, 0d6h, 0b3h, 029h, 0e3h, 02fh, 084h;   4   -
               DB 053h, 0d1h, 000h, 0edh, 020h, 0fch, 0b1h, 05bh, 06ah, 0cbh, 0beh, 039h, 04ah, 04ch, 058h, 0cfh;   5   -
               DB 0d0h, 0efh, 0aah, 0fbh, 043h, 04dh, 033h, 085h, 045h, 0f9h, 002h, 07fh, 050h, 03ch, 09fh, 0a8h;   6   -
               DB 051h, 0a3h, 040h, 08fh, 092h, 09dh, 038h, 0f5h, 0bch, 0b6h, 0dah, 021h, 010h, 0ffh, 0f3h, 0d2h;   7   -
               DB 0cdh, 00ch, 013h, 0ech, 05fh, 097h, 044h, 017h, 0c4h, 0a7h, 07eh, 03dh, 064h, 05dh, 019h, 073h;   8   -
               DB 060h, 081h, 04fh, 0dch, 022h, 02ah, 090h, 088h, 046h, 0eeh, 0b8h, 014h, 0deh, 05eh, 00bh, 0dbh;   9   -
               DB 0e0h, 032h, 03ah, 00ah, 049h, 006h, 024h, 05ch, 0c2h, 0d3h, 0ach, 062h, 091h, 095h, 0e4h, 079h;   A   -
               DB 0e7h, 0c8h, 037h, 06dh, 08dh, 0d5h, 04eh, 0a9h, 06ch, 056h, 0f4h, 0eah, 065h, 07ah, 0aeh, 008h;   B   -
               DB 0bah, 078h, 025h, 02eh, 01ch, 0a6h, 0b4h, 0c6h, 0e8h, 0ddh, 074h, 01fh, 04bh, 0bdh, 08bh, 08ah;   C   -
               DB 070h, 03eh, 0b5h, 066h, 048h, 003h, 0f6h, 00eh, 061h, 035h, 057h, 0b9h, 086h, 0c1h, 01dh, 09eh;   D   -
               DB 0e1h, 0f8h, 098h, 011h, 069h, 0d9h, 08eh, 094h, 09bh, 01eh, 087h, 0e9h, 0ceh, 055h, 028h, 0dfh;   E   -
               DB 08ch, 0a1h, 089h, 00dh, 0bfh, 0e6h, 042h, 068h, 041h, 099h, 02dh, 00fh, 0b0h, 054h, 0bbh, 016h;   F   -
;------------------------------------------------------------------------------------------------------------------------

SubBytes ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++


;----------------------------------------------------------
; InvSubBytes Procedure
;
; Receives:
; Returns:
;----------------------------------------------------------
InvSubBytes PROC
         pushad
         lea EBX, InvSBox  ; XLAT recieves EBX(The Array) and AL(The Index)
         lea ESI, Input
         mov ECX, 16
         mov EDX, 0
TIFA:    mov AL, [ESI]  ; AL is the index of the byte in InvSbox
         xlat           ; AL now will contain the contents of InvSBox at the previous value of AL
         mov [ESI], AL  ; Substitute
         inc EDX
         loop TIFA
         popad
         ret

;------------------------------------------------------------------------------------------------------------------------
;Table Name/Index   0     1     2     3     4     5     6     7     8     9     A     B     C     D     E     F         -
;------------------------------------------------------------------------------------------------------------------------
    InvSBox     DB 052h, 009h, 06ah, 0d5h, 030h, 036h, 0a5h, 038h, 0bfh, 040h, 0a3h, 09eh, 081h, 0f3h, 0d7h, 0fbh;  0   -
                DB 07ch, 0e3h, 039h, 082h, 09bh, 02fh, 0ffh, 087h, 034h, 08eh, 043h, 044h, 0c4h, 0deh, 0e9h, 0cbh;  1   -
                DB 054h, 07bh, 094h, 032h, 0a6h, 0c2h, 023h, 03dh, 0eeh, 04ch, 095h, 00bh, 042h, 0fah, 0c3h, 04eh;  2   -
                DB 008h, 02eh, 0a1h, 066h, 028h, 0d9h, 024h, 0b2h, 076h, 05bh, 0a2h, 049h, 06dh, 08bh, 0d1h, 025h;  3   -
                DB 072h, 0f8h, 0f6h, 064h, 086h, 068h, 098h, 016h, 0d4h, 0a4h, 05ch, 0cch, 05dh, 065h, 0b6h, 092h;  4   -
                DB 06ch, 070h, 048h, 050h, 0fdh, 0edh, 0b9h, 0dah, 05eh, 015h, 046h, 057h, 0a7h, 08dh, 09dh, 084h;  5   -
                DB 090h, 0d8h, 0abh, 000h, 08ch, 0bch, 0d3h, 00ah, 0f7h, 0e4h, 058h, 005h, 0b8h, 0b3h, 045h, 006h;  6   -
                DB 0d0h, 02ch, 01eh, 08fh, 0cah, 03fh, 00fh, 002h, 0c1h, 0afh, 0bdh, 003h, 001h, 013h, 08ah, 06bh;  7   -
                DB 03ah, 091h, 011h, 041h, 04fh, 067h, 0dch, 0eah, 097h, 0f2h, 0cfh, 0ceh, 0f0h, 0b4h, 0e6h, 073h;  8   -
                DB 096h, 0ach, 074h, 022h, 0e7h, 0adh, 035h, 085h, 0e2h, 0f9h, 037h, 0e8h, 01ch, 075h, 0dfh, 06eh;  9   -
                DB 047h, 0f1h, 01ah, 071h, 01dh, 029h, 0c5h, 089h, 06fh, 0b7h, 062h, 00eh, 0aah, 018h, 0beh, 01bh;  A   -
                DB 0fch, 056h, 03eh, 04bh, 0c6h, 0d2h, 079h, 020h, 09ah, 0dbh, 0c0h, 0feh, 078h, 0cdh, 05ah, 0f4h;  B   -
                DB 01fh, 0ddh, 0a8h, 033h, 088h, 007h, 0c7h, 031h, 0b1h, 012h, 010h, 059h, 027h, 080h, 0ech, 05fh;  C   -
                DB 060h, 051h, 07fh, 0a9h, 019h, 0b5h, 04ah, 00dh, 02dh, 0e5h, 07ah, 09fh, 093h, 0c9h, 09ch, 0efh;  D   -
                DB 0a0h, 0e0h, 03bh, 04dh, 0aeh, 02ah, 0f5h, 0b0h, 0c8h, 0ebh, 0bbh, 03ch, 083h, 053h, 099h, 061h;  E   -
                DB 017h, 02bh, 004h, 07eh, 0bah, 077h, 0d6h, 026h, 0e1h, 069h, 014h, 063h, 055h, 021h, 00ch, 07dh;  F   -
;------------------------------------------------------------------------------------------------------------------------

InvSubBytes ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++


;----------------------------------------------------------
; MixColumn1s Procedure
; Takes the result of ShiftRows and applies a series of
; GF multiplication and xor operation, to each Col1
; seperately as described in the AES
; Receives:
; Returns:
;----------------------------------------------------------
MixColumn1s PROC
         pushad
         mov ECX,4
         lea ESI, INPUT
         xor ebp, ebp
outerloop:
         lea EDI, COL1
         call Copy16Block

         xor EAX,EAX
         lea EDX,COL2
         lea EDI, COL1
         mov AL, BYTE PTR [EDI]
         call GFmultBy02
         mov bl, AL
         mov AL, BYTE PTR [EDI+1]
         mov bh, AL
         call GFmultBy02
         xor AL, bh
         xor AL, bl
         xor AL, BYTE PTR [EDI+2]
         xor AL, BYTE PTR [EDI+3]
         mov BYTE PTR [EDX], AL

         lea EDI,COL1
         mov AL, BYTE PTR [EDI+1]
         call GFmultBy02
         mov bl, AL
         mov AL, BYTE PTR [EDI+2]
         mov bh, AL
         call GFmultBy02
         xor AL, bh
         xor AL, bl
         xor AL, BYTE PTR [EDI]
         xor AL, BYTE PTR [EDI +3]
         mov BYTE PTR [EDX+1], AL

         lea EDI, COL1
         mov AL,BYTE PTR [EDI+2]
         call GFmultBy02
         mov bl,al
         mov AL,BYTE PTR[EDI+3]
         mov bh,al
         call GFmultBy02
         xor AL,bh
         xor AL,bl
         xor AL,BYTE PTR [EDI]
         xor AL, BYTE PTR [EDI+1]
         mov BYTE PTR [EDX+2],al

         lea EDI,COL1
         mov AL,BYTE PTR[EDI]
         mov bl,al
         call GFmultBy02
         xor bl,al
         mov AL,BYTE PTR[EDI+3]
         call GFmultBy02
         xor AL,bl
         xor AL,BYTE PTR[EDI+1]
         xor AL,BYTE PTR[EDI+2]
         mov BYTE PTR [EDX+3],al


         push ESI
         push ECX
         xor ESI, ESI
         mov ECX, 4
INNER:    mov AL, BYTE PTR COL2[ESI]
         mov BYTE PTR INPUT[ebp], AL
         inc ebp
         inc ESI
         Loop INNER
         pop ECX
         pop ESI

         add ESI,4

         dec ECX
         cmp ECX,0
         jne outerloop
         popad
         ret
MixColumn1s ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++

;----------------------------------------------------------
; InvMixColumn1s Procedure
; Performs the inverse of MixColumn1s by applieng a series
; of GF multiplication and xor operation that produces the
; original Col1 contents, Col1 by Col1 seperately,
; as described in AES
; Receives:
; Returns:
;----------------------------------------------------------
InvMixColumn1s PROC
         pushad
         mov ECX,4
         lea ESI, INPUT
         xor ebp, ebp
outerloop:
         lea EDI, COL1
         call Copy16Block

         xor EAX,EAX
         lea EDX,COL2
         push EDX
         lea EDI, COL1
         mov AL, BYTE PTR [EDI]
         call GFmultBy0E
         mov bl, AL
         mov AL, BYTE PTR [EDI+1]
         call GFmultBy0B
         mov bh, AL
         mov AL, BYTE PTR [EDI+2]
         call GFmultBy0D
         mov dl, AL
         mov AL, BYTE PTR [EDI+3]
         call GFmultBy09
         xor AL, dl
         xor AL, bh
         xor AL, bl
         pop EDX
         mov BYTE PTR[EDX], AL

         push EDX
         lea EDI, COL1
         mov AL, BYTE PTR [EDI]
         call GFmultBy09
         mov bl, AL
         mov AL, BYTE PTR [EDI+1]
         call GFmultBy0E
         mov bh, AL
         mov AL, BYTE PTR [EDI+2]
         call GFmultBy0B
         mov dl, AL
         mov AL, BYTE PTR [EDI+3]
         call GFmultBy0D
         xor AL, dl
         xor AL, bh
         xor AL, bl
         pop EDX
         mov BYTE PTR[EDX+1], AL

         push EDX
         lea EDI, COL1
         mov AL, BYTE PTR [EDI]
         call GFmultBy0D
         mov bl, AL
         mov AL, BYTE PTR [EDI+1]
         call GFmultBy09
         mov bh, AL
         mov AL, BYTE PTR [EDI+2]
         call GFmultBy0E
         mov dl, AL
         mov AL, BYTE PTR [EDI+3]
         call GFmultBy0B
         xor AL, dl
         xor AL, bh
         xor AL, bl
         pop EDX
         mov BYTE PTR [EDX+2],al

         push EDX
         lea EDI, COL1
         mov AL, BYTE PTR [EDI]
         call GFmultBy0B
         mov bl, AL
         mov AL, BYTE PTR [EDI+1]
         call GFmultBy0D
         mov bh, AL
         mov AL, BYTE PTR [EDI+2]
         call GFmultBy09
         mov dl, AL
         mov AL, BYTE PTR [EDI+3]
         call GFmultBy0E
         xor AL, dl
         xor AL, bh
         xor AL, bl
         pop EDX
         mov BYTE PTR [EDX+3],al

         push ESI
         push ECX
         xor ESI, ESI
         mov ECX, 4

INNERLOOP:    mov AL, BYTE PTR COL2[ESI]
         mov BYTE PTR INPUT[ebp], AL
         inc ebp
         inc ESI
         Loop INNERLOOP
         pop ECX
         pop ESI

         add ESI,4
         dec ECX
         cmp ECX,0
         JNE outerloop
         popad
         ret
InvMixColumn1s ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++


;----------------------------------------------------------
; GFmultBy02 Procedure
;
; Receives: AL = BYTE
; Returns:  AL = BYTE
;----------------------------------------------------------
GFMultBy02 PROC
         cmp AL,080h
         jae next
         shl AL,1
         jmp Done
next:    shl AL,1
         xor AL,1bh
Done:    ret
GFMultBy02 ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++

;----------------------------------------------------------
; GFmultBy03 Procedure
;
; Receives: AL = BYTE
; Returns:  AL = BYTE
;----------------------------------------------------------
GFmultBy03 PROC uses EBX
        mov  BL, AL
        shl  AL, 2
        XOR  AL, BL
        ret
GFmultBy03 ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++

;----------------------------------------------------------
; GFmultBy09 Procedure
;
; Receives: AL = BYTE
; Returns:  AL = BYTE
;----------------------------------------------------------
GFMultBy09 PROC uses EBX
        mov BL, AL
        call GFMultBy02
        call GFMultBy02
        call GFMultBy02
        xor AL, BL
        ret
GFMultBy09 ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++

;----------------------------------------------------------
; GFmultBy0b Procedure
;
; Receives: AL = BYTE
; Returns:  AL = BYTE
;----------------------------------------------------------
GFMultBy0b PROC uses EBX
         mov BL, AL
         call GFMultBy02
         mov BH, AL
         call GFMultBy02
         call GFMultBy02
         xor AL, BH
         xor AL, BL
         ret
GFMultBy0b ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++

;----------------------------------------------------------
; GFmultBy0d Procedure
;
; Receives: AL = BYTE
; Returns:  AL = BYTE
;----------------------------------------------------------
GFMultBy0d PROC uses EBX
         mov BL, AL
         call GFMultBy02
         call GFMultBy02
         mov BH, AL
         call GFMultBy02
         xor AL, BH
         xor AL, BL
         ret
GFMultBy0d ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++

;----------------------------------------------------------
; GFmultBy0e Procedure
;
; Receives: AL = BYTE
; Returns:  AL = BYTE
;----------------------------------------------------------
GFMultBy0e PROC uses EBX
         call GFMultBy02
         mov BL, AL
         call GFMultBy02
         mov BH, AL
         call GFMultBy02
         xor AL, BH
         xor AL, BL
         ret
GFMultBy0e ENDP
;++++++++++++++++++++++++++++++++++++++++++++++++++

;//END OF;;;;;;; CIPHER PROCEDURES ;;;;;;;;;;;;;;;;
END main
