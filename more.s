*
* more - Text viewer
*

* BEQ - Branch if Equal
*   Branches if the zero flag (Z) is set to 1. This happens if:
*       - A 0 value is loaded into the accumulator via LDA
*       - A CMP is done on two equal values
*       - A SBC is done on two equal values?
* BCC - Branch if Carry Flag is Clear
*   Equivalent to Branch if Less Than (BLT), meaning that the
*   accumulator (A) is less than the parameter: A < Param
* BGE - Branch if Accum >= Param is BCS


* System subroutines stored in ROM
CROUT   equ $fd8e   ; Outputs a carriage return
PRBYTE  equ $fdda   ; Outputs a byte
COUT    equ $fded   ; Outputs a character

* MLI stands for Machine Language Interface
* It is the ProDOS system call entry point
MLI     equ $bf00

* ProDOS system call command codes
OPEN    equ $c8
CLOSE   equ $cc

* Constants
ADDR    equ $06
MAX_EC  equ $5a     ; The largest error code


WRITE_CHAR  mac
            if  #=]1
                lda ]1
            else
                lda #]1
            fin
            jsr COUT
            eom


WRITE_BYTE  mac
            lda ]1
            jsr PRBYTE
            eom


WRITE_ASC   mac
            tya         ; Preserve Y by transferring it to A
            pha         ;  and pushing it onto the stack
            ldy #0      ; Prepare loop index
nextch      lda ]1,Y    ; Load a character into A
            beq finish  ; Branch if lda loaded a zero value
            jsr COUT    ; Write the character in A
            iny         ; Increment Y
            jmp nextch  ; Go back and load the next character
finish      pla         ; Restore Y by pulling stack into A
            tay         ;  and transferring it to Y
            eom


WRITE_STR   mac
            tya         ; Preserve Y
            pha
            lda ]1      ; Length byte
            sta len
            ldy #1      ; Prepare loop index
nextch      cpy len     ; Check if beyond end of string
            bcc writech ; Y is Less than
            beq writech ; Or equal to len
            jmp finish
writech     lda ]1,y    ; Load a character
            jsr COUT
            iny
            jmp nextch
len         db  0
finish      pla         ; Restore Y
            tay
            eom


COPY_BYTE   mac
            lda ]1
            sta ]2
            eom


mainProgram
            org $8000

            lda #<fileName
            sta oPathPtr
            lda #>fileName
            sta oPathPtr+1

            jsr MLI
            db  OPEN
            da  openParams
            bne openErrorHandler
            pmc COPY_BYTE,fileNum;closeNum
            jsr MLI
            db  CLOSE
            dw  closeParams
            bne closeErrorHandler
            jmp endMain
openErrorHandler
            sta errorCode
            pmc WRITE_ASC,openFailureText
            jmp errorHandler
closeErrorHandler
            sta errorCode
            pmc WRITE_ASC,closeFailureText
errorHandler
            pmc WRITE_STR,fileName
            pmc WRITE_CHAR,":"
            pmc WRITE_CHAR," "
            lda errorCode
            cmp #MAX_EC         ; Compare by A - MAX_EC
            bcs unknownError    ; Branch if A >= MAX_EC
            lda errorCode       ; Necessary?
            clc                 ; Prepare for addition
            adc errorCode       ; Double for indexing addresses
            tax
            lda errorMessages,x
            sta ADDR
            inx
            lda errorMessages,x
            sta ADDR+1
            bne writeError
            lda ADDR
            bne writeError
unknownError
            pmc WRITE_ASC,unknownErrorCodeText
            pmc WRITE_CHAR,"$"
            pmc WRITE_BYTE,errorCode
            jsr CROUT
            jmp endMain 
writeError
            pmc WRITE_ASC,(ADDR)
            pmc WRITE_CHAR," "
            pmc WRITE_ASC,errorCodeText
            pmc WRITE_CHAR,"$"
            pmc WRITE_BYTE,errorCode
            pmc WRITE_CHAR,")"
            jsr CROUT
endMain
            rts

* DAT-uh

openParams  db  3           ; Parameter count
oPathPtr    ds  2           ; Input param - file to open
oBufPtr     da  inBuf       ; Input param - I/O buffer
fileNum     ds  1           ; Output param - file ref num

closeParams db  1           ; Parameter count
closeNum    db  0           ; Input param - ref num to close

fileName    db  4
            asc "BLAH"

errorCode        db  0
openFailureText  asc "FAILED TO OPEN FILE ",00
closeFailureText asc "FAILED TO CLOSE FILE ",00
errorCodeText    asc "(ERROR CODE ",00

unknownErrorCodeText asc "UNKNOWN ERROR CODE: ",00
em00    asc "NO ERROR",00
em01    asc "BAD SYSTEM CALL NUMBER",00
em03    asc "NO DEVICE CONNECTED",00    ; Bug in AppleWin < 1.26.3.0
em04    asc "BAD SYSTEM CALL PARAMETER COUNT",00
em25    asc "INTERRUPT VECTOR TABLE FULL",00
em27    asc "I/O ERROR",00
em28    asc "DEVICE NOT CONNECTED",00
em2b    asc "DISK WRITE PROTECTED",00
em2e    asc "DISK SWITCHED WHILE FILE STILL OPEN",00
em40    asc "PATH CONTAINS ILLEGAL CHARACTERS",00
em42    asc "CANNOT EXCEED 8 OPEN FILES",00
em44    asc "PATH NOT FOUND",00
em45    asc "VOLUME DIRECTORY NOT FOUND",00
em46    asc "FILE NOT FOUND",00
em47    asc "DUPLICATE FILENAME",00
em48    asc "DISK IS FULL",00
em49    asc "CANNOT EXCEED 51 DIRECTORY ENTRIES",00
em4a    asc "INCOMPATIBLE FILE FORMAT",00
em4b    asc "UNSUPPORTED STORAGE TYPE",00
em4c    asc "END OF FILE",00
em4d    asc "FILE POSITION OUT OF RANGE",00
em4e    asc "FILE ATTRIBUTE FORBIDS OPERATION",00
em50    asc "FILE IS OPEN",00
em51    asc "DIRECTORY COUNT ERROR",00
em52    asc "NOT A PRODOS DISK",00
em55    asc "VOLUME CONTROL BLOCK FULL",00
em56    asc "BAD BUFFER ADDRESS",00
em57    asc "DUPLICATE VOLUME",00
em5a    asc "BIT MAP DISK ADDRESS IS IMPOSSIBLE",00
*
* ERROR CODE TO MESSAGE TRANSLATION TABLE
*
errorMessages
ec00    da  em00
ec01    da  em01
ec02    da  $0000
ec03    da  em03    ; Bug in AppleWin < 1.26.3.0
ec04    da  em04
ec05    da  $0000
ec06    da  $0000
ec07    da  $0000
ec08    da  $0000
ec09    da  $0000
ec0a    da  $0000
ec0b    da  $0000
ec0c    da  $0000
ec0d    da  $0000
ec0e    da  $0000
ec0f    da  $0000
ec10    da  $0000
ec11    da  $0000
ec12    da  $0000
ec13    da  $0000
ec14    da  $0000
ec15    da  $0000
ec16    da  $0000
ec17    da  $0000
ec18    da  $0000
ec19    da  $0000
ec1a    da  $0000
ec1b    da  $0000
ec1c    da  $0000
ec1d    da  $0000
ec1e    da  $0000
ec1f    da  $0000
ec20    da  $0000
ec21    da  $0000
ec22    da  $0000
ec23    da  $0000
ec24    da  $0000
ec25    da  $0000
ec26    da  $0000
ec27    DA  em27
ec28    DA  em28
ec29    da  $0000
ec2a    da  $0000
ec2b    da  $0000
ec2c    da  $0000
ec2d    da  $0000
ec2e    DA  em2e
ec2f    da  $0000
ec30    da  $0000
ec31    da  $0000
ec32    da  $0000
ec33    da  $0000
ec34    da  $0000
ec35    da  $0000
ec36    da  $0000
ec37    da  $0000
ec38    da  $0000
ec39    da  $0000
ec3a    da  $0000
ec3b    da  $0000
ec3c    da  $0000
ec3d    da  $0000
ec3e    da  $0000
ec3f    da  $0000
ec40    da  em40
ec42    da  em42
ec43    da  $0000
ec44    da  em44
ec45    da  em45
ec46    da  em46
ec47    da  em47
ec48    da  em48
ec49    da  em49
ec4a    da  em4a
ec4b    da  em4b
ec4c    da  em4c
ec4d    da  em4d
ec4e    da  em4e
ec4f    da  $0000
ec50    da  em50
ec51    da  em51
ec52    da  em52
ec53    da  $0000
ec54    da  $0000
ec55    da  em55
ec56    da  em56
ec57    da  em57
ec58    da  $0000
ec59    da  $0000
ec5a    da  em5a
*
* Consume all bytes up to next page boundary
*
filler  ds  \,$00
*
* Must start on page boundary
*
inBuf   ds  1024

