********************************
*                              *
* MORE - UN*X MORE COMMAND     *
* ASSEMBLES WITH MERLIN        *
*                              *
********************************

               DSK   MORE       ;WRITE ASSEMBLED FILE TO DISK
               TYP   $06        ;$FF=SYSTEM, $06=BINARY
               ORG   $2000      ;ASSEMBLE START ADDRESS

* SYSTEM VARIABLES

IN             EQU   $200       ;256-CHAR INPUT BUF

* SUBROUTINES IN MONITOR ROM: $F800 - $FFFF

RDKEY          EQU   $FD0C      ;READS A CHARACTER
GETLN          EQU   $FD6A      ;READS A LINE, WITH PROMPT($33)
GETLN1         EQU   $FD6F      ;READS A LINE, NO PROMPT
CROUT          EQU   $FD8E
COUT           EQU   $FDED
PRBYTE         EQU   $FDDA

* SUBROUTINES IN BASIC.SYSTEM ROM:

GETBUFR        EQU   $BEF5      ;BCC=OKAY & A=HIBYTE OF BUF
                                ;BCS=FAIL & A=ERRCODE
                                ;X & Y ARE DESTROYED
FREEBUFR       EQU   $BEF8      ;FREE BUFFER

* PRODOS ENTRY POINT

PRODOS         EQU   $BF00      ;MACHINE LANG IFACE (MLI)

* MEMORY LOCATIONS

OURCH          EQU   $057B      ;80-COL HORIZ CURSOR POSITION
OURCV          EQU   $05FB      ;VERTICAL CURSOR POSITION

* ZERO-PAGE ADDRESSES

ZP_A1L         EQU   $3C        ;MONITOR GENERAL PURPOSE
ZP_A1H         EQU   $3D        ;MONITOR GENERAL PURPOSE

* PRODOS COMMAND CODES

OPENCMD        EQU   $C8
READCMD        EQU   $CA
CLOSCMD        EQU   $CC

* CONSTANTS

EOFERR         EQU   $4C        ;ERROR CODE FOR END-OF-FILE
PTR            EQU   $06        ;ONLY FREE 0-PAGE LOCATION
MAXERCDE       EQU   $5A        ;LARGEST ERROR CODE
CR             EQU   $0D        ;ASCII CARRIAGE RETURN
CR_HIBIT       EQU   $8D        ;CARRIAGE RET WITH HIGH BIT SET
BUFSIZE        EQU   $00FF
SCR_HGHT       EQU   24         ;SCREEN HEIGHT

* DEBUGGING

TRACE          EQU   0

********************************
*                              *
* PUSH Y ONTO THE STACK        *
* DESTROYS A                   *
*                              *
********************************

PUSHY          MAC
               TYA
               PHA
               <<<

********************************
*                              *
* POP Y FROM THE STACK         *
* DESTROYS A                   *
*                              *
********************************

POPY           MAC
               PLA
               TAY
               <<<

********************************
*                              *
* COPY IN BUF TO STRING        *
*                              *
* X CONTAINS LENGTH OF "IN"    *
* ]1 IS DEST STRING (LEN BYT)  *
* A IS DESTROYED               *
*                              *
********************************

CPIN           MAC
               PUSHY            ;SAVE Y
               STX   ]1         ;COPY LENGTH TO FIRST BYTE OF ]1
               LDY   #0         ;INIT Y TO ZERO
LOOP           CPY   ]1         ;COMPARE Y WITH LENGTH BYTE
               BEQ   ENDLOOP    ;DONE IF LENGTH IS REACHED
               LDA   IN,Y       ;LOAD IN[Y] INTO ACCUMULATOR
               CMP   #CR        ;COMPARE WITH CARRIAGE RETURN
               BEQ   ENDLOOP    ;STOP AT CARRIAGE RETURN
               INY              ;DEST STR IS 1 AHEAD OF IN BUF
               STA   ]1,Y       ;COPY CHAR TO DEST STR ]1
               JMP   LOOP       ;LOOP TO NEXT CHAR
ENDLOOP        POPY             ;RESTORE Y
               <<<

********************************
*                              *
* WRITES A LENGTH PREFIXED     *
* STRING TO THE SCREEN         *
* A IS DESTROYED               *
*                              *
********************************

PUTS           MAC
               PUSHY
               LDY   #0         ;INIT LOOP INDEX
LOOP           CPY   ]1         ;HAS STR LENGTH BEEN REACHED
               BEQ   ENDLOOP    ;IF SO THEN FINISH
               INY              ;MOVE TO INDEX OF NEXT CHAR
               LDA   ]1,Y       ;GET THE CHAR TO BE WRITTEN
               JSR   COUT       ;WRITE THE CHARACTER
               JMP   LOOP       ;LOOP
ENDLOOP        POPY
               <<<

********************************
*                              *
* SET TO #1 (IMMEDIATE 1)      *
* ]1 DESTINATION               *
* A IS DESTROYED               *
*                              *
********************************

SET1           MAC
               LDA   #1
               STA   ]1
               <<<

SET0           MAC
               LDA   #0
               STA   ]1
               <<<

SET23          MAC
               LDA   #23
               STA   ]1
               <<<

********************************
*                              *
* MAIN PROGRAM                 *
*                              *
********************************

MAIN           CLD              ;CLEAR DECIMAL FLG, AVOID CRASH
               SET0  USRQUIT    ;INITIALIZE TO "NO"
*
* GET FILE NAME
*
               PUTS  PROMPT
               JSR   GETLN1     ;LENGTH IN X, CR AT END
               CPX   #0         ;IS THE LENGTH ZERO?
               BEQ   :END       ;USER JUST PRESSED RETURN
               CPIN  FILENAME   ;COPY "IN" BUF TO FILENAME
*
* GET FILE I/O BUFFER FOR OPEN CALL
*
               LDA   #4         ;FOUR 256 BYTE PAGES = 1KB
               JSR   GETBUFR    ;GET BUF FROM BASIC.SYSTEM
               BCS   :OBUFERR   ;CARRY CLEAR MEANS NO ERROR
               STA   OBUFADDR+1 ;GETBUFR RETURNS HIBYTE IN A
               LDA   #0         ;PREPARE
               STA   OBUFADDR   ;LOBYTE IS 0 B/C ADDR OF PAGE
:OPENFILE      JSR   PRODOS
               DB    OPENCMD
               DA    OPENPRMS
               BEQ   :OPEN_OK
               JSR   ERRPROC
               JMP   :FREEOBUF
*
* COPY FILE NUMBER FROM OPEN PARAMETERS TO READ AND CLOSE
*
:OPEN_OK       LDA   OPENFNUM
               STA   READFNUM
               STA   CLOSFNUM
*
* GET BUFFER FOR READ OPERATION FROM BASIC.SYSTEM
*
               LDA   #1         ;ONE 256 BYTE BUFFER
               JSR   GETBUFR    ;CALL BASIC.SYSTEM SUB
               BCC   :RBUF_OK   ;CARRY CLR MEANS NO ERROR
               JSR   ERRPROC
               JMP   :CLOSFILE
:RBUF_OK       STA   RBADDR+1   ;STORE HI-BYTE
               LDA   #0         ;0 FOR LO-BYTE
               STA   RBADDR     ;STORE IT
               STA   ZP_A1L     ;AGAIN, FOR 0-PAGE INDIRECTION
*
* PRINT THE FILE
*
               JSR   VIEWFILE
*
* CLEANUP
*
:FREERBUF      JSR   FREEBUFR   ;FREE READ BUFFER
:CLOSFILE      JSR   PRODOS     ;CLOSE THE FILE
               DB    CLOSCMD
               DA    CLOSPRMS
               BCC   FREEOBUF
:OPENERR       JSR   ERRPROC
:FREEOBUF      JSR   FREEBUFR   ;FREE OPEN I/O BUFFER
               JMP   :END
:OBUFERR       JSR   ERRPROC
:END           NOP
               RTS

********************************
*                              *
* VIEW FILE SUB                *
*                              *
********************************

VIEWFILE
               DO    TRACE
               PUTS  ENVIEW
               FIN

               SET1  LINENUM    ;INIT LINE NUMBER
:LOOP          JSR   PRODOS     ;CALL PRODOS TO READ FILE
               DB    READCMD    ;SPECIFY PRODOS READ COMMAND
               DA    READPRMS   ;READ PARAMETERS
               BNE   :READERR 
               JSR   WRITEBUF   ;WRITE TO SCREEN WHAT WAS READ
               LDA   #1         ;PREPARE FOR NEXT OP
               CMP   USRQUIT    ;IF USER WANTS TO QUIT, THEN
               BEQ   :ENDLOOP   ;EXIT THE LOOP
               JMP   :LOOP      ;ELSE, GET THE NEXT BUFFER
:READERR       JSR   ERRPROC
:ENDLOOP       NOP
               RTS

********************************
*                              *
* WRITE BUFFER TO SCREEN       *
*                              *
********************************

WRITEBUF
               PUSHY
               LDY   #0         ;INIT CHAR COUNTER VARIABLE
:LOOP          CPY   READCNT    ;COMPARE TO MAX CHARS
               BEQ   :ENDLOOP
               LDA   (ZP_A1L),Y ;GET CHAR FROM BUFFER
               ORA   #%10000000 ;TURN ON HIGH BIT FOR PRINTING
               JSR   COUT       ;COUT PRESERVES ACCUM
*
* CHECK END OF LINE
*
               CMP   #CR_HIBIT  ;COMPARE TO CARRIAGE RETURN
               BNE   :CONT      ;IF NOT END OF LINE, NEXT CHAR
               INC   LINENUM    ;NEXT LINE HAS BEEN REACHED
*
* CHECK AT END OF PAGE
*
               LDA   LINENUM
               CMP   #SCR_HGHT  ;AT BOTTOM OF SCREEN?
               BNE   :CONT      ;NO? THEN NEXT CHAR
               JSR   STATBAR    ;YES? THEN SHOW THE STATUS BAR
               LDA   #1         ;SETUP FOR NEXT LINE
               CMP   USRQUIT    ;DID USER ASK TO QUIT
               BEQ   :ENDLOOP   ;YES? THEN END SUB
:CONT          INY              ;STATBAR HAS ADJUSTED LINENUM
               JMP   :LOOP
:ENDLOOP       NOP
               POPY

               DO    TRACE
               PUTS  EXVIEW
               FIN

               RTS

********************************
*                              *
* PRINT ASCII IN HEX           *
*                              *
********************************

PRASCII        PHA
               LDA   #"["
               JSR   COUT
               PLA
               JSR   PRBYTE
               LDA   #"]"
               JSR   COUT
               LDA   #" "
               JSR   COUT
               RTS

********************************
*                              *
* DO THE STATUS BAR            *
*                              *
********************************

STATBAR
               DO    TRACE
               PUTS  ENSTATB
               FIN

               PUSHY
               PUTS  BAR
:LOOP          JSR   RDKEY      ;GET A KEY FROM THE USER
               CMP   #" "       ;CHECK IF SPACE ENTERED
               BNE   :CHKCR     ;IF NOT FORWARD TO NEXT CHECK
               SET1  LINENUM    ;ADVANCE ONE PAGE, STORE 1
               JMP   :ENDLOOP   ;PROCESSED SPACE SO DONE
:CHKCR         CMP   #CR_HIBIT  ;CHECK FOR CARRIAGE RETURN
               BNE   :CHKQUIT
               SET23 LINENUM
               JMP   :ENDLOOP
:CHKQUIT       CMP   #"Q"       ;USER WANTS TO QUIT
               BNE   :LOOP      ;NO RECOGNIZED INPUT
               SET1  USRQUIT
:ENDLOOP       JSR   ERASEBAR
               POPY

               DO    TRACE
               PUTS  EXSTATB
               FIN

               RTS

********************************
*                              *
* ERASE STATUS BAR             *
*                              *
********************************

ERASEBAR
               DO    TRACE
               PUTS  ENERASEB
               FIN

               PUSHY
               SET0  OURCH      ;RESET CURSOR TO BEG OF LINE
               LDY   #0         ;INIT COUNTER FOR SPACES
:LOOP          CPY   BAR        ;FIRST BYTE IS LENGTH
               BEQ   :ENDLOOP   ;IF Y=LEN THEN DONE
               LDA   #" "       ;LOAD SPACE
               JSR   COUT       ;WRITE TO SCREEN
               INY              ;MAKE PROGRESS
               JMP   :LOOP      ;LOOP TO NEXT CHAR
:ENDLOOP       SET0  OURCH      ;RESET CURSON TO BEG OF LINE
               POPY

               DO    TRACE
               PUTS  EXERASEB
               FIN

               RTS

********************************
*                              *
* ERROR HANDLER                *
* INPUT PARAM: ERRCODE         *
*                              *
********************************

ERRPROC
            LDA     ERRCODE
            CMP     #0
            BEQ     :NOERR
            CMP     #EOFERR
            BEQ     :NOERR
            PUTS    ERRTXT
            LDA     ERRCODE
            JSR     PRBYTE
            JSR     CROUT
:NOERR      NOP
            RTS

********************************
*                              *
* DATA DIVISION HAHA           *
*                              *
********************************

PROMPT         STR   "FILE:"
ERRTXT         STR   "ERROR:"
FILENAME       DS    $FF
HERE           STR   "HERE"
HERE2          STR   "HERE2"
READRET        STR   "READRET="
SREADCNT       STR   "READCNT="
ERRCODE        DS    1
READERR        DS    1
CLOSERR        DS    1
LINENUM        DS    1
BAR            STR   '[RET] NEXT LINE / [SPC] NEXT PAGE / [Q]UIT'
USRQUIT        DS    1
BUFCHAR        DS    1
USRCHAR        DS    1

ENVIEW         STR   'ENTERING VIEWFILE'
EXVIEW         STR   'EXITING VIEWFILE'
ENSTATB        STR   'ENTERING STATUSBAR'
EXSTATB        STR   'EXITING STATUSBAR'
ENERASEB       STR   'ENTERING ERASEBAR'
EXERASEB       STR   'EXITING ERASEBAR'
*
* OPEN PARAMETERS
*
OPENPRMS       DB    3
               DA    FILENAME
OBUFADDR       DS    2
OPENFNUM       DS    1
*
* READ PARAMETERS
*
READPRMS       DB    4
READFNUM       DS    1
RBADDR         DS    2
REQCNT         DW    BUFSIZE
READCNT        DS    2
*
* CLOSE PARAMETERS
*
CLOSPRMS       DB    1
CLOSFNUM       DS    1
*
* BUFFERS
*
* CONSUME ALL BYTES UP TO THE NEXT PAGE BOUNDRY
FILLER DS \,$00
* MUST START ON PAGE BOUNDRY
*OPENBUF DS 1024
*READBUF DS BUFSIZE

:
