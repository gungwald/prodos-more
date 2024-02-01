********************************
*                              *
* MORE - UN*X MORE COMMAND     *
* ASSEMBLES WITH S-C           *
*                              *
********************************

*              DSK   MORE       ;WRITE ASSEMBLED FILE TO DISK
*              TYP   $06        ;$FF=SYSTEM, $06=BINARY
               .OR   $2000      ;ASSEMBLE START ADDRESS
*
* SYSTEM VARIABLES
*
IN             .EQ   $200       ;256-CHAR INPUT BUF
*
* SUBROUTINES IN MONITOR ROM: $F800 - $FFFF
*
RDKEY          .EQ   $FD0C      ;READS A CHARACTER
GETLN          .EQ   $FD6A      ;READS A LINE, WITH PROMPT($33)
GETLN1         .EQ   $FD6F      ;READS A LINE, NO PROMPT
CROUT          .EQ   $FD8E
COUT           .EQ   $FDED
PRBYTE         .EQ   $FDDA
*
* SUBROUTINES IN BASIC.SYSTEM ROM:
*
GETBUFR        .EQ   $BEF5      ;ALLOCATE BUFFER, DESTROYS X & Y
FREEBUFR       .EQ   $BEF8      ;FREE BUFFER
*
* PRODOS ENTRY POINT
*
MLI            .EQ   $BF00
*
* MEMORY LOCATIONS
*
OURCH          .EQ   $057B      ;80-COL HORIZ CURSOR POSITION
OURCV          .EQ   $05FB      ;VERTICAL CURSOR POSITION
*
* ZERO-PAGE ADDRESSES
*
ZP.A1L         .EQ   $3C        ;MONITOR GENERAL PURPOSE
ZP.A1H         .EQ   $3D        ;MONITOR GENERAL PURPOSE
*
* PRODOS COMMAND CODES
*
OPENCMD        .EQ   $C8
READCMD        .EQ   $CA
CLOSCMD        .EQ   $CC
*
* CONSTANTS
*
EOFERR         .EQ   $4C        ;ERROR CODE FOR END-OF-FILE
PTR            .EQ   $06        ;ONLY FREE 0-PAGE LOCATION
MAXERCDE       .EQ   $5A        ;LARGEST ERROR CODE
CR             .EQ   $0D        ;ASCII CARRIAGE RETURN
CR.HIBIT       .EQ   $8D        ;CARRIAGE RET WITH HIGH BIT SET
BUFSIZE        .EQ   $00FF
SCR.HGHT       .EQ   24         ;SCREEN HEIGHT
*
* DEBUGGING
*
TRACE          .EQ   0

********************************
*                              *
* PUSH Y ONTO THE STACK        *
* DESTROYS A                   *
*                              *
********************************

PUSHY          .MA
               TYA
               PHA
               .EM

********************************
*                              *
* POP Y FROM THE STACK         *
* DESTROYS A                   *
*                              *
********************************

POPY           .MA
               PLA
               TAY
               .EM

********************************
*                              *
* COPY IN BUF TO STRING        *
*                              *
* X CONTAINS LENGTH OF "IN"    *
* ]1 IS DEST STRING (LEN BYT)  *
* A IS DESTROYED               *
*                              *
********************************

CPIN           .MA
               PUSHY            ;SAVE Y
               STX   ]1         ;COPY LENGTH TO FIRST BYTE OF ]1
               LDY   #0         ;INIT Y TO ZERO
:1             CPY   ]1         ;COMPARE Y WITH LENGTH BYTE
               BEQ   :2         ;DONE IF LENGTH IS REACHED
               LDA   IN,Y       ;LOAD IN[Y] INTO ACCUMULATOR
               CMP   #CR        ;COMPARE WITH CARRIAGE RETURN
               BEQ   :2         ;STOP AT CARRIAGE RETURN
               INY              ;DEST STR IS 1 AHEAD OF IN BUF
               STA   ]1,Y       ;COPY CHAR TO DEST STR ]1
               JMP   :1         ;LOOP TO NEXT CHAR
:2             POPY             ;RESTORE Y
               .EM

********************************
*                              *
* WRITES A LENGTH PREFIXED     *
* STRING TO THE SCREEN         *
* A IS DESTROYED               *
*                              *
********************************

PUTS           .MA
               PUSHY
               LDY   #0         ;INIT LOOP INDEX
:1             CPY   ]1         ;HAS STR LENGTH BEEN REACHED
               BEQ   :2         ;IF SO THEN FINISH
               INY              ;MOVE TO INDEX OF NEXT CHAR
               LDA   ]1,Y       ;GET THE CHAR TO BE WRITTEN
               JSR   COUT       ;WRITE THE CHARACTER
               JMP   :1         ;LOOP
:2             POPY
               .EM

********************************
*                              *
* SET TO #1 (IMMEDIATE 1)      *
* ]1 DESTINATION               *
* A IS DESTROYED               *
*                              *
********************************

SET1           .MA
               LDA   #1
               STA   ]1
               .EM

SET0           .MA
               LDA   #0
               STA   ]1
               .EM

SET23          .MA
               LDA   #23
               STA   ]1
               .EM

********************************
*                              *
* MAIN PROGRAM                 *
*                              *
********************************

MAIN           CLD              ;CLEAR DECIMAL FLG, AVOID CRASH
               >SET0 USRQUIT    ;INITIALIZE TO "NO"
*
* GET FILE NAME
*
               >PUTS PROMPT
               JSR   GETLN1     ;LENGTH IN X, CR AT END
               CPX   #0         ;IS THE LENGTH ZERO?
               BEQ   END        ;USER JUST PRESSED RETURN
               CPIN  FILENAME   ;COPY "IN" BUF TO FILENAME
*
* GET FILE I/O BUFFER FOR OPEN CALL
*
               LDA   #4         ;FOUR 256 BYTE PAGES = 1KB
               JSR   GETBUFR    ;GET BUF FROM BASIC.SYSTEM
               BCS   ERRFWDR    ;CARRY SET INDICATES AN ERROR
               STA   OBUFADDR+1 ;GETBUFR RETURNS HIBYTE IN A
               LDA   #0         ;PREPARE
               STA   OBUFADDR   ;LOBYTE IS 0 B/C ADDR OF PAGE
*
* OPEN FILE
*
               JSR   MLI
               DB    OPENCMD
               DA    OPENPRMS
               BNE   ERRFWDR
*
* COPY FILE NUMBER FROM OPEN PARAMETERS TO READ AND CLOSE
*
               LDA   OPENFNUM
               STA   READFNUM
               STA   CLOSFNUM
*
* GET BUFFER FOR READ OPERATION FROM BASIC.SYSTEM
*
               LDA   #1         ;ONE 256 BYTE BUFFER
               JSR   GETBUFR    ;CALL BASIC.SYSTEM SUB
               BCS   ERRFWDR    ;CARRY SET MEANS ERROR
               STA   RBADDR+1   ;STORE HI-BYTE
               STA   ZP.A1H     ;AGAIN, FOR 0-PAGE INDIRECTION
               LDA   #0         ;0 FOR LO-BYTE
               STA   RBADDR     ;STORE IT
               STA   ZP.A1L     ;AGAIN, FOR 0-PAGE INDIRECTION
*
* PRINT THE FILE
*
               JSR   VIEWFILE   ;MUST CLOSE BEFORE ERR HANDLING
*
* FREE READ BUFFER
*
               JSR   FREEBUFR
*
* CLOSE FILE
*
               JSR   MLI
               DB    CLOSCMD
               DA    CLOSPRMS
               STA   CLOSERR
*
* FREE FILE BUFFER USED IN OPEN CALL
*
               JSR   FREEBUFR
*
* CHECK FOR ERRORS
*
               LDA   READERR    ;RELOAD THE READ RESULT CODE
               CMP   EOFERR     ;WAS IT AN EOF "ERROR"
               BEQ   END        ;EOF IS NOT REALLY AN ERROR
               CMP   #0
               BNE   ERRFWDR
               LDA   CLOSERR
               BNE   ERRFWDR
               JMP   END
*
* FINISH
*
ERRFWDR        JSR   ERRPROC
END            NOP
               RTS

********************************
*                              *
* VIEW FILE SUB                *
*                              *
********************************

VIEWFILE
               DO    TRACE
               >PUTS ENVIEW
               FIN

               >SET1 LINENUM    ;INIT LINE NUMBER
.1             JSR   MLI        ;CALL PRODOS TO READ FILE
               DB    READCMD    ;SPECIFY PRODOS READ COMMAND
               DA    READPRMS   ;READ PARAMETERS
               STA   READERR    ;SAVE RESULT OF READ OPERATION
               BNE   .2         ;IF ERROR THEN END SUB
               JSR   WRITEBUF   ;WRITE TO SCREEN WHAT WAS READ
               LDA   #1         ;PREPARE FOR NEXT OP
               CMP   USRQUIT    ;IF USER WANTS TO QUIT, THEN
               BEQ   .2         ;EXIT THE LOOP
               JMP   .1         ;ELSE, GET THE NEXT BUFFER
.2             NOP
               RTS

********************************
*                              *
* WRITE BUFFER TO SCREEN       *
*                              *
********************************

WRITEBUF
               >PUSHY
               LDY   #0         ;INIT CHAR COUNTER VARIABLE
.1             CPY   READCNT    ;COMPARE TO MAX CHARS
               BEQ   .3      
               LDA   (ZP_A1L),Y ;GET CHAR FROM BUFFER
               ORA   #%10000000 ;TURN ON HIGH BIT FOR PRINTING
               JSR   COUT       ;COUT PRESERVES ACCUM
*
* CHECK END OF LINE
*
               CMP   #CR_HIBIT  ;COMPARE TO CARRIAGE RETURN
               BNE   .2         ;IF NOT END OF LINE, NEXT CHAR
               INC   LINENUM    ;NEXT LINE HAS BEEN REACHED
*
* CHECK AT END OF PAGE
*
               LDA   LINENUM
               CMP   #SCR_HGHT  ;AT BOTTOM OF SCREEN?
               BNE   .2         ;NO? THEN NEXT CHAR
               JSR   STATBAR    ;YES? THEN SHOW THE STATUS BAR
               LDA   #1         ;SETUP FOR NEXT LINE
               CMP   USRQUIT    ;DID USER ASK TO QUIT
               BEQ   .3         ;YES? THEN END SUB
.2             INY              ;STATBAR HAS ADJUSTED LINENUM
               JMP   .1   
.3             >POPY

               DO    TRACE
               >PUTS  EXVIEW
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
               DO       TRACE
               >PUTS    ENSTATB
               FIN

               >PUSHY
               >PUTS    BAR
.1             JSR      RDKEY      ;GET A KEY FROM THE USER
               CMP      #" "       ;CHECK IF SPACE ENTERED
               BNE      .2         ;IF NOT FORWARD TO NEXT CHECK
               >SET1    LINENUM    ;ADVANCE ONE PAGE, STORE 1
               JMP      .4         ;PROCESSED SPACE SO DONE
.2             CMP      #CR_HIBIT  ;CHECK FOR CARRIAGE RETURN
               BNE      .3      
               >SET23   LINENUM
               JMP      .4      
.3             CMP      #"Q"       ;USER WANTS TO QUIT
               BNE      .1         ;NO RECOGNIZED INPUT
               >SET1    USRQUIT
.4             JSR      ERASEBAR
               >POPY

               DO       TRACE
               >PUTS    EXSTATB
               FIN

               RTS

********************************
*                              *
* ERASE STATUS BAR             *
*                              *
********************************

ERASEBAR
               DO       TRACE
               >PUTS    ENERASEB
               FIN

               >PUSHY
               >SET0    OURCH      ;RESET CURSOR TO BEG OF LINE
               LDY      #0         ;INIT COUNTER FOR SPACES
.1             CPY      BAR        ;FIRST BYTE IS LENGTH
               BEQ      .2         ;IF Y=LEN THEN DONE
               LDA      #" "       ;LOAD SPACE
               JSR      COUT       ;WRITE TO SCREEN
               INY                 ;MAKE PROGRESS
               JMP      .1         ;LOOP TO NEXT CHAR
.2             >SET0    OURCH      ;RESET CURSON TO BEG OF LINE
               POPY

               DO       TRACE
               >PUTS    EXERASEB
               FIN

               RTS

********************************
*                              *
* ERROR HANDLER                *
*                              *
********************************

ERRPROC        STA   ERRCODE
               >PUTS ERRTXT
               JSR   PRBYTE
               JSR   CROUT
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
*FILLER DS \,$00
* MUST START ON PAGE BOUNDRY
*OPENBUF DS 1024
*READBUF DS BUFSIZE
