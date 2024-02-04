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

GETBUFR        EQU   $BEF5      ;BCC=OKAY & A=HIBYTE OF BUF
                                ;BCS=FAIL & A=ERRCODE
                                ;X & Y ARE DESTROYED
FREEBUFR       EQU   $BEF8      ;FREE BUFFER

* PRODOS ENTRY POINT

PRODOS         EQU   $BF00      ;MACHINE LANG IFACE (MLI)

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
               BCS   :OBUFERR   ;CARRY CLEAR MEANS NO ERROR
               STA   OBUFADDR+1 ;GETBUFR RETURNS HIBYTE IN A
               LDA   #0         ;PREPARE
               STA   OBUFADDR   ;LOBYTE IS 0 B/C ADDR OF PAGE
*
* OPEN FILE
*
               JSR   PRODOS
               DB    OPENCMD
               DA    OPENPRMS
               BNE   :OPENERR
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
               BCS   :RBUFERR   ;CARRY CLR MEANS NO ERROR
               STA   RBADDR+1   ;STORE HI-BYTE
               LDA   #0         ;0 FOR LO-BYTE
               STA   RBADDR     ;STORE IT
               STA   ZP.A1L     ;AGAIN, FOR 0-PAGE INDIRECTION
*
* PRINT THE FILE
*
               JSR   VIEWFILE
*
* CLEANUP
*
               JSR   FREEBUFR   ;FREE READ BUFFER
               JMP   :CLOSFILE  ;SKIP OVER READ BUF ERR HANDLER

:RBUFERR       JSR   ERRPROC    ;HANDLE ERR GETTING READ BUF

:CLOSFILE      JSR   PRODOS     ;CLOSE THE FILE
               DB    CLOSCMD
               DA    CLOSPRMS
               BEQ   :FREEOBUF
               
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
               >PUTS ENVIEW
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
               >PUSHY
               LDY   #0         ;INIT CHAR COUNTER VARIABLE
.1             CPY   READCNT    ;COMPARE TO MAX CHARS
               BEQ   .3      
               LDA   (ZP.A1L),Y ;GET CHAR FROM BUFFER
               ORA   #%10000000 ;TURN ON HIGH BIT FOR PRINTING
               JSR   COUT       ;COUT PRESERVES ACCUM
*
* CHECK END OF LINE
*
               CMP   #CR.HIBIT  ;COMPARE TO CARRIAGE RETURN
               BNE   .2         ;IF NOT END OF LINE, NEXT CHAR
               INC   LINENUM    ;NEXT LINE HAS BEEN REACHED
*
* CHECK AT END OF PAGE
*
               LDA   LINENUM
               CMP   #SCR.HGHT  ;AT BOTTOM OF SCREEN?
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
.2             CMP      #CR.HIBIT  ;CHECK FOR CARRIAGE RETURN
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
* INPUT PARAM: A HOLDS ERRCODE *
*                              *
********************************

ERRPROC
            STA     ERRCODE
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
BAR            STR   '[RET] LINE  [SPC] PAGE  [Q]UIT'
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
OPENPRMS       .DA   #3
               .DA   FILENAME
OBUFADDR       .BS   2
OPENFNUM       .BS   1
*
* READ PARAMETERS
*
READPRMS       .DA   #4
READFNUM       .BS   1
RBADDR         .BS   2
REQCNT         .DA   BUFSIZE
READCNT        .BS   2
*
* CLOSE PARAMETERS
*
CLOSPRMS       .DA   #1
CLOSFNUM       .BS   1
*
* BUFFERS
*
* CONSUME ALL BYTES UP TO THE NEXT PAGE BOUNDRY
*FILLER DS \,$00
* MUST START ON PAGE BOUNDRY
*OPENBUF DS 1024
*READBUF DS BUFSIZE

:
