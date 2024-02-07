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

PRODOS_MLI     EQU   $BF00      ;MACHINE LANG IFACE (MLI)

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
               BNE   :CONT1     ;USER JUST PRESSED RETURN
               JMP   END
:CONT1         CPIN  FILENAME   ;COPY "IN" BUF TO FILENAME

               DO    TRACE
               JSR   PRSTRBYTES
               JSR   GETLN
               FIN
*
* GET FILE I/O BUFFER FOR OPEN CALL
*
               LDA   #4         ;FOUR 256 BYTE PAGES = 1KB
               JSR   GETBUFR    ;GET BUF FROM BASIC.SYSTEM
               BCC   :CONT2     ;CARRY CLEAR MEANS NO ERROR
               JSR   OBUFERR
               JMP   END       
:CONT2         STA   OBUFADDR+1 ;GETBUFR RETURNS HIBYTE IN A
               LDA   #0         ;PREPARE
               STA   OBUFADDR   ;LOBYTE IS 0 B/C ADDR OF PAGE
*
* OPEN FILE
*
               JSR   PRODOS_MLI
               DB    OPENCMD
               DA    OPENPRMS
               BEQ   CONT3
               JSR   OPENERR
               JMP   :FREEOBUF
*
* COPY FILE NUMBER FROM OPEN PARAMETERS TO READ AND CLOSE
*
CONT3          LDA   OPENFNUM
               STA   READFNUM
               STA   CLOSFNUM
*
* GET BUFFER FOR READ OPERATION FROM BASIC.SYSTEM
*
               LDA   #1         ;ONE 256 BYTE BUFFER
               JSR   GETBUFR    ;CALL BASIC.SYSTEM SUB
               BCC   CONT4
               JSR   RBUFERR    ;CARRY SET MEANS ERROR
               JMP   :CLOSFILE
CONT4          STA   RBADDR+1   ;STORE HI-BYTE
               STA   ZP_A1H     ;FOR 0-PAGE INDIRECTION
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
               JSR   FREEBUFR   ;FREE READ BUFFER

:CLOSFILE      JSR   PRODOS_MLI ;CLOSE THE FILE
               DB    CLOSCMD
               DA    CLOSPRMS
               BEQ   :FREEOBUF
               JSR   CLOSERR
               
:FREEOBUF      JSR   FREEBUFR   ;FREE OPEN I/O BUFFER
END            NOP
               RTS

********************************
*                              *
* END OF MAIN - PROGRAM EXIT   *
*                              *
********************************

********************************
*                              *
* HANDLE ERROR WHEN OPENING    *
* FILE                         *
*                              *
********************************
OPENERR        STA   ERRCODE
               PUTS  OERRMSG
               PUTS  FILENAME
               LDA   #"'"
               JSR   COUT
               LDA   #":"
               JSR   COUT
               LDA   ERRCODE
               JSR   ERRPROC
               RTS

********************************
*                              *
* HANDLE ERROR WHEN CLOSING    *
* FILE                         *
*                              *
********************************
CLOSERR        STA   ERRCODE
               PUTS  CERRMSG
               PUTS  FILENAME
               LDA   #"'"
               JSR   COUT
               LDA   #":"
               JSR   COUT
               LDA   ERRCODE
               JSR   ERRPROC
               RTS

********************************
*                              *
* HANDLE ERROR WHEN REQUESTING *
* READ BUFFER                  *
*                              *
********************************
RBUFERR        STA   ERRCODE
               PUTS  RBERRMSG
               LDA   #":"
               JSR   COUT
               LDA   ERRCODE
               JSR   ERRPROC
               RTS

********************************
*                              *
* HANDLE ERROR WHEN REQUESTING *
* READ BUFFER                  *
*                              *
********************************
OBUFERR        STA   ERRCODE
               PUTS  OBERRMSG
               LDA   #":"
               JSR   COUT
               LDA   ERRCODE
               JSR   ERRPROC
               RTS

********************************
*                              *
* PRINT "IN" BUFFER            *
*                              *
********************************
PRINT_IN       
            PUSHY
            LDA     #"X"
            JSR     COUT
            LDA     #"="
            JSR     COUT
            TXA
            JSR     PRBYTE
            JSR     CROUT

            LDY     #0
:LOOP       CPY     #255
            BEQ     :ENDLOOP
            LDA     IN,Y
            JSR     PRBYTE
            LDA     #" "
            JSR     COUT
            INY
            JMP     :LOOP
:ENDLOOP    JSR     CROUT
            POPY
            RTS

********************************
*                              *
* PRINT STRING BYTES           *
*                              *
********************************
PRSTRBYTES
            PUSHY
            LDY     #0
:LOOP       CPY     #255
            BEQ     :ENDLOOP
            LDA     FILENAME,Y
            JSR     PRBYTE
            LDA     #" "
            JSR     COUT
            INY
            JMP     :LOOP
:ENDLOOP    JSR     CROUT
            POPY
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
:LOOP          JSR   PRODOS_MLI ;CALL PRODOS TO READ FILE
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
               PUTS  EXWRITEBUF
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
* INPUT PARAM: A HOLDS ERRCODE *
*                              *
********************************

ERRPROC
            STA     ERRCODE
            CMP     #0
            BNE     :EEOF
            JMP     :END
            
:EEOF       CMP     #EOFERR
            BNE     :E01
            JMP     :END

:E01        CMP     #1
            BNE     :E04
            PUTS    E01MSG
            JMP     :END

:E04        CMP     #4
            BNE     :E25
            PUTS    E04MSG
            JMP     :END

:E25        CMP     #$25
            BNE     :E27
            PUTS    E25MSG
            JMP     :END

:E27        CMP     #$27
            BNE     :E28
            PUTS    E27MSG
            JMP     :END

:E28        CMP     #$28
            BNE     :E2B
            PUTS    E28MSG
            JMP     :END

:E2B        CMP     #$2B
            BNE     :E2E
            PUTS    E2BMSG
            JMP     :END

:E2E        CMP     #$2E
            BNE     :E40
            PUTS    E2EMSG
            JMP     :END

:E40        CMP     #$40
            BNE     :E42
            PUTS    E40MSG
            JMP     :END

:E42        CMP     #$42
            BNE     :E43
            PUTS    E42MSG
            JMP     :END

:E43        CMP     #$43
            BNE     :E44
            PUTS    E43MSG
            JMP     :END

:E44        CMP     #$44
            BNE     :E45
            PUTS    E44MSG
            JMP     :END

:E45        CMP     #$45
            BNE     :E46
            PUTS    E45MSG
            JMP     :END

:E46        CMP     #$46
            BNE     :E47
            PUTS    E46MSG
            JMP     :END

:E47        CMP     #$47
            BNE     :E48
            PUTS    E47MSG
            JMP     :END

:E48        CMP     #$48
            BNE     :E49
            PUTS    E48MSG
            JMP     :END

:E49        CMP     #$49
            BNE     :E4A
            PUTS    E49MSG
            JMP     :END

:E4A        CMP     #$4A
            BNE     :E4B
            PUTS    E4AMSG
            JMP     :END

:E4B        CMP     #$4B
            BNE     :E4C
            PUTS    E4BMSG
            JMP     :END

:E4C        CMP     #$4C
            BNE     :E4D
            PUTS    E4CMSG
            JMP     :END

:E4D        CMP     #$4D
            BNE     :E4E
            PUTS    E4DMSG
            JMP     :END

:E4E        CMP     #$4E
            BNE     :E50
            PUTS    E4EMSG
            JMP     :END

:E50        CMP     #$50
            BNE     :E51
            PUTS    E50MSG
            JMP     :END
            
:E51        CMP     #$51
            BNE     :E52
            PUTS    E51MSG
            JMP     :END

:E52        CMP     #$52
            BNE     :E53
            PUTS    E52MSG
            JMP     :END

:E53        CMP     #$53
            BNE     :E55
            PUTS    E53MSG
            JMP     :END

:E55        CMP     #$55
            BNE     :E56
            PUTS    E55MSG
            JMP     :END
            
:E56        CMP     #$56
            BNE     :E57
            PUTS    E56MSG
            JMP     :END

:E57        CMP     #$57
            BNE     :E5A
            PUTS    E57MSG
            JMP     :END

:E5A        CMP     #$5A
            BNE     :E_UNK
            PUTS    E5AMSG
            JMP     :END

:E_UNK      PUTS    E_UNK_MSG
            LDA     #":"
            JSR     COUT
            LDA     ERRCODE
            JSR     PRBYTE  
:END        JSR     CROUT
            RTS

********************************
*                              *
* DATA DIVISION HAHA           *
*                              *
********************************

PROMPT         STR   "FILE:"
ERRTXT         STR   "ERROR:"
FILENAME       DS    $FF
ERRCODE        DS    1
LINENUM        DS    1
BAR            STR   '[RET] LINE  [SPC] PAGE  [Q]UIT'
USRQUIT        DS    1
BUFCHAR        DS    1
USRCHAR        DS    1

OERRMSG     STR     "FAILED TO OPEN FILE '"
CERRMSG     STR     "FAILED TO CLOSE FILE '"
RBERRMSG    STR     "REQUEST FOR READ BUFFER FAILED"
OBERRMSG    STR     "REQUEST FOR OPEN BUFFER FAILED"

E00MSG      STR     "NO ERROR"
E01MSG      STR     "BAD SYSTEM CALL NUMBER"
E04MSG      STR     "BAD SYSTEM CALL PARAMETER COUNT"
E25MSG      STR     "INTERRUPT TABLE FULL"
E27MSG      STR     "I/O ERROR"
E28MSG      STR     "NO DEVICE CONNECTED"
E2BMSG      STR     "DISK WRITE PROTECTED"
E2EMSG      STR     "DISK SWITCHED"
E40MSG      STR     "INVALID PATHNAME"
E42MSG      STR     "MAXIMUM NUMBER OF FILES OPEN"
E43MSG      STR     "INVALID REFERENCE NUMBER"
E44MSG      STR     "DIRECTORY NOT FOUND"
E45MSG      STR     "VOLUME NOT FOUND"
E46MSG      STR     "FILE NOT FOUND"
E47MSG      STR     "DUPLICATE FILENAME"
E48MSG      STR     "VOLUME FULL"
E49MSG      STR     "VOLUME DIRECTORY FULL"
E4AMSG      STR     "INCOMPATIBLE FILE FORMAT OR PRODOS DIRECTORY"
E4BMSG      STR     "UNSUPPORTED STORAGE TYPE"
E4CMSG      STR     "END OF FILE ENCOUNTERED"
E4DMSG      STR     "POSITION OUT OF RANGE"
E4EMSG      STR     "FILE ACCESS ERROR OR FILE LOCKED"
E50MSG      STR     "FILE IS OPEN"
E51MSG      STR     "DIRECTORY STRUCTURE DAMAGED"
E52MSG      STR     "NOT A PRODOS VOLUME"
E53MSG      STR     "INVALID SYSTEM CALL PARAMETER"
E55MSG      STR     "VOLUME CONTROL BLOCK TABLE FULL"
E56MSG      STR     "BAD BUFFER ADDRESS"
E57MSG      STR     "DUPLICATE VOLUME"
E5AMSG      STR     "FILE STRUCTURE DAMAGED"
E_UNK_MSG   STR     "UNKNOWN ERROR CODE"

ENVIEW         STR   'ENTERING VIEWFILE'
EXVIEW         STR   'EXITING VIEWFILE'
EXWRITEBUF     STR   'EXITING WRITEBUF'
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
