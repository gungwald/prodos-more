********************************
*                              *
* MORE - UN*X MORE COMMAND     *
* ASSEMBLES WITH MERLIN        *
*                              *
********************************

* Memory Map
*
* $0000 - $00FF	Zero Page
* $0100 - $01FF	Stack
* $0200 - $03FF	Mostly Free, Input Buffer, Interrupt Vectors
* $0400 - $07FF	Lo-res/Text Page1
* $0800 - $0BFF	Lo-res/Text Page2 (BASIC programs load here)
* $0C00 - $1FFF	Free
* $2000 - $3FFF	Hi-res Page1
* $4000 - $5FFF	Hi-res Page2
* $6000 - $95FF	Free
* $9600 - $BFFF	DOS3.3 and Buffers
* $C000 - $CFFF	Soft Switches, Expansion Card I/O and ROM
* $D000 - $F7FF	BASIC ROM (can be bankswitched later models)
* $F800 - $FFFF	Machine Language Monitor ROM (also can be bankswitched)
*
* Branching
*
* CMP,CPX,CPY ->
* Condition 	        N 	Z 	C
* Register < Memory 	1 	0 	0
* Register = Memory 	0 	1 	1
* Register > Memory 	0 	0 	1 
*
* BPL - BRANCH PLUS (N=0) NOT NEGATIVE
* BMI - BRANCH MINUS (N=1) NEGATIVE
* BEQ - BRANCH EQUAL (Z=1) ZERO FLAG SET
* BNE - BRANCH NOT EQUAL (Z=0) ZERO FLAG NOT SET
* BCC - BRANCH CARRY CLEAR (C=0)
* BCS - BRANCH CARRY SET (C=1)
* BVC - BRANCH OVERVLOW CLEAR (V=0)
* BVS - BRANCH OVERFLOW SET (V=1)

               DSK   MORE       ;WRITE ASSEMBLED FILE TO DISK
               TYP   $06        ;$FF=SYSTEM, $06=BINARY
               ORG   $2000      ;ASSEMBLE START ADDRESS
*
* SYSTEM VARIABLES
*
PTR            EQU   $06        ;ONLY FREE 0-PAGE LOCATION
CH             EQU   $24        ;40-COL HORZ CURS POSITION
CV             EQU   $25        ;40-COL VERT CURS POSITION
PROMPT         EQU   $33        ;PROMPT CHARACTER
ZP_A1L         EQU   $3C        ;MONITOR GENERAL PURPOSE
ZP_A1H         EQU   $3D        ;MONITOR GENERAL PURPOSE
IN             EQU   $200       ;256-CHAR INPUT BUF
*
* SLOT 3 SCRATCHPAD RAM - TEXT PAGE 0 SCREEN HOLE
* http://yesterbits.com/media/books/apple/heiserman-1983-intermediate-level-apple-ii-handbook.pdf
*
OURCH          EQU   $057B      ;80-COL HORIZ CURSOR POSITION
OURCV          EQU   $05FB      ;VERTICAL CURSOR POSITION
*
* SUBROUTINES IN MONITOR ROM: $F800 - $FFFF
*
RDKEY          EQU   $FD0C      ;READS A CHARACTER
GETLN          EQU   $FD6A      ;READS A LINE, WITH PROMPT($33)
GETLN1         EQU   $FD6F      ;READS A LINE, NO PROMPT
CROUT          EQU   $FD8E
COUT           EQU   $FDED
PRBYTE         EQU   $FDDA
*
* SUBROUTINES IN BASIC.SYSTEM ROM:
*
GETBUFR        EQU   $BEF5      ;BCC=OKAY & A=HIBYTE OF BUF
                                ;BCS=FAIL & A=ERRCODE
                                ;X & Y ARE DESTROYED
FREEBUFR       EQU   $BEF8      ;FREE BUFFER
*
* PRODOS ENTRY POINT
*
PRODOS_MLI     EQU   $BF00      ;MACHINE LANG IFACE (MLI)
*
* MEMORY MAPPED I/O: $C000 - $CFFF
*
RD80VID        EQU   $C01F      ;<=128->40COL, >128->80COL
*
* PRODOS COMMAND CODES
*
GET_PREFIX     EQU   $C7
OPEN           EQU   $C8
READ           EQU   $CA
CLOSE          EQU   $CC
*
* PRODOS MLI PARAMETER COUNTS
*
GETPRFXPARMCNT EQU  1
OPENPARMCNT    EQU  3
READPARMCNT    EQU  4
CLOSEPARMCNT   EQU  1
*
* ASCII
*
CR             EQU   $0D        ;ASCII CARRIAGE RETURN
CR_HIBIT       EQU   $8D        ;CARRIAGE RET WITH HIGH BIT SET
*
* CONSTANTS
*
EOFERR         EQU   $4C        ;ERROR CODE FOR END-OF-FILE
MAXERCDE       EQU   $5A        ;LARGEST ERROR CODE
BUFSIZE        EQU   $00FF
SCR_HGHT       EQU   24         ;SCREEN HEIGHT
*
* DEBUGGING
*
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

MAIN           CLD                  ;CLEAR DECIMAL FLG, AVOID CRASH
               SET0  USRQUIT        ;INITIALIZE TO "NO"
               JSR   GET_SCRN_WDTH
*
* GET PREFIX TO SEE IF IT IS SET
*
               JSR   PRODOS_MLI
               DB    GET_PREFIX
               DA    GETPRFXPARMS
               BEQ   :CHKPREFIX
               JSR   GETPRFXERR
               JMP   :END
:CHKPREFIX     LDA   PREFIX
               CMP   #0
               BNE   :GETFILE
               JSR   CROUT
               PUTS  WARNING
               PUTS  NOPREFIXMSG
               JSR   CROUT
               JSR   CROUT
*
* GET FILE NAME
*
:GETFILE       PUTS  INFOLINE
               JSR   CROUT
:ASKFILE       PUTS  FILEPROMPT
               JSR   GETLN1     ;LENGTH IN X, CR AT END
               CPX   #0         ;IS THE LENGTH ZERO?
               BNE   :CONT0     ;USER JUST PRESSED RETURN
               JMP   :END

:CONT0         CPX   #1
               BNE   :CONT1
               LDA   #"?"
               CMP   IN
               BNE   :CONT1
               JSR   HELPINFO
               JMP   :ASKFILE
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
               JMP   :END       
:CONT2         STA   OBUFADDR+1 ;GETBUFR RETURNS HIBYTE IN A
               LDA   #0         ;PREPARE
               STA   OBUFADDR   ;LOBYTE IS 0 B/C ADDR OF PAGE
*
* OPEN FILE
*
               JSR   PRODOS_MLI
               DB    OPEN
               DA    OPENPARMS
               BEQ   :CONT3
               JSR   OPENERR
               JSR   FREEBUFR   ;CLEAN UP BEFORE TRY AGAIN
               JMP   :ASKFILE
*
* COPY FILE NUMBER FROM OPEN PARAMETERS TO READ AND CLOSE
*
:CONT3         LDA   OPENFNUM
               STA   READFNUM
               STA   CLOSFNUM
*
* GET BUFFER FOR READ OPERATION FROM BASIC.SYSTEM
*
               LDA   #1         ;ONE 256 BYTE BUFFER
               JSR   GETBUFR    ;CALL BASIC.SYSTEM SUB
               BCC   :CONT4
               JSR   RBUFERR    ;CARRY SET MEANS ERROR
               JMP   :CLOSFILE
:CONT4         STA   RBADDR+1   ;STORE HI-BYTE
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
*
* Close file
*
:CLOSFILE      JSR   PRODOS_MLI ;CLOSE THE FILE
               DB    CLOSE  
               DA    CLOSEPARMS
               BEQ   :FREEOBUF
               JSR   CLOSERR
               
:FREEOBUF      JSR   FREEBUFR   ;FREE OPEN I/O BUFFER
:END           NOP
               RTS

********************************
*                              *
* END OF MAIN - PROGRAM EXIT   *
*                              *
********************************

********************************
*                              *
* DETERMINE IF SCREEN IS IN    *
* 40 COL OR 80 COL MODE        *
*                              *
********************************
GET_SCRN_WDTH   LDA     #128
                CMP     RD80VID         ;RD80VID <= 128 -> 40 COL
                BPL     :EIGHTY_COLUMNS ;128 < RD80VID  -> 80 COL
                LDA     #40
                STA     SCR_WDTH
                JMP     :END
:EIGHTY_COLUMNS LDA     #80
                STA     SCR_WDTH
:END            NOP
                RTS

********************************
*                              *
* PROGRAM INFO AND HELP        *
*                              *
********************************
HELPINFO       JSR   CROUT
               PUTS  INFO0
               JSR   CROUT
               PUTS  INFO1
               JSR   CROUT
               PUTS  INFO2
               JSR   CROUT
               PUTS  INFO3
               JSR   CROUT
               PUTS  INFO4
               JSR   CROUT
               JSR   CROUT
               RTS

********************************
*                              *
* HANDLE ERROR WHEN GETTING    *
* PREFIX                       *
*                              *
********************************
GETPRFXERR     STA   ERRCODE
               PUTS  GETPRFXERRMSG
               LDA   #":"
               JSR   COUT
               LDA   ERRCODE
               JSR   ERRPROC
               RTS

********************************
*                              *
* HANDLE ERROR WHEN OPENING    *
* FILE                         *
*                              *
********************************
OPENERR        STA   ERRCODE
               PUTS  OERRMSG
               PUTS  FILENAME
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
PRSTRBYTES  PUSHY
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
               SET1  LINEIDX    ;POSITION IN LINE
:LOOP          JSR   PRODOS_MLI ;CALL PRODOS TO READ FILE
               DB    READ       ;SPECIFY PRODOS READ COMMAND
               DA    READPARMS  ;READ PARAMETERS
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

WRITEBUF       PUSHY
               LDY   #0         ;INIT CHAR COUNTER VARIABLE
:LOOP          CPY   READCNT    ;COMPARE TO MAX CHARS
               BEQ   :ENDLOOP
               LDA   (ZP_A1L),Y ;GET CHAR FROM BUFFER
               ORA   #%10000000 ;TURN ON HIGH BIT FOR PRINTING
               JSR   WRITECHAR  ;COUT PRESERVES ACCUM
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
* WRITE CHAR TO SCREEN         *
* CLIPS TO SCREEN WIDTH        *
* PRESERVES ACCUMULATOR        *
*                              *
********************************
WRITECHAR   STA     CHAR
            LDA     LINEIDX
            CMP     SCR_WDTH    ;COMPARE WITH SCREEN WIDTH
            BPL     :OFF_SCR    ;DON'T PRINT IF OFF SCREEN
            LDA     CHAR
            JSR     COUT
            JMP     :DONE
:OFF_SCR    INC     LINEIDX     ;TODO - THIS IS WRONG!
            LDA     CHAR
            CMP     #CR_HIBIT
            BNE     :DONE
            JSR     CROUT
:DONE       LDA     CHAR
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
               BEQ   :QUITTING  ;NO RECOGNIZED INPUT
               CMP   #"q"
               BEQ   :QUITTING
               JMP   :LOOP
:QUITTING      SET1  USRQUIT
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

INFOLINE       STR   "ENTER [?] FOR PROGRAM INFO AND HELP"
INFO0          STR   "MORE - PAGES THROUGH TEXT FILE"
INFO1          STR   "COPYRIGHT (C) 2024 BILL CHATFIELD"
INFO2          STR   "DISTRIBUTED UNDER THE GPL VERSION 3"
INFO3          STR   "https://github.com/gungwald/prodos-more"
INFO4          STR   "PRESS RETURN TO QUIT"
FILEPROMPT     STR   "FILE:"
ERRTXT         STR   "ERROR:"
FILENAME       DS    $FF
PREFIX         DS    64
ERRCODE        DS    1
LINENUM        DS    1
LINEIDX        DS    1
CHAR           DS    1
BAR            STR   '[RET] LINE  [SPC] PAGE  [Q]UIT'
USRQUIT        DS    1
BUFCHAR        DS    1
USRCHAR        DS    1
SCR_WDTH       DS    1

PREFIXMSG   STR     "THE PREFIX IS "
WARNING     STR     'WARNING'
NOPREFIXMSG STR     ": NO PREFIX IS SET. YOU MUST ENTER THE FULL PATH TO THE FILE."
GETPRFXERRMSG STR   "CANNOT GET PREFIX"
OERRMSG     STR     "CANNOT OPEN "
CERRMSG     STR     "CANNOT CLOSE "
RBERRMSG    STR     "CANNOT CREATE READ BUFFER"
OBERRMSG    STR     "CANNOT CREATE FILE BUFFER"

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
* GET_PREFIX PARAMETERS
*
GETPRFXPARMS   DB    #GETPRFXPARMCNT
PREFIXADDR     DA    PREFIX
*
* OPEN PARAMETERS
*
OPENPARMS      DB    #OPENPARMCNT
               DA    FILENAME
OBUFADDR       DS    2
OPENFNUM       DS    1
*
* READ PARAMETERS
*
READPARMS      DB    #READPARMCNT
READFNUM       DS    1
RBADDR         DS    2
REQCNT         DW    BUFSIZE
READCNT        DS    2
*
* CLOSE PARAMETERS
*
CLOSEPARMS     DB    #CLOSEPARMCNT
CLOSFNUM       DS    1
*
* BUFFERS
*
* CONSUME ALL BYTES UP TO THE NEXT PAGE BOUNDRY
FILLER DS \,$00
* MUST START ON PAGE BOUNDRY
*OPENBUF DS 1024
*READBUF DS BUFSIZE
