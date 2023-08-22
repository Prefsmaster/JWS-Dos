                .PROC ASM
                .RADIX 16

init            .EQU       0EE2              ; set interrupt vectors, timers and disk head to track 0
delay           .EQU       0EFF              ; delayloop in MONITOR
calibrate       .EQU       0F08
resul_7         .EQU       0F62              ; get extended status from FDC
track           .EQU       0F6C              ; ga naar track
go_track        .EQU       0F7D              ; ga naar track, (track voor seek komt uit lees/schrijfo
on_motor        .EQU       0F88              ; motor on
result          .EQU       0F90              ; get result from drive, stores max 7 bytes at addresses 6087-608D
voerfdc         .EQU       0FA5              ; send command to FDC
ready           .EQU       0F80              ;
dummy           .EQU       0FD6              ; ei reti instructions
resul_2         .EQU       0FD9
default         .EQU       0FE8              ; beschrijving disk action

overheen        .EQU       1454
JA              .EQU       1450
zoek            .EQU       1488
NEE             .EQU       14E0
INPUT           .EQU       1911
INSERT          .EQU       1C5A
NJ              .EQU       1D0E
CRLF            .EQU       1A2A
CHIN            .EQU       104D              ; Lees karakter in van toetsenbord.
COUT            .EQU       104A              ; Zet de waarde in de accumulator op het scherm.
UIT             .EQU       1AE1              ; Cursor OFF
NSCH            .EQU       3383              ; print 0-terminated string to screen

INI1            .EQU       6020              ; interrupt vector 1
INI2            .EQU       6022              ; interrupt vector 2
;
; currrent/working directory entry
;
header          .EQU       6030              ; filename
hea1            .EQU       6040              ; EXT
hea2            .EQU       6043              ; TYPE
hea3            .EQU       6044              ; LENGTE
hea4            .EQU       6046              ; TRANSFER
hea5            .EQU       6048              ; KANT
hea6            .EQU       6049              ; START SECTOR
hea7            .EQU       6045              ; AANT SECT
hea8            .EQU       6044              ; #of BYTES in last SECTOR
hea9            .EQU       604B              ; EIND SECTOR

descrip         .EQU       6070              ; beschrijving disc actie
RDTRACK         .EQU       6072              ; lees of schrijf een track
LEES_SCH        .EQU       6073              ; Lezen of schrijven
DRVNR           .EQU       6074              ; drive nummer/head
TRKNR           .EQU       6075
LHEAD           .EQU       6076
SECNR           .EQU       6077
;
drnr            .EQU       607E
trnr            .EQU       607F
CALDR           .EQU       6082
step_rate       .EQU       6085
sspave          .EQU       608E
verttab         .EQU       6094
INPBUFFER       .EQU       6261
BFFER           .EQU       0F000
buffer          .EQU       0F800
;

;#####################################################################################################################################
;                .INCLUDE P2.1
;#####################################################################################################################################

STRT            LD        (sspave),SP        ; SP NAAR FFFF
                LD        A,(bas1)           ; IS NW BASIC GEACTIVEERD?
                AND       A
                JP        Z,basic            ; (bas1)==0 dan 'basic' ; insert hook!

STAA            LD        HL,ddisk           ; tekst "system"
                CALL      NSCH               ; "system" Naar het SCHerm
                CALL      RES_2              ; ALLEEN DRIVE 1 EN 2 in A de waarde 1 of 2
                ADD       A,30               ; 1/2 + 30 = ASCII 1/2
                CALL      COUT               ; Character naar scherm
                CALL      CRLF               ; carriage ret en line feed
;
; wachten op een toetsindruk en bepalen welke routine is gekozen
;
STA1            CALL      CHIN               ;
                CP        88                 ; SH 2
                CALL      Z,copy             ; copieren van de gehele schijf
STA0            CP        8C                 ; OPN
                CALL      Z,DSAVE            ;
                CP        8D                 ; INL
                CALL      Z,DLOAD            ;
                CP        0F                 ; Wisregel
                CALL      Z,WISP             ; wissen van een programma van schijf
                CP        0C                 ; Wis scherm
                CALL      Z,WISS             ; wissen van de directory van de schijf
                CP        82                 ; DEF
                CALL      Z,FORMAT           ; formatteren van de schijf
                CP        80                 ; ZOEK
                CALL      Z,ZOK              ; tonen van de directory op het scherm
                CP        8B                 ; SH 2
                CALL      Z,Pcopy            ; copieren van een programma
                CP        83                 ; START
                CALL      Z,DRUN
                CP        85                 ; SH 5
                CALL      Z,CRUNCH
                CP        31                 ; '1'
                JR        Z,SWAP             ; verwisselen van drive 1 naar 2 of omgekeerd
                CP        32                 ; '2'
                JR        Z,SWAP
                CP        10                 ;
                JR        Z,DECTR            ; DECREMENT TRACKS
                CP        13                 ;
                JR        Z,INCTR            ; INCREMENT TRACKS
                RES       5,A                ; (reset bit 5) => VERWISSELEN VAN DRIVE TYPE Reset bit 5
                CP        53                 ; = 10010011
                JR        Z,SWAPS            ; SWAP SIDE (ss)
                CP        44                 ; = 10001000
                JR        Z,SWAPS            ; SWAP SIDE (ds)

STA9            LD        SP,(sspave)
                JP        6200               ; soft return in basic

SWAP            AND       0F                 ; VERWISSEL VAN DRIVE in A staat een 1 of 2
                LD        (SSYSTEM),A
SWAPR           LD        A,11               ; PIJLTJE OMHOOG
                CALL      COUT
                JR        STAA

SWAPS           LD        (ss_ds),A          ;zet mode
                JR        SWAPR
; ZETTR
; HL points to a record of 3 bytes
; first byte is number of tracks+1, followed by 2 bytes with an ASCII
; representation of the actual number of tracks. e.g. 24,'3','5'
ZETTR           LD        A,(HL)             ; get number of tracks
                LD        (AANTTR),A         ; store it ; TODO: 35H  = 53D ? why?
                LD        DE,TRIND           ; get pointer to 'nnTr Drive' string
                INC       HL                 ; HL points to first ascii character
                LD        A,(HL)             ; copy it into the string
                LD        (DE),A
                INC       HL                 ; next character
                INC       DE
                LD        A,(HL)             ; copy it
                LD        (DE),A
                JR        SWAPR              ; and exit

DECTR           LD        A,29               ; 0x29 = 41 assume 40 tracks
                LD        HL,AANTTR          ; compare with current # of tracks
                CP        (HL)
                JR        C,TR40             ; Carry set: current # is 80, decrement to 40
                JR        Z.TR35             ; Equal: current # is 40, decrement to 35
TR80            LD        HL,TR80D           ; otherwise current is 35, wrap to 80
                JR        ZETTR
TR40            LD        HL,TR40D           ; 40 track data block
                JR        ZETTR              ; and set
TR35            LD        HL,TR35D           ; 25 track data block
                JR        ZETTR              ; and set

INCTR           LD        A,29               ; 0x29 = 41 assume 40 tracks
                LD        HL,AANTTR
                CP        (HL)
                JR        C,TR35             ; carry set: current is 80, wrap to 35
                JR        Z,TR80             ; zero: current # is 40, increase to 80
                JR        TR40               ; otherwise current is 35, increase to 40

TR35D           .BYTE     24                 ; 35+1
                .ASCII    "35"
TR40D           .BYTE     29                 ; 40+1
                .ASCII    "40"
TR80D           .BYTE     51                 ; 80+1
                .ASCII    "80"
;
; WISSEN DIRECTORY
; - asks for confirmation
; - if 'Ja' (Yes) then 4kb starting at BFFER (0xF000-0xFFFF) is filled with 00's
; - and the clean directory written to disk
;
WISS            LD        HL,SCHIJF
                CALL      NSCH
                CALL      j_n

wise            XOR       A
                LD        HL,BFFER
                LD        (HL),A            ; zero first byte
                PUSH      HL                ; copy source HL
                POP       DE                ; to dest; DE
                INC       DE                ; one byte further
                LD        BC,0FFF           ; now copy FFF times
                LDIR                        ; to clear entire directory in memory (2*800h bytes = 1000h)
                CALL      dirweg            ; and save

DSAR            JP        return
;
; DSAVE entry: save program to disk
;
DSAVE           LD      HL,dsve             ; put save"
                CALL    NSCH                ; on screen
                CALL    NIput               ; get file name, location and length
                RET     C                   ; 'STOP' was pressed so abort
DSAX            CALL    direct              ; get directory from disk
DSA3            CALL    DSA0                ; fill header and search for file
                JP      C,TUSSEN            ; C means file exists. IX points to entry that matches the file.
                                            ; ask for overwrite. If yes, the existing file is removed
                                            ; and execution continues at DSA3 to try again.
                                            ; the filename may contain an '*' and can match more than 1 existing file!

DSA             CALL    DSA5                ; Try to find space on the disk (exits with error when disk is full)
                CALL    dirweg              ; save new directory to disk

                CALL    DSA7                ; Perform the save
                JR      DSAR                ; JP return

DSA0            LD     HL,(6405)            ; get end-address of BASIC program
                LD     DE,(625C)            ; get start-address of BASIC program (normally 0x6547)
                LD     (hea4),DE            ; store start of transfer adress in header
                XOR    A                    ; Zero A also clears carry.
                SBC    HL,DE                ; determine program length
                LD     (hea3),HL            ; store in header
                LD     DE,7A68              ; = 31.336 dec filesize is max 128 sectors with 244 bytes each.
                PUSH   HL                   ; save length
                SBC    HL,DE                ; larger than 31336 bytes?
                POP    HL
                JP     NC,STA9              ; When carry clear it is larger than 31336 bytes; exit to BASIC
                JP     vergelijk            ; file exists?

DSA5            CALL   ruimte               ; Try to find room for the file
; 'ruimte' returns:
; (hea6) # of sectors required for file, is 0 when no space (disk full)
; (hea5) side (0/1) where the file can be saved
; DE : last sector#+1 of preceding file;
; BC : length of file in sectors
; IY : points to directory entry to insert BEFORE
                 LD     HL,(hea6)           ; if number of sectors to write (hea6)
                 LD     A,H                 ; is equal to zero then the disk is full
                 OR     L
                 JR     Z,VL                ; issue an error!
                 DEC    DE                  ; decrement last sector of preceding file to point to correct destination sector
                 PUSH   DE                  ; copy starting sector
                 POP    HL                  ; to HL
                 ADD    HL,BC               ; add length of file in sectors
                 LD     (hea9),HL           ; store ending sector number in the directory-entry
                 LD     A,42                ; "B"
                 LD     (hea2),A            ; store filetype in directory entry
                 JP     MRUI                ; create room and insert entry into directory

DSA7             CALL   begin               ; in Strack begin track, in Ssec begin sector
                 CALL   INVUL
                 LD     HL,(hea4)
                 LD     (descrip),HL
                 LD     A,45                ; IO is WRITE action
                 LD     (LEES_SCH),A        ; store in disk IO command block
                 CALL   REST                ; perform IO (write)
                 JP     resul_7             ; read Disk Controller status
                  ; disk ful
VL               LD     E,24                ; error code
                 LD     HL,dvol             ; error message
                 JP     EERROR
;
; DLOAD load program from disk
;
DLOAD           LD      HL,dlad             ; place 'Load"' on screen
                CALL    NSCH
DRU1            CALL    NIput               ; get filename and load/save addresses
                RET     C                   ; stop was pressed: terminate
prun            CALL    direct              ; get directory
                CALL    start               ; start diskdrive (motor on, interrupts off)
                CALL    DLO0                ; try to find file if not found exits with error message
                ; file found!
                LD     HL,(625C)            ; get start of basic memory (usually &H6547)
                LD     (hea4),HL            ; store in transfer address of header info
                CALL   DL02                 ; check if file fits in available memory. CLEAR command may have changed this.
                                            ; if too large, DL02 exits with an error
                ; file fits!
                CALL   DL03                 ; read the file

                LD     DE,(hea4)            ; Start address
                PUSH   DE                   ; save
                CALL   25FB                 ; BASIC: UPDATE LINE POINTERS
                POP    DE                   ; start address
                LD     HL,(hea3)            ; length of file
                ADD    HL,DE                ; add to start and save in
                LD     (6405),HL            ; start of variable pointe
                LD     (6407),HL            ; start of array space pointer
                LD     (6409),HL            ; end of array space pointer
                JP     return               ; clean exit
;
; search directory for filename
; of not found, exit with error
;
DLO0            CALL   vergelijk            ; search directory for name
                JP     NC,NNGEV             ; No carry means file not found

DL01            PUSH   IX                   ; IX points to directory entry
                POP    HL                   ; copy to HL
                JP     NHEAD                ; fetch data from directory entry and return
;
; does the file fit in memory?
;
DL02            LD     HL,0032              ; Some room (50 bytes = 32h) needs to be reserved for the stack
                LD     DE,(hea4)            ; get start address
                ADD    HL,DE                ; add stack space
                LD     DE,(hea3)            ; get file length
                ADD    HL,DE                ; add it
                EX     DE,HL                ; move to DE
                LD     HL,(6258)            ; get end of free memory for BASIC program (as was set with CLEAR command)
                XOR    A                    ; clear carry
                SBC    HL,DE                ; compare max address with calculated end address
                JP     C,gehvol             ; if too large, print error and exit
                RET                         ; otherwise continue
;
; read the file
;
DL03            CALL   begin                ; INLEZEN
                CALL   INVUL                ; calculate track and sector to start reading from
                LD     HL,(hea4)            ; destination address
                LD     (descrip),HL         ; in transfer vector
                PUSH   HL                   ; save destination
                CALL   calibrate            ; MONITOR: go to first track
                POP    HL                   ; get destination
                LD     A, (SSYSTEM)         ; get side number
                CALL   inDRIVE              ; convert to drive number and store in disk command
                LD     A,46                 ; 0x46 = set command in READ mode
                LD     (LEES_SCH),A         ; set in disk command
                CALL   REST                 ; perform Disk IO on all sectors of file
                JP     resul_7              ; read controller state an exit
;
; Delete file
;
WISP            LD        HL,wissen         ; put 'WIS"' on screen
                CALL      NSCH
                CALL      NIput             ; prompt for filename
                RET       C                 ; 'Stop' was pressed

WB              CALL      direct            ; get directory
WISO            CALL      vergelijk         ; search directory for name
                JP        NC,NNGEV          ; NC means file not found, exit with error message

WISA            CALL      WIS1              ; remove dir entry pointed to by IX
                CALL      dirweg            ; save directory
                JP        return            ; and exit

WIS1            PUSH      IX                ; save pointer to directory entry
                LD        HL,0020           ; get pointer to next entry by adding 32 (20h = size of directory entry)
                POP       DE
                ADD       HL,DE
                EX        DE,HL             ; HL points tou source address, DE to destination address
                PUSH      IX                ; copy destination address
                POP       BC                ; to BC

                LD        HL,buffer         ; start of dir for side 2
                CALL      1CFD              ; BASIC routine at 1CFD = Compare HL and DE: Z if HL==DE, S&C when HL<DE, NZ&P&NC when HL>DE
                JR        NC,WIS2           ; is DE (source) < HL? then the move should END at HL-1 (F7FF)
                LD        HL,0000           ; otherwise end at FFFF (0-1) for the dir of side 2
WIS2            DEC       HL                ; subtract 1 from the end address
;
; Delete is a kind of LDIR/LDDR routine with DE as source, HL as end address
; of the block to move and destination in BC
;
DELETE          CALL    1CFD                ; 1CFD = Compare HL and DE: Z if HL==DE, S&C when HL<DE, NZ&P&NC when HL>DE
                LD      A,(DE)              ; get source byte
                LD      (BC),A              ; store at dest
                RET     Z                   ; was this the last one (HL==DE)? then return
                INC     BC                  ; next dest
                INC     DE                  ; next source
                JR      DELETE              ; and loop
; TODO ?? Why not a simple LDIR with source in HL, Dest in DE and count in BC?
; where count = 800h - (HL & 07FFh).
; and then fill the last 20 bytes at DE with 00 to prevent duplication of last dir entry!

;#####################################################################################################################################
;                .INCLUDE P2.2
;#####################################################################################################################################

CRUNCH          ID      HL,ccrunch          ; put "Crunch"
                CALL    NSCH                ; on screen
                CALL    j_n                 ; ask j/n (yes/No) if No it exits
                LD      SP,61FF             ; Ja (Yes) move stackpointer to increase $ space ??
                CALL    direct              ; get directory
                LD      HL,BFFER            ; start of directory entries side 1
CRU0            PUSH    HL                  ; save pointer to dir-entry
                LD      A,(HL)              ; first byte entry (1st byte of filename)
                AND     A
                JR      Z,CRU8              ; if first byte of dir entry == 0 this side of the disk is done
                CALL    NHEAD               ; copy data from entry to working area at 6030
                CALL    start               ; start drive
                CALL    calibrate           ; move to start track
                CALL    DLO0                ; prep load
                CALL    DLO3                ; load file
                CALL    return              ; stop drive
                CALL    vergelijk           ; find file
                CALL    WIS1                ; erase entry from directory
                CALL    vergelijk           ; Not sure why this is done
                CALL    start               ; start drive
                CALL    calibrate           ; move to start track
                CALL    DSA5                ; find room for file
                CALL    DSA7                ; and save file
                CALL    return              ; stop drive
                ;
                ; see if side of disk has changed
                ; bit convoluted
                ;
                POP     IX                  ; get pointer do dir entry from stack
                PUSH    IX                  ; and place back on stack
                LD      A,(IX+18)           ; Get side of the entry
                LD      E,A                 ; copy to E
                LD      A,(LHEAD)           ; Get side from disk IO command (previous file)
                POP     HL                  ; and get pointer to current dir entry
                XOR     E                   ; A xor E not zero? this means side was switched between files
                JR      NZ,CRU0             ; side was switched, start looking from the start!
                LD      DE,0020             ; increment entry pointer with 0020: next entry
                ADD     HL,DE               ;
                JR      CRU0                ; and proecess the file
;
CRU8            POP     DE                  ; Get pointer to entry off stack
                CALL    SS_DSS              ;
                JR      Z,CRU9              ; Single sided: we're done!

                LD      HL,buffer           ; pointer to start of 2nd side directory
                PUSH    HL                  ; push to stack
                XOR     A
                DEC     HL                  ; CARRY FORCERE
                SBC     HL,DE               ; DE contains pointer to last entry checked
                POP     HL
                JR      NC,CRU0             ; if last checked entry < buffer then did not check side 2 yet: start processing

CRU9            CALL    start               ; motor on prep interrupts etc.
                CALL    calibrate           ; to first track
                CALL    dirweg              ; save dir
                CALL    return              ; restore interrupts, motor off etc

CRUR            XOR     A                   ; clear A
                LD      HL,(625C)           ; get start of basic address
                LD      (HL),A              ; store two zeros at start of basci program
                INC     HL                  ; that indicates 'no program present'
                LD      (HL),A              ; to the interpreter
                INC     HL
                LD      (6405),HL           ; start of basic variables memory
                LD      (6407),HL           ; start of array memory
                LD      (6409),HL           ; end of array
                JR      6200                ; JP 1FC6 Clean reset of BASIC, program NOT cleared
                                            ; that was done just befor this call :-)
;
; copy directory entry to working area
; HL points to dir entry
; returns:
; HL points to working area
; flags: Z  if entry is empty
;        NZ if entry contains file info
;
NHEAD           LD      DE,header
                LD      BC,0020
                LDIR
                LD      HL,header
                LD      A,(HL)              ; check if entry contains file info
                AND     A
                RET

ZOK             CALL    11EC                ; DEEL-PRINT TODO ?? find out in BASCI disassembly
                CALL    UIT                 ; turn cursor off
ZEK             LD      HL,zoek             ; "Zoek" naar scherm
                CALL    NSCH
                LD      HL,0000         !
                LD      (TELSEC),HL         ; initialize free-sectors counter
                CALL    direct              ; read directory from disk
                CALL    SS_DSS              ; Check SS or DS; if DS add "Kant" (Side) after "Inhoud" (Contents)
                JR      Z,ZOEB              ; SS skip "Kant"
                LD      HL,kkant            ; Print "kant"
                CALL    NSCH
                LD      A,'1'               ; add '1'
                CALL    COUT
                CALL    CRLF                ; next line

ZOEB            LD      HL,BFFER            ; start with BFFER = Directory of side 1
ZOE0            PUSH    HL                  ; save pointer to current entry
                CALL    NHEAD               ; move to working area (located at header)
                JR      Z,Z0E3              ; No file at current entry
                CALL    ZOEA                ; print file info
                LD      DE,0020             ; next item is 20h/32d bytes further
                POP     HL
                ADD     HL,DE               ; add 20
                JR      ZOE0                ; continue listing
;
; listing of the directory for disk-side is done
; print # of free sectors and start next side if it is a DS disk
;
ZOE3            CALL    VRIJ                ; print Free sectors
                POP     HL                  ; clear stack
                CALL    SS_DSS              ; is er 2e kant
                RET     Z                   ; no, single sided, we're all done!
;
;
;
                EX      DE,HL               ; pointer to last entry in DE
                LD      HL,buffer           ; start of side 2 directory in HL
                PUSH    HL                  ; opslaan
                DEC     HL                  ; Force carry when side 2 is empty
                SBC     HL,DE               ; subtract last checked entry
                POP     HL                  ; get start address side 2 back
                RET     C                   ; if carry set then we're all done!
;
; print "Kant2"
;
                PUSH     HL
                LD       HL,kkant           ; print text "kant "
                CALL     NSCH
                LD       A,32               ; add '2'
                CALL     COUT
                CALL     CRLF               ; new line
                POP      HL
                JR       ZOE0               ; start loop to list entries for side 2
;
; print file information to screen
; HL Points to header info
; 16 bytes name               ; 16
; 3 bytes extension           ; 19
; 1 byte type                 ; 20
; WORD length                 ; 22
; WORD start address          ; 24
; BYTE  disk side             ; 25
; WORD start sector number    ; 27
; WORD ending sector number   ; 29
; 3 bytes unused              ; 32
; total 32 bytes
;
ZOEA            LD      A,82               ; Color green
                LD      B,11               ; filename
                CALL    162B               ; filename to screen, prints total of 17 characters: 1 extra for the color in A
                LD      A,82               ; Color green
                LD      B,04               ; extension (3 chars) to screen
                CALL    162B
                LD      A,86               ; Color blue
                CALL    19A9               ; to screen (why not COUT ??)
                LD      A,(HL)             ; file type (1 character)
                CALL    19A9               ; to screen (why not COUT ??)
                CALL    HHEA7              ; calculate # of sectors for file
                LD      L,A                ; # of sectors in HL
                LD      H,00
                PUSH    HL                 ; save
                LD      DE,(TELSEC)        ; add to sector counter
                ADD     HL,DE
                LD      (TELSEC),HL
                POP     HL                 ; get sectorcount back
                CALL    1601               ; print INT in HL to screen, aligned right TODO: ?? Basic routine
                LD      HL,CRLF            ; new line
                PUSH    HL                 ; put as return address on stack
                LD      HL,(hea3)          ; get # of bytes
                CALL    1601               ; file size in bytes to screen, right-aligned
                JP      1918               ; Test for stop key, terminates if pressed
                                           ; RETurns if not: will RET to CRLF and continue.
;
TELSEC          .WORD   0000
;
; Calculate #o free sectors and print to screen
;
VRIJ            LD      B,11               ; print 17 spaces  (11h)
                LD      A,20
VRI1            CALL    COUT
                DJNZ    VRI1

                LD      A,82               ; Color Green
                CALL    19A9               ; to screen (why not COUT ??)
                CALL    SECTEL             ; total # of sectors on 1 disk side (16*#tracks+1)!!
                XOR     A                  ; Clears carry
                LD      DE,(TELSEC)        ; get number of sectors in use
                SBC     HL,DE              ; calculate difference == nuber of free sectors
                LD      DE,0021            ; and subtract first 2 tracks (16 each)
                SBC     HL,DE
                PUSH    HL                 ; save # of free sectors
                LD      A,H                ; if HL == 0000 then this side of the disk is completely full
                OR      L
                JR      NZ,VRI2            ; not zero, print # of free sectors to screen
                LD      HL,VOL             ; otherwise print "vol" = 'full' to screen
                JR      VRI3
VRI2            LD      HL,vr              ; text "vrij" ('free') to screen
VRI3            CALL    NSCH
                LD      A,86               ; Color blue
                CALL    19A9
                POP     HL                 ; # of free sectors
                CALL    1601               ; print INT in HL to screen, aligned right
                LD      HL,0000
                LD      (TELSEC),HL        ; teller weer naar 0000
                RET

vr              .ASCII  "vrij"
                .BYTE   00
vol             .ASCII  "vol"
                .BYTE   00

DRUN            LD      HL,RUN            ; text 'run"' to screen
                CALL    NSCH
                CALL    DRU1              ; Jump into DLOAD to get filename and loading of file
                RET     C                 ; exit when'Stop' pressed
                JP      28D4              ; Execute program with BASIC RUN
;
; disable some keys, get name from user
; returns Carry set when 'Stop' key was pressed
; otherwise NC
;
NIput            LD      HL,(verttab)         ; get current key translation
                 PUSH    HL                   ; save on stack
                 LD      HL,toetsen           ; disable some keys during input
                 LD      (verttab),HL
                 CALL    INPUT                ; get user input without showing '?'
                 POP     HL
                 LD      (verttab),HL         ; enable all keys again
                 RET     C                    ; C indicates 'STOP' was pressed, if so we're done
                 LD      A,'"'                ; add closing quotes (") to input in input buffer
                                              ; TODO : weird 6261 vs 6260 (voor de naam op 6261 plaatsen??)
                 LD      HL,6260              ; 6260 is the basic input buffer
                 LD      (HL),A               ; place '"' at the first position
                 CALL    HINVUL               ; TOKENIZE, move to 6130 etc
                 OR      A                    ; Clear carry ('Stop' was not pressed)
                 RET
;
; HL contains 0x6260
;
HINVUL          CALL     2651                 ; BASIC tokenize function
                PUSH     HL
                LD       HL,HIN1              ; 60CD contains the address of the CSAVE, make it
                LD       (60CD),HL            ; jump to our disk IO code instead.
                POP      HL
                INC      HL                   ; make HL point to next byte in input buffer
                CALL     4E5D                 ; Call CSAVE in BASIC
                                              ; This will fill in the file header at 0x6130
                                              ; then jumps through CALL 60CC to actually perform the save
                                              ; and this will jump to HIN1 (the vector we just changed)
                                              ; It will not do much; we just want the header!
HIN1            PUSH     HL                   ;
                LD       HL,16F0              ; restore old CSAVE value
                LD       (60CD),HL
                CALL     VERPLAATS            ; copy filestart and length in the header and copy name to 6030
                POP      HL
                RET
;
;#####################################################################################################################################
;                .INCLUDE P2.3
;#####################################################################################################################################
;

;
; compare header with directory
;
vergelijk       LD      IX,BFFER            ; IX points to dir entry
ver1            PUSH    IX                  ; save start of dir entry
                LD      B,13                ; filename is 19 characters 16 name, 3 ext
                LD      HL,header           ; HL points to the header of the file to save
ver2            LD      A,(IX+00)           ; Get first byte of directory entry
                AND     A                   ; when zero then
                JR      Z,ver4              ; last header was checked (file not found)

                CP      (HL)                ; compare with name from header.
                JR      Z,ver3              ; equal, so keep comparing

                POP     IX                  ; Get start of dir entry back
                LD      DE,0020             ; and skip to next entry (skip 32 bytes)
                ADD     IX,DE
                JR      ver1                ; and loop

ver3            INC     IX                  ; next byte of dir entry
                INC     HL                  ; next byte of filename
                LD      A,(HL)              ; if filename contains an '*'
                CP      "*"
                JR      Z,ver               ; then stop comparing
                DJNZ    ver 2
ver             SCF                         ; Carry set: file found/exists
                POP     IX
                RET

ver4            POP     IX                  ; remove from stack
                CALL    SS_DSS              ; Single side?
                JR      Z,ver9              ; YES, file not found!
                PUSH    IX                  ; first dir buffer on stack
                LD      IX,buffer           ; 2nd buffer (F800) in IX
                PUSH    IX                  ; 2nd buffer on stack
                POP     HL                  ; 2nd buffer in HL
                DEC     HL                  ; HL now contains F7FF
                POP     DE                  ; first buffer in DE (F000)
                SBC     HL,DE               ; subtractis DHL smaller than
                JR      NC,ver1             ; NC means DE is smaller than HL, so keep searching
                                            ; end of dir reached fall through to NOT found
ver9            XOR     A                   ; a = 0, also clears carry: file NOT found
                RET
;
; Find a location where the file fits.
; for each side:
; - First try to find a fitting gap between two consecutive files.
; - If this can't be found, try to append to the end.
; returns:
; (hea6) # of sectors required for file, is 0 when no space (disk full)
; (hea5) side (0/1) where the file can be saved
; DE : last sector# of preceding file;
; BC : length of file in sectors
; IY : points to directory entry to insert BEFORE
;
ruimte          LD      IY,BFFER            ; BFFER points to directory of side 1
                LD      B,00
                CALL    HHEA7               ; A contains required # of sectors
                LD      C,A                 ; BC holds # of required sectors
                XOR     A                   ; start on side 1 (0)
 ruiO           LD      (hea5),A            ; save side in dir entry buffer
                LD      DE,0020             ; last sector used by directory
                JR      rui2

 ruil           LD      E,(IX*1B)           ; copy number of the last sector of the previous file
                LD      D,(IX*1C)           ; into DE

 rui2           INC     DE                  ; Destination is sector following the last one of the preceding file
                LD      L,(IY+19)           ; copy first sector of next file
                LD      H,(IY+1A)           ; into HL
                LD      A,H                 ; if HL == 0000 we reached end of directory
                OR      L                   ;
                JR      NZ,rui3             ; not zero, see if file fits

                CALL    SECTEL              ; Get total number of sectors on disk in HL
                XOR     A                   ; Clear carry
                SBC     HL,DE               ; HL becomes # of empty sectors at the end of the disk
                SBC     HL,BC               ; does the file fit? (subtract required sectors)
                JR      NC,rui9             ; NC means the file fits
                JR      ruiA                ; this side has no room, try next side (if present)

rui3            XOR     A                   ; Clear carry
                SBC     HL,DE               ; HL becomes # of empty sectors between the two files
                SBC     HL,BC               ; does the file fit? (subtract required sectors)
                JR      NC,rui9             ; NC means it fits

                PUSH    IY                  ; Make IX point to the previous directory entry
                POP     IX
                LD      DE,0020             ; and IY points now to the next directory entry
                ADD     IY,DE
                JR      ruil                ; loop

rui9            LD      (hea6),DE           ; hea6 contains starting sector #
                RET

ruiA            CALL    SS_DSS              ; Is the drive double sided?
                JR      Z,ru iB             ; no, so disk is full
                LD      A,(hea5)            ; which side is active?
                AND     A
                JR      NZ,ruiB             ; 2nd side was active: disk is full
                INC     A                   ; switch to next side (1) means side 2
                LD      IY, buffer          ; And look in the directory for side 2 located at buffer (F800h)
                JR      rui0                ; and start looking

ruiB            LD      HL,0000             ; 0000 in hea6 indicates disk full
                LD      (hea6),HL
                RET
;
; make room for, and insert, the current file's directory entry
; IY: points to the directory item to insert BEFORE
;
Mrui            LD        A,(hea5)          ; get side of the disk jeerste bekijken of het kant 1 of 2 is
                AND       A                 ; test
                JR        Z,Mrui            ;
                LD        BC,0FFFF          ; side 2 is stored up to 0FFFFh
                JR        Mru2              ;
Mru1            LD        BC,0F7FF          ; side 1`is stored up to 0f7FFh
Mru2            PUSH      BC                ; BC contains end address
                POP       HL                ; copy to HL
                LD        DE,0020           ; size of a dir entry = 32 bytes
                XOR       A                 ; clear carry
                SBC       HL,DE             ; move 20 bytes less
                PUSH      IY                ; pointer to entry to insert before
                POP       DE                ; copy tp DE
                CALL      INSERT            ; BASIC memmove routine TODO: how does this one work ??
                LD        HL,header         ; HL points to new dir entry
                LD        BC,0020           ; 32 bytes
                LDIR                        ; and copy
                RET
;
; saven van de directory
;
dirweg          CALL    start               ; head to track 0, motor on
                LD      A,02                ; set destination
                LD      (TRKNR),A           ; to track 2; location of directory
                LD      HL,BFFER            ; directory's location in memory
                LD      (descrip),HL        ; place in file descriptor
                LD      A,09                ; start sector (laatste 2K kant 1)
                LD      (SECNR),A
                LD      A,(SSYSTEM)         ; get active drive
                RES     2, A                ; reset bit 2: is set for side 2. we need drive #
                CALL    inDRIVE
                LD      (stdr),A            ; reset internal drive status for sense
                CALL    calibrate           ; MONITOR entry head to track 0
                CALL    go_track            ; go to track (2)
                CALL    sense               ; plakkertje aanwezig if write protected it exits to basic!
                                            ; if not protected we continue normally
                CALL    SS_DSS              ; Single (NZ) or double sided drive?
                JR      Z,vul4              ; Single sided
                LD      A,0C5               ; C5 means Double-sided
                LD      (LEES_SCH),A        ; Set it
                LD      E,10                ; 10H = 16 sectors
                JR      VU15
vul4            LD      A,045               ; SS
                LD      (LEES_SCH),A
                LD      E,08                ; 8H = 8 sectors
vul5            XOR     A
                LD      (LHEAD),A           ; use side 0 of the disk
                LD      B,A                 ; 0 means 256 bytes/sector
                CALL    lees                ; perform disk-io (write)
                JR      resul_7             ; get 7 status bytes

dsve            .BYTE   1D
                .ASCII  'save"'
                .BYTE   00

dlad            .BYTE   1D
                .ASCII  'load"'
                .BYTE   00

wissen          .BYTE   1D
                .ASCII  'wis"'
                .BYTE   00

formatt         .BYTE   1D,83
                .ASCII  "formatteren"
                .BYTE   00

dvol            .BYTE   1D,85
                .ASCII  "disk vol"
                .BYTE   00

ngev            .BYTE   1D,85
                .ASCII  "niet gevonden"
                .BYTE   00

ddisk           .BYTE   1D,82
ss_ds           .ASCII  "SS "
trind           .ASCII  "40Tr drive"
                .BYTE   20,00
;
; key translate table to disable 5 keys during input of filenames
;
toetsen          .BYTE   05                 ; 5 pairs (code, replacement)
                 .BYTE   20,07,70,07        ; code,CLEARSCREEN,code, tab
                 .BYTE   68,07,08,07        ; code, tab, code, tab
                 .BYTE   78,07              ; MEM
;
; Format disk
;
FORMAT          LD      HL,formatt          ; put text "Formatteren"
                CALL    NSCH                ; on screen
                CALL    j_n                 ; Ask "J/N" (yes/no) returns on J/yes, terminates on N/No
dformat         CALL    KEUZE               ; get choice for drive and side when DS drive is selected
                CP      03                  ; If Stop key pressed
                RET     Z                   ; terminate
                LD      HL,geduld           ; print "even geduld" (please wait) on screen
                CALL    NSCH
                LD      A,(IKANT)           ; get user's choice for side
                LD      (HCODE),A           ; place in Disk Format command
                LD      A,(IDRNR)           ; same with chosen drive
                LD      (FORDR),A
                CALL    start               ; drive on
                CALL    DFORMATT            ; perform format
                JP      return              ; drive off and exit
;
; Copy a side of a disk (technically not a DOS core function :-)
; will copy side 1 of drive 1 (FDC dr 1) to side 1 of drive 2 (FDC dr 4)
; and  copy side 2 of drive 1 (FDC dr 5) to side 2 of drive 2 (FDC dr 6)
;
copy            LD      HL,KOPIE            ; Text "copieren" (copy) to screen
                CALL    NSCH
                CALL    j_n                 ; Ask "J/N" (yes/no) returns on J/yes, terminates on N/No
                CALL    SS_DSS              ; is Disk SS or DS?
                JR      Z,cop3              ; don't ask for side when SS
                LD      HL,VKANT            ; Text "Kant: " to screen
                CALL    NSCH
copO            CALL    CHIN                ; read key
                CP      03                  ; Stop? then exit.
                RET     Z
                CP      '1'                 ; was '1' pressed?only accept side 1 or 2
                JR      Z,ccop1             ; accepted!
                CP      '2'                 ; was '2' pressed?
                JR      NZ,copO             ; no, only side 1 or 2 can be accepted so ask again!
ccop1           CALL    COUT                ; print choice
                SUB     '1'                 ; turn into 0 and 1
cop3            LD      (vvkant),A          ; source side
                AND     A
                JR      Z,cop2
                LD      A,04                ; side 2, adjust to FDC drive #
cop2            INC     A                   ; from drive is 1 or 5
                LD      (vdr),A             ; and store in vdr (fromdrive)
                INC     A                   ; dest drive is 1 higher
                LD      (ndr),A             ; store in ndr (todrive)
                LD      HL,geduld           ; text "even geduld" (please wait) to screen
                CALL    NSCH
                LD      A,01                ; dest drive, #2
                LD      (CALDR),A           ; in calibrate command
                CALL    start               ; start the drive and prep interrupts
                CALL    CCOPY               ; perform copy
                CALL    calibrate           ; head to track 00 for drive 1
                LD      A,01
                LD      (CALDR),A           ; head to track 00 for drive 2
                CALL    calibrate
                JP      return              ; motor off, restore interrupts and exit

;#####################################################################################################################################
;                .INCLUDE P2.4
;#####################################################################################################################################
;
; let user choose drive and (optionally) the side to format
;
KEUZE           LD      HL,DRIVE            ; text "drive nr.:" to screen
                CALL    NSCH
KEU1            CALL    CHIN                ; input 1 character
                CP      03                  ; if Stop key pressed
                RET     Z                   ; terminate
                CP      '1'                 ; drive 1?
                JR      Z,KEU2              ; ask side
                CP      '2'                 ; drive 2?
                JR      NZ,KEU1             ; no, get new input
KEU2            CALL    COUT                ; print choice to screen
                AND     0F                  ; turn into 1 or 2
                LD      (IDRNR),A           ; store drive choice
                CALL    SS_DSS              ; is drive SS?
                JR      Z,KEU5              ; if SS then side=1
                LD      HL,KANT             ; DS: ask for side to format
                CALL    NSCH                ; "Kant (1/2)?" to screen
KEU3            CALL    CHIN                ; input 1 character
                CP      03                  ; if Stop key pressed
                RET     Z                   ; then terminate
                CP      '1'                 ; Side 1?
                JR      Z,KEU4              ; print and finish
                CP      '2'                 ; side 2?
                JR      NZ,KEU3             ; no, get new input
KEU4            CALL    COUT                ; print choice to screen
                SUB     31                  ; turn into 0 or 1
KEU5            LD      (IKANT),A           ; store side choice
                AND     A                   ; was it side 2?
                RET     Z                   ; no. we're done
                LO      A,(IDRNR)           ; get drive number
                XOR     04                  ; correct for FDC side code
                LD      (IDRNR),A           ; and store
                RET
;
; read directory from disk and store at 0xF000
;
direct          CALL    RES_2               ; get drive number (only drives 1 & 2)
                LD      (CALDR),A           ; drnr naar calibrate
                CALL    inDRIVE             ; store drive number in action & seek
                CALL    start               ; motor on etc
                XOR     A
                LD      (LHEAD),A           ; 0 = side one of the disk
                INC     A                   ; LD A,02  TODO: more readable...
                INC     A
                LD      (TRKNR),A           ; directory is found on track 2
                CALL    calibrate           ; MONITOR entry: reset head to track 0
                CALL    go_track            ; MONITOR entry: move head to track 2
                LD      HL,BFFER            ; Set destination BFFER == 0xF000
                LD      (descrip),HL        ; in transfer adress
                LD      A,09                ; start reading from sector 09 (09-16 = last 2K of track)
                LD      (SECNR),A           ; place in disk command block (0x6077)
                CALL    SS_DSS              ; is single of double side
                JR      Z,DIR1              ; single side
                LD      A,0C6               ; double side so Multi track
                LD      (LEES_SCH),A
                LD      E,10                ; last 2K side 1 + first 2K kant 2 = 10H (16d) sectors
                JR      DIR2
DIR1            LD      A,046               ; single side
                LD      (LEES_SCH),A        ; set ID for disk IO command
                LD      E,08                ; read 2k = 8 sectors
DIR2            LD      B,00
                CALL    lees                ; perform read operation
                CALL    resul_7             ; get 7 status bytes from controller
                CALL    return              ; clean up
                RET
;
; Format 1 side of a disk
;
DFORMATT        LD      A,(IDRNR)           ; get user selected drive#
                LD      (drnr),A            ; place in seek command
                RES     4,A
                LD      (stdr),A            ; convert to actual drive number
                LD      (CALDR),A           ; calibrate (go to track 0) the right drive
                CALL    calibrate
                CALL    ZET_INTER           ; prepare interrupts
                CALL    sense               ; abort when write protection is on
                XOR     A                   ; start at track 00
FORI            PUSH    AF                  ; save track# and flags
                LD      (trnr),A            ; store in disk io command for 'seek'
                INC     A                   ; add 1
                LD      (TRCODE),A          ; store in internal active track
                CALL    track               ; MONITOR perform seek
                LD      HL,FORCODE          ; start address of format command
                CALL    ZET                 ; execute format command
                LD      E,10                ; 1 track has 16 sectors (10h)
                LD      A,01                ; sector 1
FOR2            LD      (SECODE),A          ; store internally
                LD      B,04                ; # of bytes to write in B
                LD      HL,TRCODE           ; 1st byte = track#, 2nd = HEad, 3rd&4th: sector#
                CALL    weg                 ; send 4 bytes to naar de DD, decrements E
                JR      Z,FOR3              ; when E == 0 all sectors are done, next track
                LD      A,(SECODE)          ; increment sector counter
                INC     A
                JR      FOR2                ; and loop
FOR3            CALL    resul_7             ; read FDC status
                LD      HL,AANTTR           ; get # of tracks (+1)
                POP     AF                  ; get current track#
                INC     A                   ; increment
                CP      (HL)                ; all done?
                JR      NZ,FOR1             ; no, do next track
                JP      calibrate           ; head to track 0 and exit
;
; copy side x of drive 1 to side x of drive 2
;
CCOPY           LD      HL,lsec             ; during copy always transfer complete sectors
                RES     1,(HL)              ; so we need to reset last sector flag
                LD      A,02                ; drive 2
                LD      (CALDR),A           ; place in calibrate command
                LD      A,(ndr)             ; get destination drive
                LD      (stdr),A            ; inint drive status
                CALL    calibrate           ; head to track 0
                CALL    sense               ; write protected? then Error and exit
                LD      A,01                ; start with sector 01
                LD      (SECNR),A
                LD      A,01                ; start with track 01
COP1            LD      (TRKNR),A           ; track to read/write
                LD      HL,BFFER            ; use the directory buffer area (F000-FFFF) to store 16 sectors of 1 track
                LD      (descrip),HL        ; transfer address for disk IO command
                LD      A,(vdr)             ; source drive #
                CALL    inDRIVE             ; set active drive
                LD      A,(vvkant)          ; get source side
                LD      (LHEAD),A           ; and read with the right head
                LD      A,42                ; read-action in command block
                LD      (LEES_SCH),A        ;
                CALL    go_track            ; seek to correct track
                LD      E,10                ; read 10H (=16) sectors
                LD      B,00                ; 0 = 256D bytes per sector
                CALL    lees                ; read all sectors
                CALL    resul_7             ; get FDC status
                LD      HL,BFFER            ; start of data
                LD      (descrip),HL
                LD      A,(ndr)             ; make destination drive
                CALL    inDRIVE             ; active drive
                LD      A,(vvkant)          ; get destination side
                LD      (LHEAD),A
                LD      A,45                ; write code in disk IO command
                LD      (LEES_SCH),A
                CALL    go_track            ; go to destination track
                LD      E,10                ; transfer 16 = 10h sectors
                LD      B,00                ; of 256D bytes
                CALL    lees                ; write all sectors
                CALL    resul_7             ; get FDC status
                LD      A,(AANTTR)          ; get # tracks per disk
                LD      B,A                 ; in B
                LD      A,(TRKNR)           ; current track
                INC     A                   ; move to next
                CP      B                   ; next track equal to tracks + 1?
                RET     Z                   ; Yes! we're done
                JR      COP1               ; no, copy next track
;
; start the drive
;
start            DI
                 LD     A,01                ; CTC uit ?
                 OUT    (88),A              ; ??
                 OUT    (89),A              ; ??
                 OUT    (8A),A              ; ??
                 OUT    (8B),A              ; ??
                 CALL   init                ; MONITOR routine will set vectors and timing also set head of drive 1 to track 0
                 JP     on_motor            ; turn motor on and return to caller

;
; clean exit from disk actionrestore interrupts and exit from disk action
;
return           LD     A,03                ; ??
                 OUT    (88),A
                 RST    20                  ; toetsenbord beschikbaar
                 XOR    A
                 OUT    (90),A              ; ??
                 EI
                 RET
;
; exit gracefully from disk IO action when something went wrong.
;
fout             CALL     return
                 LD       HL,error          ; text 'controleer disk'
                 LD       E,22
                 JP       ERRA              ; naar error routine

;#####################################################################################################################################
;                .INCLUDE P2.5
;#####################################################################################################################################

;
; Perform read or write
; header must be filled in correctly
; E contains # of sectors
;
lees            CALL    ZET_INTER           ; prepare interrupt vectors
                LD      HL,6072             ; disk command
                CALL    ZET                 ; send disk IO command to FDC
                LD      HL,(descrip)        ; get transfer address (destination for read, source for write)
                LD      A,(HL)              ; Read (0x42) or write (0x45)
                RES     7,A                 ; clear high bit
                CP      45                  ; writing?
                JR      Z,lee1              ; perform write

lee0            LD      A,E                 ; get # of sectors
                JR      Z,lee2              ; when zero then all sectors are read

                CP      01                  ; last sector?
                JR      NZ,leeA             ; no, read full sector
                LD      A,(lsec)            ; get last sector flag
                BIT     1,A                 ; if bit 0 is not set then
                JR      Z,leeA              ; read full sector
                LD      A,(hea8)            ; hea8 contains # of bytes to read/write for last sector
                LD      B,A
leeA            CALL    NI                  ; perform read
                JR      lee0                ; and loop

lee1            CALL    weg                 ; write full sector
                JR      NZ,lee1             ; repeatuntil E==0 (E is decremented in 'weg')

lee2            LD      A,0A
                OUT     (90),A              ; ??
                RET

ZET             CALL    voerfdc             ; send command to diskdrive
                LD      A,0C5               ; ??
                OUT     (89),A
                LD      A,001               ; ??
                OUT     (89),A
                LD      C,08D               ; port number in C
                LD      A,00D               ; motor on ??
                OUT     (90),A
                RET
;
; Read bytes from disk
; B : contains # of bytes to read (0=256)
; C : contains port to read from  (8D)
; HL: points to destination address for data
; E contains # of sectors
; returns Z when last sector was done (E=0)
; returns NZ if more to do (E!=0)
;
NI              IN       A,(90)             ; get status of FDC
                RRA                         ; Rotate bit 0 into Carry
                JR       NC,NI              ; no, so check again
                INI                         ; LD (HL),IN (C); INC HL, DEC B
                JR       NZ,NI              ; repeat until B==0
                DEC      E                  ; decrement sectorcounter
                RET
;
; Write bytes to disk controller
; B : contains # of bytes to write (0=256)
; C : contains port to write to
; HL: points to source address of data
; E contains # of sectors
; returns Z when last sector was done (E=0)
; returns NZ if more to do (E!=0)
;
weg             IN       A,(90)             ; get status of fdc
                RRA                         ; Rotate bit 0 into Carry
                JR       NC,weg             ; not ready, check again
                OUTI                        ; OUT C,(HL); INC HL; DEC B
                JR       NZ,weg             ; repeat until B==0
                DEC      E                  ; decrement sectorcounter
                RET
;
; Prepare interrupts for disk-IO
;
ZET_INTER       LD      HL,dummy              ; dummy ei/reti routine
                LD      (INI1),HL             ; on channel 0 (disk ready or CTC-timer)
                LD      HL,fout               ; In case of not ready exit gracefully
                LD      (INI2),HL             ; through interrupt vector of channel 1 (disk NOT ready)
                RET

DRIVE           .BYTE   1D,86
                .ASCII  "drive nr.:"
                .BYTE   00

KANT            .BYTE   1D,86
                .ASCII  "kant (1/2) :"
                .BYTE   00

geduld          .BYTE   1D,86
                .ASCII  "even geduld"
                .BYTE   1D,00

IDRNR           .BYTE   01
IKANT           .BYTE   00

FORCODE         .BYTE   06,4D
FORDR           .BYTE   01,01,10,32
                .BYTE   00

TRCODE          .BYTE   01
HCODE           .BYTE   00
SECODE          .BYTE   01,01

;#####################################################################################################################################
;                .INCLUDE P2.6
;#####################################################################################################################################
;
; determine starting track and sector for disk read/write action
; sector# is in hea6 and numbered consecutively per disk-side
; files start at sector 0x21
;
begin           XOR     A
                LD      HL,(hea6)           ; get startsector#
                LD      DE,0010             ; 16 (0x10h) sectors per track
                LD      C,01                ; C is een track teller
beg1            SBC     HL,DE
                JR      C,beg8              ; track (+1) found
                INC     C                   ; next track
                JR      beg1

beg8            ADD     HL,DE
                LD      A,L
                AND     A
                JR      NZ,beg9
                LDA     A,10
                DEC     C
beg9            LD      B,A                 ; Remainder sectors in Br e s t sectoren
                LD      (Strack),BC         ; store sector and track # in Strack, Ssec
                LD      A,(hea5)            ; get disk side
                LD      (SIDE),A            ; store in disk io data
                RET
; Better to use bit-shifting! TODO ??
;
SSYSTEM         .BYTE   01
Strack          .BYTE   00                  ; starting track for read/write action
Ssec            .BYTE   00                  ; starting sector for read/write action
SIDE            .BYTE   00                  ; disk side to read/write

;
; fill read/write data in the disk command block
; stored from 6072 onwards.
;
INVUL           CALL    RES2                ; get drive nr
                LD      (drnr),A            ; for seek
                LD      (CALDR),A           ; for calibrate
                LD      B,A                 ; save in B
                LD      A,(SIDE)            ; disk side 0 or 1
                LD      (LHEAD),A           ; and copy to head in command block
                AND     A
                JR      Z,INV1              ; bij kant 1 staat in A het juiste drive nr
                LD      A,B                 ; get drive#
                XOR     04                  ; add 4 side b numbers from 4-7
                LD      (SSYSTEM),A
INV1            LD      A,(SSYSTEM)
                LD      (DRVNR),A           ; drive nr to disk command block
                LD      A,(Ssec)            ; get start sector
                LD      (SECNR),A           ; store in command
                LD      A,(Strack)          ; get start track
                LD      (TRKNR),A           ; store in command
                RET

lsec            .BYTE   00                  ; last sector flag

;
; calculate nuber of tracks and sectors to read/write
; Set flag for last sector, important for read, not for write.
; input: HL contains transfer adddress?
;
REST            PUSH    HL                  ; save HL
                LD      HL,1sec             ; reset last sector flag
                RES     1,(HL)
                POP     HL
                CALL    HHEA7               ; calc # sectors to transfer. Result in A
                LD      (Stel),A            ; store in Stel (Sector counter)

RES0            LD      A,(Stel)            ; get sectors to go
                AND     A
                RET     Z                   ; if zero we're done

                LD      D,A                 ; copy foor counting loop
                LD      A,(Ssec)            ; get startsector
                DEC     A                   ; predecrement
                LD      E,00                ; prep counter that contains # of sectors to process for the call to 'lees' in E
$1              INC     A                   ; 1 more sector to write
                INC     E                   ; And also add 1 to # of sectors to write in 'lees'
                DEC     D                   ; 1 less sector to process
                JR      Z,$2                ; if last sector, set flag to tell 'lees'
                CP      10                  ; capacity of track (10h = 16 sectors) reached?
                JR      NZ,$1               ; no, add next sector
                ; do the disk transfer, increment track, reset starting sector # on track and zero sectors to write
$3              LD      A,0
                LD      (Stel),A            ; save sectors left to write
                CALL    go_track            ; MONITOR entry: move head to track stored in TRKNR
                XOR     A                   ; 0 = 256 and store in
                LD      B,A                 ; B: bytes per sector to transfer
                CALL    lees                ; perform Disk IO (E contains # of sectors)
                LD      (descrip),HL        ; save transfer address after this partial IO action
                CALL    RESUL_7             ; Monitor: read disk status
                LD      A,01                ; Move to next track, always start at sector 1
                LO      (6077),A            ; save starting sector in disk IO command block
                LD      (Ssec),A            ; and to our working variable
                LD      A,(TRKNR)           ; And move to next track
                INC     A                   ; incrementing TRKNR
                LD      (TRKNR),A           ; located in the disk io command block
                JR      RES0                ; and loop

$2              PUSH    HL                  ; set flag indicating that in the IO call
                LD      HL,lsec             ; the last block is being transferred
                SET     1,(HL)              ; by setting BIT 1 in lsec
                POP     HL
                JR      $3

RUN             .ASCII  'run"'
                .BYTE   00

error           .BYTE   1D,85
                .ASCII  "controleer disk"
                .BYTE   00
;
; check for write protect sticker.
; when writing this will generate an error and abort the request
;
sense           LD      HL,STATUS            ; drive status request command
                CALL    voerfdc              ; send to floppy controller
                LD      B,01                 ; we need only one result byte
                CALL    result               ; get the result
                LD      A,(6087)
                BIT     6,A                  ; is the disk write protected?
                RET     Z                    ; no, continue
                LD      HL,beveiligd         ; yes, so report and terminate
                LD      E,21                 ; JA
                JP      EERROR

beveiligd       .BYTE   1D,85
                .ASCII  "disk beveiligd"
                .BYTE   00

STATUS           .BYTE 02,04
stdr             .BYTE 00

KOPIE            .BYTE 1D,83
                 .ASCII "copieren"
                 .BYTE 00

VKANT            .BYTE 1D,86
                 .ASCII "kant:"
                 .BYTE 20,00

vdr              .BYTE 01
ndr              .BYTE 02
vvkant           .BYTE 00
;
; when during a save the file already exists the name is printed and
; a confirmation to overwrite is required.
; when confirmed, the existing file is removed from the directory.
; otherwise the operation is terminated.
;
TUSSEN           PUSH   IX                  ; save pointer to matching dir-entry
                 PUSH   IX                  ; and copy to HL
                 POP    HL
                 CALL   NHEAD               ; copy existing filename to new dir-entry
                 CALL   ZOEA                ; and print to screen
                 LD     HL,overheen         ; BASIC entry for the prompt "overschrijven"  (overwrite)
                 CALL   NSCH                ; print to screen
                 POP    IX                  ; restore pointer to existing dir-entry
                 CALL   j_n                 ; Get J/N (Yes/No) answer. Of no, the process ends!

TUS0             CALL   WIS1                ; remove entry from directory
                 JP     DSA3                ; and try to save again

;
; save drive number in appropriate locations
;
inDRIVE          LD       (DRVNR),A
                 LD       (drnr), A
                 RET

ccrunch          .BYTE    1D,83
                 .ASCII   "crunch"
                 .BYTE    00
;
jja              LD        HL,JA            ; pointer to "ja"
                 CALL      NSCH             ; to screen
                 RET

nnee             LD        HL,NEE           ; address of "nee"
                 LD        E,51             ; error code
                 JP        ERRA             ; print and exit

Stel             .BYTE     00               ; Sector counter for load/save

;
; Put J/N on-screen and wait for input.
; returns on J (Yes)
; jumps to ERROR on N (No)
;
j_n               LD    HL,NJ               ; pointer to text "? (J/N)"
                  CALL  NSCH                ; to screen
                  CALL  CHIN                ; get a character
                  CP    03                  ; "Stop" key (0x03) means 'N'
                  JP    Z,nnee
                  RES   5,A                 ; make lowercase
                  CP    "J"
                  JP    NZ,nnee             ; Only J is yes, all other keys are N (No)
                  JP    jja

gehvol          LD    HL,GGEHVOL            ; error message
                LD    E,25                  ; error code
                JP    EERROR                ; and exit with error
;
GGEHVOL         .BYTE   1D,85
                .ASCII  "geheugen vol"
                .BYTE   00
;
NNGEV           CALL    return              ; restore interrupts etc
                LD      E,20                ; error code in E
                LD      HL,ngev             ; pointer error message in HL
                JP      EERROR

kkant           .BYTE   1D,86
                .ASCII  "kant"
                .BYTE   20,00

;#####################################################################################################################################
;                .INCLUDE P2.7
;#####################################################################################################################################

;
; start van basic
;
basic           PUSH    AF                  ; save A and Flags
                LD      A,0C                ; clear screen
                CALL    COUT
                CALL    UIT                 ; cursor off
                POP     AF
                INC     A                   ; set flag that indicates
                LD      (bas1),A            ; basic is active
                LD       A,(6013)           ; contains 1 if running on P2000M
                AND      A
                JR       Z,bast             ; running on model T
;
; extra work for P2000M
;
                LD      HL,5800             ; start of video attribute memory
                LD      BC,07FF             ; clear from 5800-5fff (800h bytes)
                XOR     A                   ; store 0 at first location
                LD      (HL),A              ;
                PUSH    HL                  ; transfer to dest
                POP     DE
                INC     DE                  ; dest 1 byte further
                LDIR                        ; fill with 0

                LD      HL,6256             ; set new tab stop (~end of line)
                LD      (HL),56
bast            LD      HL,(60DD)           ; String save routine address
                LD      (sload),HL          ; make backup
                LD      HL,(60CD)           ; CSave address
                LD      (tsave),HL          ; make backup

                LD      HL,0DFFF            ; do a "CLEAR 0xDFFF" to protect the DOS code and data
                LD      (63B8),HL           ; store as highest addres that BASIC may use
                LD      DE,0032             ; preserve the standard 50 bytes of string space
                SBC     HL,DE
                LD      (6258),HL           ; store as start of string space
                LD      HL,DTABEL           ; address of key translate table
                LD      (6094),HL           ; place in BASIC vector so we can hook into the keyboard

                LD      HL,tokkel           ; get address of jmp intruction to Function-key parse routine
                LD      DE,60E9             ; Basic jump vector address
                LD      BC,0003             ; jmp is 3 bytes long
                LDIR                        ; copy our jump into table

                LD      A,02                ; limit memory to 32K; DOS and DIR need the other 16 KB
                LD      (605C),A
                ; find and remove ?usr(0) vector (was used to activate DOS)
                LD      B,0A                ; 10 vectors to check
                LD      DE,0005             ; vector that must be removed
                LD      IX,623E             ; start of USR-vector table
bas0            LD      L,(IX+00)           ; get lo byte
                LD      H,(IX+01)           ; and hi byte
                CALL    1CFD                ; 1CFD = Compare HL and DE: Z if HL==DE, S&C when HL<DE, NZ&P&NC when HL>DE
                JR      Z,bas2              ; found, remove!

                INC     IX
                INC     IX
                DJNZ    bas0                ; repeat
                JR      bas3                ; not found, just exit to basic

BAS2            LD      HL,289C             ; standard 'inactive' vector for USR
                LD      (IX+00),L           ; replace old vector with inactive one
                LD      (IX+01),H
                ; and done
bas3            LD      HL,INTRO            ; INTRO NAAR SCHERM
                JP      1FA9                ; Jump into basic to continue normal startup
                                            ; print "PHILIPS CASSETTE BASIC" etc.

bas1            .BYTE   00                  ; flag: 1 if DOS-basic hook is active, 0 if not

;#####################################################################################################################################
;                .INCLUDE P2.8
;#####################################################################################################################################
;
sload           .WORD   0000               ; 60DD save location
tsave           .WORD   0000               ; 60CD save location

RESUME          PUSH    HL                  ; restore old (cassette basic) jump address values
                LD      HL,(sload)          ; first the TODO ?? address
                LD      (60DD),HL           ;
                LD      HL,(tsave)          ; then the CSAVE (Tape save) address
                LD      (60CD),HL
                POP     HL
                RET

VERPLAATS       LD      HL,(6130)           ; get transfer address (usually 0x6547)
                LD      (hea4),HL           ; store in file descriptor area
                LD      HL,(6132)           ; get file length
                LD      (hea3),HL           ; store in file descriptor area
                LD      HL,6136             ; copy first 8 characters of filename
                LD      DE,header           ; into the header (starts at 0x6030)
                LD      BC,0008
                LDIR
                LD      HL,6147             ; copy 2nd 8 characters of the filename
                LD      C,08
                LDIR
                LD      HL,613E             ; and finally the extension (3 characters)
                LD      C,03
                LDIR
                RET

EERROR          PUSH    DE                  ; Error was encountered
                PUSH    HL
                CALL    RESUL_7             ; Read extended FDC status
                CALL    return              ; motor off, interrupts back
                POP     HL
                POP     DE
ERRA            LD      D,00
                LD      A,E                 ; illegal function call
                CP      05
                JR      Z,ERR0
                LD      (6252),A            ; Store in basic error code address
                CALL    NSCH                ; print error message
                JP      6200                ; soft return to basic

ERR0            CALL    RESUME              ; restore cassete basic jump vectors in (60DD en 60CD}
                JP      247D                ; BASIC error handling routine

;#####################################################################################################################################
;                .INCLUDE P2.9
;#####################################################################################################################################
;
SCHIJF          .BYTE   1D,86
                .ASCII  "disk wissen"
                .BYTE   00
;
tokkel          JP      TOK0
TOK0            PUSH    AF                  ; save A and flags
                LD      A,(DTABEL)          ; get # of entries in key translation table
                CP      09                  ; 9 means Disk mode is active, Cassette otherwise
                JR      NZ,CENTRY           ; Not 9: Cassette!
DENTRY          POP     AF                  ; get key in A
                CP      81                  ; Cassette Key?
                JR      NZ,DEN1             ; no, parse further
                CALL    TAPE                ; print "disk" on screen
                LD      A,05                ; Cassette has less key translations
                LD      (DTABEL),A          ; set translation table length to 9
                JP      6200                ; warm restart of BASIC

DEN1            CP      80                  ; 'Zoek' key?
                JR      Z,TOK9              ; Yes, handle it
                CP      8A                  ; less than 8A?
                RET     C                   ; do nothing
                CP      8F                  ; larger than 8F?
                RET     NC                  ; not a function key, do nothing
                CP      8E                  ; is it ?? TODO
                JR      NZ,TOK9             ; een disk functie toets
                CALL    STRT
                JR      swa1

swab            CALL    DISK                ; print "disk" on screen
                LD      A,09                ; set translation table length to 9
                LD      (DTABEL),A
swa1            JP      6200                ; warm start of BASIC

TOK9            LD      (sspave),SP         ; save stackpointer
                PUSH    AF                  ; and key
                CALL    RES2                ; make sure drive 1 or 2 only
                POP     AF                  ; Key back
                JP      STA0                ; jump to key parse code

CENTRY          POP     AF                  ; cassette entry
                CP      88
                RET     C                   ; less than 88: no function
                LD      HL,FUNCTABEL        ; een cassette functietoets 88-8E
                RET

TAPE            PUSH    HL                  ; save HL and AF
                PUSH    AF
                LD      HL,ttape            ; pointer to text "Tape"
                JR      DIsr                ; to screen and exit

DISK            PUSH    HL                  ; save HL and AF
                PUSH    AF
                LD      HL,Diskk            ; pointer to text "Disk"

DIsr            CALL    NSCH                ; print to screen
                POP     AF
                POP     HL
                RET                         ; and done

ttape           .BYTE   1D,83
                .ASCII  "tape"
                .BYTE   00

Diskk           .BYTE   1D,83
                .ASCII  "disk"
                .BYTE   00

;#####################################################################################################################################
;                .INCLUDE P2.10
;#####################################################################################################################################

;
;Function key table
;
DTABEL          .BYTE   09                  ; # of translation pairs
                ; Disk and Cassette keys
                .BYTE   7A,8E               ; DISK
                .BYTE   78,8A,8B,8D         ; MEM, INL
                .BYTE   88,8C,82,8B         ; OPN, SH 2
                ; Disk-Only
                .BYTE   8A,85,83,80         ; SH 5, ZOEK
                .BYTE   8D,83,5B,82         ; ART, DEF
;
; casette command jump table
;
FUNCTABEL       .WORD   breturn             ; 88
                .WORD   breturn             ; 89
                .WORD   USR                 ; 8A
                .WORD   EDIT                ; 8B
                .WORD   cSAVEE              ; 8C
                .WORD   cLOAD               ; 8D
                .WORD   swab                ; 8E

usrr            .BYTE   85,26,0B,27         ; keycodes for: ?usr(0)
                .BYTE   7E,2D,71,00
                .BYTE   00,00,0A

cloadd          .BYTE   1C,41,31,22         ; keycodes for: cload"
                .BYTE   0C,87,06

csave           .BYTE   1C,0B,22,1F         ; keycodes for: csave"
                .BYTE   24,87,06

editt           .BYTE   24,0C,46,25         ; keycodes for: edit
                .BYTE   04
;
; place keycodes for a command in the keyboard input buffer
;
USR             LD      HL,usrr             ; ?usr(0) command
                LD      C,0A                ; length
ldirr           LD      DE,6000             ; start of keyboard buffer
                LD      B,00                ; BC contains bytes to transfer
                LDIR                        ; copy them to the buffer
                LD      A,(HL)              ; Last byte is # of keys placed in the buffer
                LD      (600C),A            ; store in buffer length
breturn         RET

cLOAD           LD       HL,cloadd          ; cload" command
cas             LD       C,06               ; length
                JR       ldirr              ; place in buffer
cSAVEE          LD       HL,csave           ; csave" command
                JR       cas                ; same length as cload" commmand
EDIT            LD       HL,editt           ; edit command
                LD       C,04               ; length
                JR       ldirr              ; to buffer
;
; Calculate total number of sectors on one side of a disk
; Sectors = (tracks * 16)+1
; returns:
; HL : # of sectors on one side + 1
;
SECTEL          LD      A,(AANTTR)          ; get ## of tracks
                LD      B,A                 ; in B
                DEC     B                   ; AANTTR contains # tracks+1, so decrement
                PUSH    DE
                LD      DE,0010
                LD      HL,0001
SEC1            ADD     HL,DE
                DJNZ    SEC1                ; tellen
                POP     DE
                RET
; better
; LD A,(AANTTR)
; DEC A
; LD H,0
; LD L,A
; ADD HL,HL ; *  2
; ADD HL,HL ; *  4
; ADD HL,HL ; *  8
; ADD HL,HL ; * 16
; INC HL    ; +  1
; RET


; use only side A of the disk
; drive 1 side B = 5, drive 2 side B = 6 TODO: wat is dit???
;
RES2            LD      A,(SSYSTEM)
                RES     2,A
                LD      (SSYSTEM),A
                RET
;
; SS_DSS
; returns Z for single-sided, NZ for double-sided
;
SS_DSS          LD      A,(ss_ds)
                CP      "S"
                RET     NZ                  ; TODO: this can become a simple RET!!
                XOR     A                   ; Z is set *again*
                RET
;
; in hea8 staat de lengte van de file
; hea7 geeft als in hea8 00 staat het aantal te lezen of schrijven sectoren
; als hea8 != 00 dan moet er 1 sector meer gelezen of geschreven worden
; returns:
; A : # of sectors required for file
;
HHEA7           LD      A,(hea8)            ; lo byte of file size
                AND     A                   ; check for zero
                LD      A,(hea7)            ; hi byte contains # of full sectors
                RET     Z                   ; File size is multiple of 256 When lo byte == 0
                INC     A                   ; add extra sector for remainding bytes
                RET
;
; copy a (program)file from drive 1 to 2 or vice versa
;
Pcopy           LD      HL,PPCOPY           ; text 'copy"'
                CALL    NSCH                ; to screen
                CALL    DRU1                ; jump into dload: get name and load
                RET     C                   ; 'Stop' was pressed: abort
                LD      A,(SSYSTEM)         ; swap systen drive
                XOR     03                  ; by toggling bits
                LD      (SSYSTEM),A
                CALL    RES2                ; limit to drive 1 or 2
                CALL    DSAX                ; jump into dsave, filename is known TODO: what happens when filename contains an '*'???
                LD      A,(SSYSTEM)         ; go back to orignal drive
                XOR     03
                LD      (SSYSTEM),A
PcoR            JP      CRUR                ; jump to 'erase' BASIC program from memory and exit

PPCOPY          .BYTE   1D
                .ASCII  'copy"'
                .BYTE   00

INTRO           .BYTE   0C,86,8D            ; Clearscreen,Cyan, double height
                .ASCII  "MINIWARE DISK/CASSETTE BASIC"
                .BYTE   04,03,02,83         ; gotoxy(3,2),Yellow
                .ASCII  "Versie 1.0"
                .BYTE   04,05,02,86         ; gotoxy(5,2),geel ?? TODO is color correct??
                .BYTE   00
AANTTR          .BYTE   29                  ; # of tracks+1 (default = 41d)

                .END
