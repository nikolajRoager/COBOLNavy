      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    ADDSH
       AUTHOR.        Nikolaj R Christensen
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALLIED-SHIPS ASSIGN TO ALLSHPS
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS UID
           FILE STATUS IS WS-FILE-STATUS.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  ALLIED-SHIPS.
       01 SHIP-RECORD.
      *UID is generated from navy, type, and id number
           05 UID PIC X(12).
           05 OTHER-DATA.
                10 SHIP-NAVY PIC X(4).
                10 SHIP-TYPE PIC X(4).
                10 SHIP-IDNR PIC ZZZ9.
                10 SHIP-NAME PIC X(20).
                10 SHIP-CLASS PIC X(20).
                10 SHIP-STATUS PIC X(11).
                10 SHIP-THEATRE PIC X(20).
                10 SHIP-FLEET PIC X(20).
                10 SHIP-FORMATION PIC X(25).
                10 SHIP-CAPTAIN PIC X(25).
                10 SHIP-SPEED-KN PIC 99V99.
                10 SHIP-BELT-ARMOUR-MM PIC 999V99.
                10 SHIP-DECK-ARMOUR-MM PIC 999V99.
                10 SHIP-MAIN-GUN-NR PIC 9(3).
                10 SHIP-MAIN-GUN-CALIBRE PIC 999V99.
                10 SHIP-SECONDARY-NR PIC 9(3).
                10 SHIP-SECONDARY-CALIBRE PIC 999V99.
                10 SHIP-FIRE-CONTROL-CPU PIC X(20).
                10 SHIP-HEAVY-AA-NR PIC 9(3).
                10 SHIP-LIGHT-AA-NR PIC 9(3).
                10 SHIP-AA-CONTROL-CPU PIC X(20).
                10 SHIP-RADAR PIC X(20).
                10 SHIP-SONAR PIC X(20).
                10 SHIP-DEPTH-CHARGES PIC 9(3).
                10 SHIP-TORPEDOES PIC 9(3).
                10 SHIP-FIRST-AIRCRAFT-NR PIC 9(3).
                10 SHIP-FIRST-AIRCRAFT-MODEL PIC X(20).
                10 SHIP-SECOND-AIRCRAFT-NR PIC 9(3).
                10 SHIP-SECOND-AIRCRAFT-MODEL PIC X(20).
                10 SHIP-THIRD-AIRCRAFT-NR PIC 9(3).
                10 SHIP-THIRD-AIRCRAFT-MODEL PIC X(20).
      *Currently unussed, we might add more stuff
                10 SHIP-FUTURE-DATA PIC X(139).
      *-------------------
       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS PIC XX.
       01 WS-END-OF-FILE PIC X value 'n'.
       01 WS-UID-CALCULATION.
            05 WS-4-DIGIT PIC 9999 VALUE 0000.
            05 TYPE-STR-LEN PIC 9 VALUE 2.
            05 NAVY-STR-LEN PIC 9 VALUE 2.
       01 WS-SHIP.
           05 WS-UID PIC X(12).
           05 WS-OTHER-DATA.
                10 WS-NAVY PIC X(4).
                10 WS-TYPE PIC X(4).
                10 WS-IDNR PIC ZZZ9.
                10 WS-NAME PIC X(20).
                10 WS-CLASS PIC X(20).
                10 WS-STATUS PIC X(11).
                10 WS-THEATRE PIC X(20).
                10 WS-FLEET PIC X(20).
                10 WS-FORMATION PIC X(25).
                10 WS-CAPTAIN PIC X(25).
                10 WS-SPEED-KN PIC Z9V99.
                10 WS-BELT-ARMOUR-MM PIC ZZ9V99.
                10 WS-DECK-ARMOUR-MM PIC ZZ9V99.
                10 WS-MAIN-GUN-NR PIC 9(3).
                10 WS-MAIN-GUN-CALIBRE PIC ZZ9V99.
                10 WS-SECONDARY-NR PIC 9(3).
                10 WS-SECONDARY-CALIBRE PIC ZZ9V99.
                10 WS-FIRE-CONTROL-CPU PIC X(20).
                10 WS-HEAVY-AA-NR PIC 9(3).
                10 WS-LIGHT-AA-NR PIC 9(3).
                10 WS-AA-CONTROL-CPU PIC X(20).
                10 WS-RADAR PIC X(20).
                10 WS-SONAR PIC X(20).
                10 WS-DEPTH-CHARGES PIC 9(3).
                10 WS-TORPEDOES PIC 9(3).
                10 WS-FIRST-AIRCRAFT-NR PIC 9(3).
                10 WS-FIRST-AIRCRAFT-MODEL PIC X(20).
                10 WS-SECOND-AIRCRAFT-NR PIC 9(3).
                10 WS-SECOND-AIRCRAFT-MODEL PIC X(20).
                10 WS-THIRD-AIRCRAFT-NR PIC 9(3).
                10 WS-THIRD-AIRCRAFT-MODEL PIC X(20).
                10 WS-FUTURE-DATA PIC X(139) VALUE SPACES.
      *------------------
       PROCEDURE DIVISION.
      *------------------
       MAIN-PROCEDURE.
      *Output to write new entries, Input to check for duplicate keys
           OPEN I-O ALLIED-SHIPS.
           DISPLAY 'start'.
      *00, opened succesfullu, 97, opened, but not closed correctly last
           IF WS-FILE-STATUS NOT = '00' AND NOT = '97'
      * We don't need to close it, it is not open
      * File not found (35) triggered by opening empty vsam files
              IF WS-FILE-STATUS NOT = '35'
      *Other errors can not be fixed, sorry
                 DISPLAY 'ERROR: FILE OPEN ERROR-CODE:' WS-FILE-STATUS
                 GOBACK
              ELSE
      *Open as output
                 OPEN OUTPUT ALLIED-SHIPS
                 IF WS-FILE-STATUS NOT = '00' AND NOT = '97'
                   DISPLAY 'ERROR: FILE OPEN ERROR-CODE:' WS-FILE-STATUS
                   GOBACK
                 ELSE
                     PERFORM ADD-OR-UPDATE-SHIP
                     CLOSE ALLIED-SHIPS
                     GOBACK
                 END-IF
           ELSE
                PERFORM ADD-OR-UPDATE-SHIP
                CLOSE ALLIED-SHIPS
                GOBACK
           END-IF.

       ADD-OR-UPDATE-SHIP.
           PERFORM LOAD-SHIP
      *Try just uploading it, if it doesn't work, maybe the key exists
           MOVE WS-SHIP TO SHIP-RECORD
           WRITE SHIP-RECORD
           INVALID KEY
      *get the existing record and overwrite it then
               DISPLAY 'DUPLICATE KEY. UPDATING EXISTING RECORD...'
               READ ALLIED-SHIPS RECORD KEY UID
               INVALID KEY
      *I don't know if this is a thing which can even happen
                   DISPLAY 'ERROR: DUBLICATE RECORD COULD NOT BE LOADED'
                   GOBACK
               END-READ
      *Update the rest of the data, not the UID
               MOVE WS-OTHER-DATA TO OTHER-DATA
               REWRITE SHIP-RECORD
           END-WRITE.
      *Verify that stuff happened
           IF WS-FILE-STATUS = '00'
               DISPLAY 'UPDATED'
           ELSE
               DISPLAY 'ERROR: UPDATE FAILED WITH STATUS' WS-FILE-STATUS
           END-IF.

      *Read data from sysin to workspace, in practice this is oft defined in jcl
       LOAD-SHIP.
           DISPLAY 'Enter Navy, 4 chars: '.
           ACCEPT WS-NAVY.
           DISPLAY WS-NAVY '-> Navy'.
           DISPLAY 'Enter hull class type, 4 chars: '.
           ACCEPT WS-TYPE.
           DISPLAY WS-TYPE '-> Type'.
           DISPLAY 'Enter pennant number, 4 digits: '.
           ACCEPT WS-IDNR.
           DISPLAY WS-IDNR '-> Type'.
           PERFORM GET-UID.
           DISPLAY 'UID generated ' WS-UID.
           DISPLAY 'Enter ship name, 20 chars: '.
           ACCEPT WS-NAME.
           DISPLAY 'Enter ship class name, 20 chars: '.
           ACCEPT WS-CLASS.
           DISPLAY 'Enter ship status, 11 chars, one of the following: '
           DISPLAY '    Operational'.
           DISPLAY '    Repairing'.
           DISPLAY '    Refitting'.
           DISPLAY '    Resupplying'.
           DISPLAY '    Sunk'.
           DISPLAY '    Scrapped'.
           DISPLAY '    Missing'.
           ACCEPT WS-STATUS.
           DISPLAY 'Enter ship operating theatre, 20 chars '.
           ACCEPT WS-THEATRE.
           DISPLAY 'Enter ship fleet 20 chars'.
           ACCEPT WS-FLEET.
           DISPLAY 'Enter ship formation 25 chars'.
           ACCEPT WS-FORMATION.
           DISPLAY 'Enter captain name 25 chars'.
           ACCEPT WS-CAPTAIN.
           DISPLAY 'Enter max speed in knots (example 23.00)'.
           ACCEPT WS-SPEED-KN.
           DISPLAY
             'Enter main belt armour thickness mm (example 330.00)'.
           ACCEPT WS-BELT-ARMOUR-MM.
           DISPLAY
             'Enter deck armour thickness in mm (example 200.00)'.
           ACCEPT WS-DECK-ARMOUR-MM.
           DISPLAY 'Enter number of main gun (# of barrels)'.
           ACCEPT WS-MAIN-GUN-NR.
           DISPLAY 'Enter main gun calibre in mm (example 406.00)'.
           ACCEPT WS-MAIN-GUN-CALIBRE.
           DISPLAY 'Enter number of secondary gun (# of barrels)'.
           ACCEPT WS-SECONDARY-NR.
           DISPLAY 'Enter main gun calibre in mm (example 152.00)'.
           ACCEPT WS-SECONDARY-CALIBRE.
           DISPLAY 'Enter fire control computer model name 20 chars)'.
           ACCEPT WS-FIRE-CONTROL-CPU.
           DISPLAY
           'Enter number of heavy (>100mm) Anti Air guns (# of barrels)'
           .
           ACCEPT WS-HEAVY-AA-NR.
           DISPLAY
           'Enter number of light (<100mm) Anti Air guns (# of barrels)'
           .
           ACCEPT WS-LIGHT-AA-NR.
           DISPLAY
           'Enter anti-air fire control computer model name, 20 chars'.
           ACCEPT WS-AA-CONTROL-CPU.
           DISPLAY 'Enter surface search rader model name 20 chars'.
           ACCEPT WS-RADAR.
           DISPLAY 'Enter Sonar/Asdic model name 20 chars'.
           ACCEPT WS-SONAR.
           DISPLAY 'Enter number of depth charges, 3 digits'.
           ACCEPT WS-DEPTH-CHARGES.
           DISPLAY 'Enter number of torpedo tubes, 3 digits'.
           ACCEPT WS-TORPEDOES.
           DISPLAY 'Enter number of first aircraft model, 3 digits'.
           ACCEPT WS-FIRST-AIRCRAFT-NR.
           DISPLAY 'Enter name of first aircraft model, 20 chars'.
           ACCEPT WS-FIRST-AIRCRAFT-MODEL.
           DISPLAY 'Enter number of second aircraft model, 3 digits'.
           ACCEPT WS-SECOND-AIRCRAFT-NR.
           DISPLAY 'Enter name of second aircraft model, 20 chars'.
           ACCEPT WS-SECOND-AIRCRAFT-MODEL.
           DISPLAY 'Enter number of third aircraft model, 3 digits'.
           ACCEPT WS-THIRD-AIRCRAFT-NR.
           DISPLAY 'Enter name of third aircraft model, 20 chars'.
           ACCEPT WS-THIRD-AIRCRAFT-MODEL.
           MOVE SPACES TO WS-FUTURE-DATA.

      *Calculate the UID from navy, type and pennant number in ws
       GET-UID.
      *Similar to stoi() or atoi() in C++/C, we need to save to workspace first
      *Not zero-suppressed, so becomes 006 or whatever
           COMPUTE WS-4-DIGIT = FUNCTION NUMVAL(WS-IDNR).
      *Now count the length of the navy and type name
           MOVE 0 TO NAVY-STR-LEN
           INSPECT FUNCTION REVERSE(WS-NAVY)
              TALLYING NAVY-STR-LEN FOR LEADING SPACES.
           COMPUTE NAVY-STR-LEN = 4 - NAVY-STR-LEN.

           MOVE 0 TO TYPE-STR-LEN
           INSPECT FUNCTION REVERSE(WS-TYPE)
              TALLYING TYPE-STR-LEN FOR LEADING SPACES.
           COMPUTE TYPE-STR-LEN = 4 - TYPE-STR-LEN.

      *Make sure the id is empty, then move the strings into the uid
           MOVE SPACES TO WS-UID.
           STRING  WS-NAVY(1:NAVY-STR-LEN)
                   WS-TYPE(1:TYPE-STR-LEN)
                   WS-4-DIGIT
              SPACES
              DELIMITED BY SIZE INTO WS-UID.
