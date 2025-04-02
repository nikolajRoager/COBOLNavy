      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    LSSHVS
       AUTHOR.        Nikolaj R Christensen
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALLIED-SHIPS ASSIGN TO ALLSHPS
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS SHIP-UID
           FILE STATUS IS WS-FILE-STATUS.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  ALLIED-SHIPS.
       01 SHIP-RECORD.
      *UID is generated from navy, type, and id number
           05 SHIP-UID PIC X(12).
           05 SHIP-NAVY PIC X(4).
           05 SHIP-TYPE PIC X(4).
           05 SHIP-IDNR PIC X(4).
           05 SHIP-NAME PIC X(20).
           05 SHIP-CLASS PIC X(20).
           05 SHIP-STATUS PIC X(11).
           05 SHIP-THEATRE PIC X(20).
           05 SHIP-FLEET PIC X(20).
           05 SHIP-FORMATION PIC X(25).
           05 SHIP-CAPTAIN PIC X(25).
           05 SHIP-SPEED-KN PIC 99V99.
           05 SHIP-BELT-ARMOUR-MM PIC 999V99.
           05 SHIP-DECK-ARMOUR-MM PIC 999V99.
           05 SHIP-MAIN-GUN-NR PIC 999.
           05 SHIP-MAIN-GUN-CALIBRE PIC 999V99.
           05 SHIP-SECONDARY-NR PIC 999. 
           05 SHIP-SECONDARY-CALIBRE PIC 999V99.
           05 SHIP-FIRE-CONTROL-CPU PIC X(20).
           05 SHIP-HEAVY-AA-NR PIC 999.
           05 SHIP-LIGHT-AA-NR PIC 999.
           05 SHIP-AA-CONTROL-CPU PIC X(20).
           05 SHIP-RADAR PIC X(20).
           05 SHIP-SONAR PIC X(20).
           05 SHIP-DEPTH-CHARGES PIC 999.
           05 SHIP-TORPEDOS PIC 999.
           05 SHIP-FIRST-AIRCRAFT-NR PIC 999.
           05 SHIP-FIRST-AIRCRAFT-MODEL PIC X(20).
           05 SHIP-SECOND-AIRCRAFT-NR PIC 999.
           05 SHIP-SECOND-AIRCRAFT-MODEL PIC X(20).
           05 SHIP-THIRD-AIRCRAFT-NR PIC 999.
           05 SHIP-THIRD-AIRCRAFT-MODEL PIC X(20).
      *-------------------
       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS PIC XX.
       01 WS-END-OF-FILE PIC X value 'n'.
       01 SNL PIC 9.
       01 STL PIC 9.
       01 WS-9999 PIC 9999.
      *------------------
       PROCEDURE DIVISION.
      *------------------
       OPEN-FILES.
           OPEN INPUT ALLIED-SHIPS.
       PRINT-ALL.
           DISPLAY 'TESTING TEST TEST'
           PERFORM UNTIL WS-END-OF-FILE = 'Y'
               READ ALLIED-SHIPS
                   AT END
                       MOVE 'Y' TO WS-END-OF-FILE
                   NOT AT END
                       DISPLAY 'Ship ID: ' SHIP-UID
                       DISPLAY 'Ship Name: ' SHIP-NAME
                       DISPLAY 'Ship Type: ' SHIP-TYPE
                       DISPLAY '-------------------------'
               END-READ
           END-PERFORM
           
           CLOSE ALLIED-SHIPS.
           GOBACK.
