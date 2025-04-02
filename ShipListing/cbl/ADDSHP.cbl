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
           ACCESS MODE IS DYNAMIC
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
           05 OTHER-DATA PIC X(488).
      *     05 SHIP-NAVY PIC X(4).
      *     05 SHIP-TYPE PIC X(4).
      *     05 SHIP-IDNR PIC X(4).
      *     05 SHIP-NAME PIC X(20).
      *     05 SHIP-CLASS PIC X(20).
      *     05 SHIP-STATUS PIC X(11).
      *     05 SHIP-THEATRE PIC X(20).
      *     05 SHIP-FLEET PIC X(20).
      *     05 SHIP-FORMATION PIC X(25).
      *     05 SHIP-CAPTAIN PIC X(25).
      *     05 SHIP-SPEED-KN PIC X(5) .
      *     05 SHIP-BELT-ARMOUR-MM PIC X(6).
      *     05 SHIP-DECK-ARMOUR-MM PIC X(6).
      *     05 SHIP-MAIN-GUN-NR PIC X(3).
      *     05 SHIP-MAIN-GUN-CALIBRE PIC X(6).
      *     05 SHIP-SECONDARY-NR PIC X(3).
      *     05 SHIP-SECONDARY-CALIBRE PIC X(6).
      *     05 SHIP-FIRE-CONTROL-CPU PIC X(21).
      *     05 SHIP-HEAVY-AA-NR PIC X(3).
      *     05 SHIP-LIGHT-AA-NR PIC X(3).
      *     05 SHIP-AA-CONTROL-CPU PIC X(20).
      *     05 SHIP-RADAR PIC X(20).
      *     05 SHIP-SONAR PIC X(20).
      *     05 SHIP-DEPTH-CHARGES PIC X(3).
      *     05 SHIP-TORPEDOES PIC X(3).
      *     05 SHIP-FIRST-AIRCRAFT-NR PIC X(3).
      *     05 SHIP-FIRST-AIRCRAFT-MODEL PIC X(20).
      *     05 SHIP-SECOND-AIRCRAFT-NR PIC X(3).
      *     05 SHIP-SECOND-AIRCRAFT-MODEL PIC X(20).
      *     05 SHIP-THIRD-AIRCRAFT-NR PIC X(3).
      *     05 SHIP-THIRD-AIRCRAFT-MODEL PIC X(20).
      *-------------------
       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS PIC XX.
       01 WS-END-OF-FILE PIC X value 'n'.
      *------------------
       PROCEDURE DIVISION.
      *------------------
       OPEN-FILES.
           OPEN I-O ALLIED-SHIPS.
           
           MOVE 'RNBB29'                TO UID. 
           MOVE 'STUPID HA HA NOT WORK' TO OTHER-DATA. 
      *     MOVE 'RN'                          TO SHIP-NAVY. 
      *     MOVE 'BB'                          TO SHIP-TYPE. 
      *     MOVE '29'                          TO SHIP-IDNR. 
      *     MOVE 'HMS Rodney'                  TO SHIP-NAME. 
      *     MOVE 'Nelson Class'                TO SHIP-CLASS.
      *     MOVE 'Repairing'                   TO SHIP-STATUS.
      *     MOVE 'North-East Atlantic'         TO SHIP-THEATRE. 
      *     MOVE 'Home Fleet'                  TO SHIP-FLEET. 
      *     MOVE '2nd Battle Squadron'         TO SHIP-FORMATION. 
      *     MOVE 'F H G Dalrymple-Hamilton'    TO SHIP-CAPTAIN. 
      *     MOVE '23.00'              TO SHIP-SPEED-KN.
      *     MOVE '330.00'             TO SHIP-BELT-ARMOUR-MM. 
      *     MOVE '200.00'             TO SHIP-DECK-ARMOUR-MM. 
      *     MOVE '009'                TO SHIP-MAIN-GUN-NR. 
      *     MOVE '406.00'             TO SHIP-MAIN-GUN-CALIBRE. 
      *     MOVE '009'                TO SHIP-SECONDARY-NR. 
      *     MOVE '152.00'             TO SHIP-SECONDARY-CALIBRE. 
      *     MOVE 'A.F.C.T. Mark I'    TO SHIP-FIRE-CONTROL-CPU. 
      *     MOVE '006'                TO SHIP-HEAVY-AA-NR. 
      *     MOVE '008'                TO SHIP-LIGHT-AA-NR. 
      *     MOVE 'H.A.C.S. Mark I'    TO SHIP-AA-CONTROL-CPU. 
      *     MOVE 'Type 79Y'           TO SHIP-RADAR. 
      *     MOVE 'None'               TO SHIP-SONAR. 
      *     MOVE '000'                TO SHIP-DEPTH-CHARGES. 
      *     MOVE '002'                TO SHIP-TORPEDOES. 
      *     MOVE '001'                TO SHIP-FIRST-AIRCRAFT-NR. 
      *     MOVE 'Supermarine Walrus' TO SHIP-FIRST-AIRCRAFT-MODEL. 
      *     MOVE '000'                TO SHIP-SECOND-AIRCRAFT-NR. 
      *     MOVE 'None'               TO SHIP-SECOND-AIRCRAFT-MODEL. 
      *     MOVE '000'                TO SHIP-THIRD-AIRCRAFT-NR.
      *     MOVE 'None'               TO SHIP-THIRD-AIRCRAFT-MODEL.
           
           DISPLAY 'HAS WRITHEN'
           WRITE SHIP-RECORD.

           CLOSE ALLIED-SHIPS.
           GOBACK.
