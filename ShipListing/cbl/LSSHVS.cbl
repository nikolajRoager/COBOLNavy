       IDENTIFICATION DIVISION.
       PROGRAM-ID. ReadVSAM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VSAM-FILE ASSIGN TO VSAMFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  VSAM-FILE DATA RECORD IS VSAM-RECORD.
       01  VSAM-RECORD.
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
                10 SHIP-BELT-ARMOUR-MM PIC ZZ9V99.
                10 SHIP-DECK-ARMOUR-MM PIC ZZ9V99.
                10 SHIP-MAIN-GUN-NR PIC 9(3).
                10 SHIP-MAIN-GUN-CALIBRE PIC ZZ9V99.
                10 SHIP-SECONDARY-NR PIC 9(3).
                10 SHIP-SECONDARY-CALIBRE PIC ZZ9V99.
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

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS     PIC XX.
       01  WS-EOF             PIC X VALUE 'N'.
       01  WS-START           PIC X VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT VSAM-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
              DISPLAY '{"success":0,'
              DISPLAY '"error":"File error ' WS-FILE-STATUS '"}'
              GOBACK.
        READ-FILE.
              DISPLAY '{"success":1,'
              DISPLAY '"error":"File error ' WS-FILE-STATUS '",'
              DISPLAY '"ships":['
           PERFORM UNTIL WS-EOF = 'Y'
               READ VSAM-FILE NEXT RECORD
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF WS-START NOT = 'Y'
                          DISPLAY ','
                       END-IF
                       MOVE 'N' TO WS-START
                       CALL 'JSONSHIP' USING VSAM-RECORD
              END-READ
           END-PERFORM.
              DISPLAY ']}'
           CLOSE VSAM-FILE.
           GOBACK.
