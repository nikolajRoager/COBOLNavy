      *This program creates the Unique ID of a ship, given the navy,
      *type, and pennant/hull number of the ship
      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.   GETUID
       AUTHOR.       Nikolaj R Christensen
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SHIP-NAVY PIC XXXX VALUE SPACES.
       01 SNL PIC 9 VALUE 0.
       01 SHIP-TYPE PIC XXXX VALUE SPACES.
       01 STL PIC 9 VALUE 0.
       01 SHIP-IDNR PIC 9999 VALUE 0000.
       01 SHIP-UID PIC X(12) VALUE SPACES.

       LINKAGE SECTION.
       01  LK-UID-OUTPUT PIC X(12).

       PROCEDURE DIVISION RETURNING LK-UID-OUTPUT.
           DISPLAY 'CALL'.
      *Some calculations
           MOVE 'RESULT      ' TO LK-UID-OUTPUT
           GOBACK.
