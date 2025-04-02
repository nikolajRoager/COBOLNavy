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
       01 SHIP-IDNR PIC 999 VALUE SPACES.
       

       LINKAGE SECTION.
       01  LK-NAVY-INPUT PIC XXXX.
       01  LK-TYPE-INPUT PIC XXXX.
       01  LK-IDNR-INPUT PIC XXX.
       01  LK-UID-OUTPUT PIC X(12).

       PROCEDURE DIVISION USING LK-NAVY-INPUT , LK-TYPE-INPUT ,
           LK-IDNR-INPUT RETURNING LK-UID-OUTPUT.
      *Move to local storage
           MOVE LK-NAVY-INPUT TO SHIP-NAVY.
           MOVE LK-TYPE-INPUT TO SHIP-TYPE.
      *Similar to stoi() or atoi() in C++/C, save id number to workspace
      *Not zero-suppressed, so becomes 006 or whatever
           COMPUTE SHIP-IDNR = FUNCTION NUMVAL(LK-IDNR-INPUT).
      *Get the size of the navy and type
           MOVE 0 TO SNL
           INSPECT FUNCTION REVERSE(SHIP-NAVY)
              TALLYING SNL FOR LEADING SPACES.
           COMPUTE SNL = 4 - SNL.
           MOVE 0 TO STL
           INSPECT FUNCTION REVERSE(SHIP-TYPE)
              TALLYING STL FOR LEADING SPACES.
           COMPUTE STL = 4 - STL.
      *Copy the result to output
           MOVE SPACES TO SHIP-ID.
           STRING SHIP-NAVY(1:SNL) SHIP-TYPE(1:STL) SHIP-IDNR SPACES
              DELIMITED BY SIZE INTO LK-UID-OUTPUT
           GOBACK.