      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    FINDSHP
       AUTHOR.        Nikolaj R Christensen
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALLIED-SHIPS ASSIGN TO ALLSHPS.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  ALLIED-SHIPS RECORDING MODE F.
      *Each ship is spread out unto 4 lines,
      *Because each line is at most 80 chars
       01 BUFFER-I PIC X(80).
      *
       WORKING-STORAGE SECTION.
      *
       01 STRING-LEN  PIC 9(2) VALUE 0.
       01 BUFFER PIC X(80).
       01 IDX PIC 9.
       01 SHIP-ID PIC X(12).
       01 SNL PIC 9.
       01 STL PIC 9.
       01 SHIP-IDNR-LEN PIC 9.
       01 SHIP-BUFFER.
           05 SHIP-PART OCCURS 5 TIMES PIC X(80).
      *Used for storing the results of numval
       01 WS-NUMBERS.
      *number of planes on ship, need 999 to compare to 0
           05 WS-PLANE-0 PIC 999.
           05 WS-PLANE-1 PIC 999.
           05 WS-PLANE-2 PIC 999.
      *general purpose up to 4 digit number
      * Non zero-suppressed version
           05 WS-999 PIC 999.
      *Z suppresses leading zeros so '0109' becomes ' 109'
           05 WS-ZZ9 PIC ZZ9.
           05 WS-ZZZ9 PIC ZZZ9.
           05 WS-2decimal PIC Z9.99.
           05 WS-3decimal PIC ZZ9.9.
       01 SHIP-DATA.
      *Read everything as strings, we will cast to numbers manually
      *When we have read the entire buffer
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
           05 SHIP-SPEED-KN PIC X(5) .
           05 SHIP-BELT-ARMOUR-MM PIC X(6).
           05 SHIP-DECK-ARMOUR-MM PIC X(6).
           05 SHIP-MAIN-GUN-NR PIC X(3).
           05 SHIP-MAIN-GUN-CALIBRE PIC X(6).
           05 SHIP-SECONDARY-NR PIC X(3).
           05 SHIP-SECONDARY-CALIBRE PIC X(6).
           05 SHIP-FIRE-CONTROL-CPU PIC X(21).
           05 SHIP-HEAVY-AA-NR PIC X(3).
           05 SHIP-LIGHT-AA-NR PIC X(3).
           05 SHIP-AA-CONTROL-CPU PIC X(20).
           05 SHIP-RADAR PIC X(20).
           05 SHIP-SONAR PIC X(20).
           05 SHIP-DEPTH-CHARGES PIC X(3).
           05 SHIP-TORPEDOES PIC X(3).
           05 SHIP-FIRST-AIRCRAFT-NR PIC X(3).
           05 SHIP-FIRST-AIRCRAFT-MODEL PIC X(20).
           05 SHIP-SECOND-AIRCRAFT-NR PIC X(3).
           05 SHIP-SECOND-AIRCRAFT-MODEL PIC X(20).
           05 SHIP-THIRD-AIRCRAFT-NR PIC X(3).
           05 SHIP-THIRD-AIRCRAFT-MODEL PIC X(20).
      *------------------
       PROCEDURE DIVISION.
      *------------------
       OPEN-FILES.
           OPEN INPUT  ALLIED-SHIPS.
      *
       READ-AND-PRINT-SHIPS.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
               READ ALLIED-SHIPS INTO SHIP-PART(IDX)
               END-READ
           END-PERFORM.


      *Copy the chars into this buffer, so everything is in the right location
           MOVE SHIP-BUFFER TO SHIP-DATA.
      *Get the id, and check if it matches


      *Similar to stoi() or atoi() in C++/C, we need to save to workspace first
      *Not zero-suppressed, so becomes 006 or whatever
           COMPUTE WS-999 = FUNCTION NUMVAL(SHIP-IDNR).

           MOVE 0 TO SNL
           INSPECT FUNCTION REVERSE(SHIP-NAVY)
              TALLYING SNL FOR LEADING SPACES.
           COMPUTE SNL = 4 - SNL.

           MOVE 0 TO STL
           INSPECT FUNCTION REVERSE(SHIP-TYPE)
              TALLYING STL FOR LEADING SPACES.
           COMPUTE STL = 4 - STL.

           MOVE SPACES TO SHIP-ID.
           STRING '"' SHIP-NAVY(1:SNL) SHIP-TYPE(1:STL) WS-999 '"'
              SPACES
              DELIMITED BY SIZE INTO SHIP-ID
      *Ok then print it
      *Print everything in a human readable format:
      *JSON (People are not going to be looking directly at the output)
           DISPLAY '  {'
           DISPLAY '    "Id":' SHIP-ID ','.
           MOVE 4 TO STRING-LEN.
           CALL 'MKQUOTE' USING SHIP-NAVY, STRING-LEN RETURNING BUFFER.
           DISPLAY '    "Navy":' BUFFER .
           MOVE 4 TO STRING-LEN.
           CALL 'MKQUOTE' USING SHIP-TYPE , STRING-LEN RETURNING BUFFER.
           DISPLAY '    "Type":' BUFFER .
           MOVE 20 TO STRING-LEN.
           CALL 'MKQUOTE' USING SHIP-NAME , STRING-LEN RETURNING BUFFER.
           DISPLAY '    "Name":' BUFFER .
           MOVE WS-999 TO WS-ZZ9.
           DISPLAY '    "Pennant-nr":' WS-ZZ9 ','.
           MOVE 20 TO STRING-LEN.
           CALL 'MKQUOTE' USING SHIP-CLASS , STRING-LEN RETURNING BUFFER.
           DISPLAY '    "Class":' BUFFER .
           MOVE 11 TO STRING-LEN.
           CALL 'MKQUOTE' USING SHIP-STATUS , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "Status":' BUFFER .
           MOVE 20 TO STRING-LEN.
           CALL 'MKQUOTE' USING SHIP-THEATRE , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "Theatre":' BUFFER .
           MOVE 20 TO STRING-LEN.
           CALL 'MKQUOTE' USING SHIP-FLEET , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "Fleet":' BUFFER .
           MOVE 25 TO STRING-LEN.
           CALL 'MKQUOTE' USING SHIP-FORMATION , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "Formation":' BUFFER .
           MOVE 25 TO STRING-LEN.
           CALL 'MKQUOTE' USING SHIP-CAPTAIN , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "Captain":' BUFFER .
      *    String to decimals, again save to workspace
           COMPUTE WS-2decimal = FUNCTION NUMVAL(SHIP-SPEED-KN).
           DISPLAY '    "Speed":' WS-2DECIMAL ','.
           COMPUTE WS-3decimal = FUNCTION NUMVAL(SHIP-BELT-ARMOUR-MM).
           DISPLAY '    "BeltArmour":' WS-3decimal ','.
           COMPUTE WS-3decimal = FUNCTION NUMVAL(SHIP-DECK-ARMOUR-MM).
           DISPLAY '    "DeckArmour":' WS-3decimal ','.
           COMPUTE WS-ZZ9 = FUNCTION NUMVAL(SHIP-MAIN-GUN-NR).
           DISPLAY '    "MainGunNr":' WS-ZZ9 ','.
           COMPUTE WS-3decimal = FUNCTION NUMVAL(SHIP-MAIN-GUN-CALIBRE).
           DISPLAY '    "MainGunCalibre":'
              WS-3decimal ','.
           COMPUTE WS-ZZ9 = FUNCTION NUMVAL(SHIP-SECONDARY-NR).
           DISPLAY '    "SecondaryGunNr":' WS-ZZ9 ','.
           COMPUTE WS-3decimal = FUNCTION
              NUMVAL(SHIP-SECONDARY-CALIBRE).
           DISPLAY '    "SecondaryGunCalibre":'
              WS-3decimal ','.

           MOVE 21 TO STRING-LEN.
           CALL 'MKQUOTE' USING SHIP-FIRE-CONTROL-CPU , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "FireControlComputer":' BUFFER.
           COMPUTE WS-ZZ9 = FUNCTION NUMVAL(SHIP-HEAVY-AA-NR).
           DISPLAY '    "HeavyAAGunNr":' WS-ZZ9 ','.
           COMPUTE WS-ZZ9 = FUNCTION NUMVAL(SHIP-LIGHT-AA-NR).
           DISPLAY '    "LightAAGunNr":'  WS-ZZ9 ','.

           MOVE 20 TO STRING-LEN.
           CALL 'MKQUOTE' USING SHIP-AA-CONTROL-CPU , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "AAControlComputer":'
             BUFFER.

           MOVE 20 TO STRING-LEN.
           CALL 'MKQUOTE' USING SHIP-RADAR , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "RadarModel":' BUFFER .

           MOVE 20 TO STRING-LEN.
           CALL 'MKQUOTE' USING SHIP-SONAR, STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "SonarModel":' BUFFER .

           COMPUTE WS-ZZ9 = FUNCTION NUMVAL(SHIP-DEPTH-CHARGES).
           DISPLAY '    "DepthCharges":' WS-ZZ9 ','.
           COMPUTE WS-ZZ9 = FUNCTION NUMVAL(SHIP-TORPEDOES).
           DISPLAY '    "Torpedos":' WS-ZZ9 ','.

           compute WS-PLANE-0 = FUNCTION NUMVAL(SHIP-FIRST-AIRCRAFT-NR)
           compute WS-PLANE-1 = FUNCTION NUMVAL(SHIP-SECOND-AIRCRAFT-NR)
           compute WS-PLANE-2 = FUNCTION NUMVAL(SHIP-THIRD-AIRCRAFT-NR)

           DISPLAY '    "Aircraft":['
           IF WS-PLANE-0 > 0 THEN
              CALL 'MKQUOTE' USING SHIP-FIRST-AIRCRAFT-MODEL ,
                 STRING-LEN RETURNING BUFFER.
           IF WS-PLANE-0 > 0 THEN
              DISPLAY '      {'
              DISPLAY '        "model":'  BUFFER
              MOVE WS-PLANE-0 TO WS-ZZ9
              DISPLAY '        "number":' WS-ZZ9
              DISPLAY '      }'
           END-IF
           IF WS-PLANE-0 > 0 THEN
              IF WS-PLANE-1 > 0 THEN
                   DISPLAY '    ,'
              ELSE IF WS-PLANE-2 > 0 THEN
                   DISPLAY '    ,'.

           IF WS-PLANE-1 > 0 THEN
              CALL 'MKQUOTE' USING SHIP-SECOND-AIRCRAFT-MODEL,
                 STRING-LEN RETURNING BUFFER.
           IF WS-PLANE-1 > 0 THEN
              DISPLAY '      {'
              DISPLAY '        "model":'  BUFFER
              MOVE WS-PLANE-1 TO WS-ZZ9
              DISPLAY '        "number":' WS-ZZ9
              DISPLAY '      }'
           END-IF
           IF WS-PLANE-1 > 0 THEN
              IF WS-PLANE-2 > 0 THEN
                   DISPLAY '    ,'.

           IF WS-PLANE-2 > 0 THEN
              CALL 'MKQUOTE' USING SHIP-THIRD-AIRCRAFT-MODEL,
                 STRING-LEN RETURNING BUFFER.
           IF WS-PLANE-2 > 0 THEN
              DISPLAY '      {'
              DISPLAY '        "model":'  BUFFER
              MOVE WS-PLANE-2 TO WS-ZZ9
              DISPLAY '        "number":' WS-ZZ9
              DISPLAY '      }'
           END-IF
           DISPLAY '    ]'


      *    DISPLAY '"Torpedoes":' '"' SHIP-FIRST-AIRCRAFT-NR '"'.
      *    05 PIC 999.
      *    05 SHIP-FIRST-AIRCRAFT-MODEL PIC X(20).
      *    05 SHIP-SECOND-AIRCRAFT-NR PIC 999.
      *    05 SHIP-SECOND-AIRCRAFT-MODEL PIC X(20).
      *    05 SHIP-THIRD-AIRCRAFT-NR PIC 999.
      *    05 SHIP-THIRD-AIRCRAFT-MODEL PIC X(20).

           DISPLAY '  }'

           GO TO READ-AND-PRINT-SHIPS.
