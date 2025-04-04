      *This program prints a ship as json
      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.   JSONSHIP
       AUTHOR.        Nikolaj R Christensen
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *All data for a ship
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
                10 WS-SPEED-KN PIC 99V99.
                10 WS-BELT-ARMOUR-MM PIC 999V99.
                10 WS-DECK-ARMOUR-MM PIC 999V99.
                10 WS-MAIN-GUN-NR PIC 9(3).
                10 WS-MAIN-GUN-CALIBRE PIC 999V99.
                10 WS-SECONDARY-NR PIC 9(3).
                10 WS-SECONDARY-CALIBRE PIC 999V99.
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
       01 WS-UID-CALCULATION.
            05 WS-4-DIGIT PIC 9999 VALUE 0000.
            05 TYPE-STR-LEN PIC 9 VALUE 2.
            05 NAVY-STR-LEN PIC 9 VALUE 2.
      *Used for storing the results of numval
       01 WS-NUMBERS.
      *general purpose up to 4 digit number
      * Non zero-suppressed version
           05 WS-999 PIC 999.
      *Z suppresses leading zeros so '0109' becomes ' 109'
           05 WS-FORMAT-INTEGER PIC ZZZ9.
       01 STRING-LEN  PIC 9(2) VALUE 0.
       01 BUFFER PIC X(80).
       LINKAGE SECTION.
      *All data for a ship
       01 LK-SHIP.
           05 LK-UID PIC X(12).
           05 LK-OTHER-DATA.
                10 LK-NAVY PIC X(4).
                10 LK-TYPE PIC X(4).
                10 LK-IDNR PIC X(4).
                10 LK-NAME PIC X(20).
                10 LK-CLASS PIC X(20).
                10 LK-STATUS PIC X(11).
                10 LK-THEATRE PIC X(20).
                10 LK-FLEET PIC X(20).
                10 LK-FORMATION PIC X(25).
                10 LK-CAPTAIN PIC X(25).
                10 LK-SPEED-KN PIC Z9V99.
                10 LK-BELT-ARMOUR-MM PIC ZZ9V99.
                10 LK-DECK-ARMOUR-MM PIC ZZ9V99.
                10 LK-MAIN-GUN-NR PIC 9(3).
                10 LK-MAIN-GUN-CALIBRE PIC ZZ9V99.
                10 LK-SECONDARY-NR PIC 9(3).
                10 LK-SECONDARY-CALIBRE PIC ZZ9V99.
                10 LK-FIRE-CONTROL-CPU PIC X(20).
                10 LK-HEAVY-AA-NR PIC 9(3).
                10 LK-LIGHT-AA-NR PIC 9(3).
                10 LK-AA-CONTROL-CPU PIC X(20).
                10 LK-RADAR PIC X(20).
                10 LK-SONAR PIC X(20).
                10 LK-DEPTH-CHARGES PIC 9(3).
                10 LK-TORPEDOES PIC 9(3).
                10 LK-FIRST-AIRCRAFT-NR PIC 9(3).
                10 LK-FIRST-AIRCRAFT-MODEL PIC X(20).
                10 LK-SECOND-AIRCRAFT-NR PIC 9(3).
                10 LK-SECOND-AIRCRAFT-MODEL PIC X(20).
                10 LK-THIRD-AIRCRAFT-NR PIC 9(3).
                10 LK-THIRD-AIRCRAFT-MODEL PIC X(20).
                10 LK-FUTURE-DATA PIC X(139) VALUE SPACES.

       PROCEDURE DIVISION USING LK-SHIP.

           MOVE LK-SHIP TO WS-SHIP.
      *Print everything in a human readable format:
      *JSON (People are not going to be looking directly at the output)
           DISPLAY '  {'

           PERFORM GET-UID.
           DISPLAY '    "Id":' WS-UID.
           MOVE 4 TO STRING-LEN.
           CALL 'MKQUOTE' USING WS-NAVY, STRING-LEN RETURNING BUFFER.
           DISPLAY '    "Navy":' BUFFER .
           MOVE 4 TO STRING-LEN.
           CALL 'MKQUOTE' USING WS-TYPE , STRING-LEN RETURNING BUFFER.
           DISPLAY '    "Type":' BUFFER .
           MOVE 20 TO STRING-LEN.
           CALL 'MKQUOTE' USING WS-NAME , STRING-LEN RETURNING BUFFER.
           DISPLAY '    "Name":' BUFFER .
           MOVE WS-IDNR TO WS-FORMAT-INTEGER.
           DISPLAY '    "Pennant-nr":' WS-IDNR WS-FORMAT-INTEGER ','.
           MOVE 20 TO STRING-LEN.
           CALL 'MKQUOTE' USING WS-CLASS , STRING-LEN RETURNING BUFFER.
           DISPLAY '    "Class":' BUFFER .
           MOVE 11 TO STRING-LEN.
           CALL 'MKQUOTE' USING WS-STATUS , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "Status":' BUFFER .
           MOVE 20 TO STRING-LEN.
           CALL 'MKQUOTE' USING WS-THEATRE , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "Theatre":' BUFFER .
           MOVE 20 TO STRING-LEN.
           CALL 'MKQUOTE' USING WS-FLEET , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "Fleet":' BUFFER .
           MOVE 25 TO STRING-LEN.
           CALL 'MKQUOTE' USING WS-FORMATION , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "Formation":' BUFFER .
           MOVE 25 TO STRING-LEN.
           CALL 'MKQUOTE' USING WS-CAPTAIN , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "Captain":' BUFFER .
      *    Essentially remove leading 0's, but Cobol makes it bloody difficult
           DISPLAY '    "Speed":' WS-SPEED-KN ','.
           DISPLAY '    "BeltArmour":' WS-BELT-ARMOUR-MM ','.

           DISPLAY '    "DeckArmour":' WS-DECK-ARMOUR-MM ','.

           MOVE WS-MAIN-GUN-NR  TO  WS-FORMAT-INTEGER.
           DISPLAY '    "MainGunNr":' WS-FORMAT-INTEGER ','.
           DISPLAY '    "MainGunCalibre":' WS-MAIN-GUN-CALIBRE ','.
           MOVE WS-SECONDARY-NR TO WS-FORMAT-INTEGER.
           DISPLAY '    "SecondaryGunNr":' WS-FORMAT-INTEGER ','.
           DISPLAY '    "SecondaryGunCalibre":' WS-SECONDARY-CALIBRE ','
           .
           MOVE 20 TO STRING-LEN.
           CALL 'MKQUOTE' USING WS-FIRE-CONTROL-CPU , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "FireControlComputer":' BUFFER.
           MOVE WS-HEAVY-AA-NR TO WS-FORMAT-INTEGER.
           DISPLAY '    "HeavyAAGunNr":' WS-FORMAT-INTEGER ','.
           MOVE WS-LIGHT-AA-NR TO WS-FORMAT-INTEGER.
           DISPLAY '    "LightAAGunNr":' WS-FORMAT-INTEGER ','.

           MOVE 20 TO STRING-LEN.
           CALL 'MKQUOTE' USING WS-AA-CONTROL-CPU , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "AAControlComputer":'
             BUFFER.

           MOVE 20 TO STRING-LEN.
           CALL 'MKQUOTE' USING WS-RADAR , STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "RadarModel":' BUFFER .

           MOVE 20 TO STRING-LEN.
           CALL 'MKQUOTE' USING WS-SONAR, STRING-LEN
              RETURNING BUFFER.
           DISPLAY '    "SonarModel":' BUFFER .

           MOVE WS-DEPTH-CHARGES TO WS-FORMAT-INTEGER.
           DISPLAY '    "DepthCharges":' WS-FORMAT-INTEGER ','.
           MOVE WS-TORPEDOES TO WS-FORMAT-INTEGER.
           DISPLAY '    "Torpedos":' WS-FORMAT-INTEGER ','.

           DISPLAY '    "Aircraft":['
           IF WS-FIRST-AIRCRAFT-NR > 0 THEN
              CALL 'MKQUOTE' USING WS-FIRST-AIRCRAFT-MODEL ,
                 STRING-LEN RETURNING BUFFER.
           IF WS-FIRST-AIRCRAFT-NR > 0 THEN
              DISPLAY '      {'
              DISPLAY '        "model":'  BUFFER
              MOVE WS-FIRST-AIRCRAFT-NR TO WS-FORMAT-INTEGER
              DISPLAY '        "number":' WS-FORMAT-INTEGER
              DISPLAY '      }'
           END-IF
           IF WS-FIRST-AIRCRAFT-NR > 0 THEN
              IF WS-SECOND-AIRCRAFT-NR > 0 THEN
                   DISPLAY '    ,'
              ELSE IF WS-THIRD-AIRCRAFT-NR > 0 THEN
                   DISPLAY '    ,'.

           IF WS-SECOND-AIRCRAFT-NR > 0 THEN
              CALL 'MKQUOTE' USING WS-SECOND-AIRCRAFT-MODEL,
                 STRING-LEN RETURNING BUFFER.
           IF WS-SECOND-AIRCRAFT-NR > 0 THEN
              DISPLAY '      {'
              DISPLAY '        "model":'  BUFFER
              MOVE WS-SECOND-AIRCRAFT-NR TO WS-FORMAT-INTEGER
              DISPLAY '        "number":' WS-FORMAT-INTEGER
              DISPLAY '      }'
           END-IF
           IF WS-SECOND-AIRCRAFT-NR > 0 THEN
              IF WS-THIRD-AIRCRAFT-NR > 0 THEN
                   DISPLAY '    ,'.

           IF WS-THIRD-AIRCRAFT-NR > 0 THEN
              CALL 'MKQUOTE' USING WS-THIRD-AIRCRAFT-MODEL,
                 STRING-LEN RETURNING BUFFER.
           IF WS-THIRD-AIRCRAFT-NR > 0 THEN
              DISPLAY '      {'
              DISPLAY '        "model":'  BUFFER
              MOVE WS-THIRD-AIRCRAFT-NR TO WS-FORMAT-INTEGER
              DISPLAY '        "number":' WS-FORMAT-INTEGER
              DISPLAY '      }'
           END-IF
           DISPLAY '    ]'
           DISPLAY '  }'
           GOBACK.
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
           STRING '"' WS-NAVY(1:NAVY-STR-LEN)
                   WS-TYPE(1:TYPE-STR-LEN)
                   WS-4-DIGIT '",'
              SPACES
              DELIMITED BY SIZE INTO WS-UID.
