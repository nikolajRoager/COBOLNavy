      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    LSSHP
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
       01 SHIP-BUFFER.
           05 SHIP-PART OCCURS 5 TIMES PIC X(80).
      *Used for storing the results of numval
       01 WS-NUMBERS.
           05 WS-999 PIC 999.
           05 WS-9999 PIC 9999.
           05 WS-2decimal PIC 99.99.
           05 WS-3decimal PIC 999.9.
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
               AT END DISPLAY 'End of File' STOP RUN
               END-READ
           END-PERFORM.


      *Copy the chars into this buffer, so everything is in the right location
           MOVE SHIP-BUFFER TO SHIP-DATA.



      *Print everything in a human readable format:
      *JSON (People are not going to be looking directly at the output)
           DISPLAY '{'
      *Similar to stoi() or atoi() in C++/C, we need to save to workspace first
           COMPUTE WS-999 = FUNCTION NUMVAL(SHIP-IDNR).
           DISPLAY '"Id": "' SHIP-NAVY SHIP-TYPE WS-999 '",'.

           MOVE 4 TO STRING-LEN.
           CALL 'MKQUOTE' USING SHIP-NAVY, STRING-LEN RETURNING BUFFER.
           DISPLAY '"Navy":' '"' BUFFER '",'.
           DISPLAY '"Type":' '"' SHIP-TYPE '",'.
           DISPLAY '"Name":' '"' SHIP-NAME '",'.
           DISPLAY '"Pennant-or-hull-nr":' SHIP-IDNR.
           DISPLAY '"Class":' '"' SHIP-CLASS '",'.
           DISPLAY '"Status":' '"' SHIP-STATUS '",'.
           DISPLAY '"Theatre":' '"' SHIP-THEATRE '",'.
           DISPLAY '"Fleet":' '"' SHIP-FLEET '",'.
           DISPLAY '"Formation":' '"' SHIP-FORMATION '",'.
           DISPLAY '"Captain":' '"' SHIP-CAPTAIN '",'.
      *    String to decimals, again save to workspace
           COMPUTE WS-2decimal = FUNCTION NUMVAL(SHIP-SPEED-KN).
           DISPLAY '"Speed":' '"' WS-2DECIMAL '"'.
           COMPUTE WS-3decimal = FUNCTION NUMVAL(SHIP-BELT-ARMOUR-MM).
           DISPLAY '"BeltArmour":' '"' WS-3decimal '"'.
           COMPUTE WS-3decimal = FUNCTION NUMVAL(SHIP-DECK-ARMOUR-MM).
           DISPLAY '"DeckArmour":' '"' WS-3decimal '"'.
           COMPUTE WS-999 = FUNCTION NUMVAL(SHIP-MAIN-GUN-NR).
           DISPLAY '"MainGunNr":' '"' WS-999 '",'.
           COMPUTE WS-3decimal = FUNCTION NUMVAL(SHIP-MAIN-GUN-CALIBRE).
           DISPLAY '"MainGunCalibre":'
             '"' WS-3decimal '",'.
           COMPUTE WS-999 = FUNCTION NUMVAL(SHIP-SECONDARY-NR).
           DISPLAY '"SecondaryGunNr":' '"' WS-999 '",'.
           COMPUTE WS-3decimal = FUNCTION
              NUMVAL(SHIP-SECONDARY-CALIBRE).
           DISPLAY '"SecondaryGunCalibre":'
             '"' WS-3decimal '",'.
           DISPLAY '"FireControlComputer":'
             '"' SHIP-FIRE-CONTROL-CPU '",'.
           COMPUTE WS-999 = FUNCTION NUMVAL(SHIP-HEAVY-AA-NR).
           DISPLAY '"HeavyAAGunNr":' '"' WS-999 '",'.
           COMPUTE WS-999 = FUNCTION NUMVAL(SHIP-LIGHT-AA-NR).
           DISPLAY '"LightAAGunNr":' '"'  WS-999 '",'.
           DISPLAY '"AAControlComputer":'
             '"' SHIP-AA-CONTROL-CPU '",'.
           DISPLAY '"RadarModel":' '"' SHIP-RADAR '",'.
           DISPLAY '"SonarModel":' '"' SHIP-SONAR '",'.
           DISPLAY '"DepthCharges":' '"' SHIP-DEPTH-CHARGES '",'.
           DISPLAY '"Torpedoes":' '"' SHIP-DEPTH-CHARGES '",'.

      *    DISPLAY '"Torpedoes":' '"' SHIP-FIRST-AIRCRAFT-NR '"'.
      *    05 PIC 999.
      *    05 SHIP-FIRST-AIRCRAFT-MODEL PIC X(20).
      *    05 SHIP-SECOND-AIRCRAFT-NR PIC 999.
      *    05 SHIP-SECOND-AIRCRAFT-MODEL PIC X(20).
      *    05 SHIP-THIRD-AIRCRAFT-NR PIC 999.
      *    05 SHIP-THIRD-AIRCRAFT-MODEL PIC X(20).

           DISPLAY '}'

           GO TO READ-AND-PRINT-SHIPS.
