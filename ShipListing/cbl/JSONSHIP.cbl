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
       01 A PIC X(78) VALUE SPACES.
       01 B PIC X(80) VALUE SPACES.
       01 ASTART PIC 99.
       01 ALEN PIC 99.

       LINKAGE SECTION.
       01  LK-INPUT PIC X(80).

       PROCEDURE DIVISION USING LK-INPUT.
           DISPLAY 'CALLED'.
           GOBACK.