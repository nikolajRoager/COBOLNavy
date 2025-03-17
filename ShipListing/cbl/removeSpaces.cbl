      *This program removes leading spaces of a string, and shifts it left 
      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    RMSPC
       AUTHOR.        Nikolaj R Christensen
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC X(80) VALUE "    Fiat Lux   ".
       01 B PIC X(80).
       01 A-START PIC 99.
       PROCEDURE DIVISION.
           INSPECT A
               TALLYING A-START FOR LEADING SPACES.
      *First non-space character
           COMPUTE A-START = 1+A-START.
           
           STRING A(A-START:) SPACES
              DELIMITED BY SIZE INTO B.

           DISPLAY B.
