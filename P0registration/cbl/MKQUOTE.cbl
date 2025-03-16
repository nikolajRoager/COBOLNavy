      *This program surounds any string with quotes,
      * in the process removing leading and trailing spaces
      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.   MKQUOTE
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
       01  LK-LEN PIC 99.
       01  LK-INPUT PIC X(78).
       01  LK-OUTPUT PIC X(80).

       PROCEDURE DIVISION USING LK-INPUT , LK-LEN RETURNING LK-OUTPUT.
      *Move to local storage
           MOVE LK-INPUT(1:LK-LEN) TO A.
           INSPECT A
               TALLYING ASTART FOR LEADING SPACES.
           INSPECT FUNCTION REVERSE(A)
               TALLYING ALEN FOR LEADING SPACES.
      *First non-space character
           COMPUTE ASTART = 1 + ASTART.
      *Get actual length of non-space
           COMPUTE ALEN = 79 - ALEN - ASTART.

      *Make the string
           STRING  '"' A(ASTART:ALEN) '"'
              DELIMITED BY SIZE INTO B.

      *Move to return value
           MOVE B TO LK-OUTPUT.
