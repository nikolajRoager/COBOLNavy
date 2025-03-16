       IDENTIFICATION DIVISION.
       FUNCTION-ID. TRMTRL.
       AUTHOR. AI GENERATED, supervised by Nikolaj R Christensen

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-INPUT            PIC X(4).
       01  LS-OUTPUT           PIC X(4).
       01  LS-LENGTH           PIC 9(1).
       01  LS-COUNTER          PIC 9(1).

       LINKAGE SECTION.
       01  LK-INPUT            PIC X(4).
       01  LK-OUTPUT           PIC X(4).

       PROCEDURE DIVISION USING LK-INPUT RETURNING LK-OUTPUT.
           *> Initialize local variables
           MOVE LK-INPUT TO LS-INPUT.
           MOVE 4 TO LS-LENGTH.
           MOVE 4 TO LS-COUNTER.

           *> Find last non-space character
           PERFORM VARYING LS-COUNTER FROM 4 BY -1 UNTIL LS-COUNTER = 1
               OR LS-INPUT(LS-COUNTER:1) NOT = ' '
           END-PERFORM.

           *> Extract the trimmed string
           MOVE SPACES TO LS-OUTPUT.
           MOVE LS-INPUT(1:LS-COUNTER) TO LS-OUTPUT.

           *> Return the trimmed string
           MOVE LS-OUTPUT TO LK-OUTPUT.
           GOBACK.
       END FUNCTION TRMTRL.