//ADDSHJ  JOB 1,NOTIFY=&SYSUID
//***************************************************/
//STEP1 EXEC IGYWCL
//*SECOND compile task, compile the program to list ships
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(ADDSH),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(ADDSH),DISP=SHR
//*If that all worked, run the program
// IF RC = 0 THEN
//***************************************************/
//STEP2 EXEC PGM=ADDSH
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//ALLSHPS   DD DSN=&SYSUID..VSAM,DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//*Example data: HMS Rodney
//SYSIN     DD *
RN
BB
0029
HMS Rodney
Nelson Class
Repairing
North-East Atlantic
Home Fleet
2nd Battle Squadron
F H G Dalrymple-Hamilton
23.00
330.00
200.00
009
406.00
009
152.00
A.F.C.T. Mark I
006
008
H.A.C.S. Mark I
Type 79Y
None
000
002
001
Supermarine Walrus
000
None
000
None
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
// ELSE
// ENDIF
