//LSSHVS  JOB 1,NOTIFY=&SYSUID
//***************************************************/
//STEP1 EXEC IGYWCL
//*SECOND compile task, compile the program to list ships
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(LSSHVS),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(LSSHVS),DISP=SHR
//*If that all worked, run the program
// IF RC = 0 THEN
//***************************************************/
//STEP2 EXEC PGM=LSSHVS
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//ALLSHPS   DD DSN=&SYSUID..USERDATA(ALLIED),DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
// ELSE
// ENDIF