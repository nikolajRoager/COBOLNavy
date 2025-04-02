//ADDSHJ JOB 1,NOTIFY=&SYSUID
//*compile the program to list ships
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(ADDSH),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(ADDSH),DISP=SHR
//LKED.SYSLIB  DD  DSNAME=CEE.SCEELKED,DISP=SHR
//             DD  DSNAME=CEE.SCEELKEX,DISP=SHR
//             DD  DSNAME=&SYSUID..LOAD,DISP=SHR
//LKED.SYSIN DD *
  INCLUDE SYSLIB(MKQUOTE)
//*If that all worked, run the program
// IF RC = 0 THEN
//***************************************************/
//RUN EXEC PGM=ADDSH
//*Link libraries
//STEPLIB DD DSN=&SYSUID..LOAD,DISP=SHR
//*User supplied data: allied ships
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
//* Something went wrong compiling
// ENDIF
