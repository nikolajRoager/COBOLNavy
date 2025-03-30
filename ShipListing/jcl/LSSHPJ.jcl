//LSSHPJ JOB 1,NOTIFY=&SYSUID
//***************************************************/
//STEP1 EXEC IGYWCL
//*FIRST compile task, compile the library trim trailing (trmtrl)
//*The name has been shortened, to keep the code within 80 lines in main
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(MKQUOTE),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(MKQUOTE),DISP=SHR
//*If that worked, move on to compile the main program
// IF RC = 0 THEN
//STEP2 EXEC IGYWCL
//*SECOND compile task, compile the program to list ships
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(LSSHP),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(LSSHP),DISP=SHR
//LKED.SYSLIB  DD  DSNAME=CEE.SCEELKED,DISP=SHR
//         DD  DSNAME=CEE.SCEELKEX,DISP=SHR
//         DD  DSNAME=&SYSUID..LOAD,DISP=SHR
//LKED.SYSIN DD *
  INCLUDE SYSLIB(MKQUOTE)
//*If that all worked, run the program
// IF RC = 0 THEN
//***************************************************/
//RUN EXEC PGM=LSSHP
//*Link libraries
//STEPLIB DD DSN=&SYSUID..LOAD,DISP=SHR
//*User supplied data: allied ships
//ALLSHPS   DD DSN=&SYSUID..USERDATA(ALLIED),DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
//* Something went wrong in second compile
// ENDIF
// ELSE
//* Something went wrong in first compile
// ENDIF