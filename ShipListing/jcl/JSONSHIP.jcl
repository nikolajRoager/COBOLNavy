//JSONSHP JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(JSONSHIP),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(JSONSHIP),DISP=SHR
//LKED.SYSLIB  DD  DSNAME=CEE.SCEELKED,DISP=SHR
//         DD  DSNAME=CEE.SCEELKEX,DISP=SHR
//         DD  DSNAME=&SYSUID..LOAD,DISP=SHR
//LKED.SYSIN DD *
  INCLUDE SYSLIB(MKQUOTE)
//*
