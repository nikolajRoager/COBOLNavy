//VSMCRJ JOB 1,NOTIFY=&SYSUID
//***************************************************/
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT   DD SYSOUT=*
//SYSIN      DD *
  DELETE Z67124.VSAM
  SET MAXCC=0
  DEFINE CLUSTER(     -
    NAME(Z67124.VSAM) -
    TRACKS(30)        -
    RECSZ(500 500)    -
    INDEXED           -
    KEYS(12 0)        -
    CISZ(2048))
