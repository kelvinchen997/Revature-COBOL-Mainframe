//ARI011CB JOB OZA,OZA,MSGLEVEL=(1,1),
//            CLASS=A,MSGCLASS=A,NOTIFY=&SYSUID,REGION=0M
//**********************************************************************
//*** KEEP USERID IN PLACE OF OZAGS1                                 ***
//*** SAMPLE JCL TO BIND COBOL + CICS + DB2 BATCH PROGRAM            ***
//*** DBRMLIB ===> DBRM LIBRARY                                      ***
//*** MEMBER ===> DBRM MEMBER NAME                                   ***
//*** PLAN   ===> PLAN NAME SAME AS MEMBER NAME                      ***
//*** NOTE: NEVER USE OZAXXXPL AS A PLAN NAME                        ***
//**********************************************************************
//JOBLIB   DD DISP=SHR,DSN=DSNA10.SDSNEXIT
//         DD DISP=SHR,DSN=DSNA10.SDSNLOAD
//         DD DISP=SHR,DSN=CEE.SCEERUN
//BINDPLAN EXEC PGM=IKJEFT01,DYNAMNBR=20
//DBRMLIB  DD DSN=ARI011.KELVIN.TRNING.FINAL.DBRM,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSTSIN DD *   PLAN,DBRM AND PROGRAM NAMES SHOULD BE SAME
 DSN SYSTEM(DBAG)
  BIND PLAN(ARI011P) MEMBER(ARI011P) ACT(REP) ISOLATION(CS) -
                      QUALIFIER(ARI011) OWNER(ARI011)
 END
//
//