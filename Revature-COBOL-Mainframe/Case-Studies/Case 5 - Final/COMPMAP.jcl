//ARI011CM JOB OZA,OZA,MSGLEVEL=(1,1),
//            CLASS=A,MSGCLASS=A,NOTIFY=&SYSUID,REGION=0M
//**********************************************************************
//***     SAMPLE JCL TO COMPILE CICS MAP PROGRAM                     ***
//***     KEEP USERID IN PLACE OF OZAGS1                             ***
//***     DO NOT CHANGE LOADLIB=OZAADM.CICS.LOADLIB                  ***
//***     MAPSET & PROGRAM NAME SHOULD BE SAME                       ***
//**********************************************************************
//JOBPROC  JCLLIB ORDER=OZAGS1.USER.PROCLIB
//CICSMAP  EXEC PRCCCMAP,
//         OUTC=*,
//         COPYLIB=ARI011.KELVIN.TRNING.FINAL.COPYLIB,
//         LOADLIB=OZAADM.CICS.LOADLIB,
//         MAPSET=ARI011                *NAME OF MAPSET
//COPY.SYSUT1  DD  DSN=ARI011.KELVIN.TRNING.FINAL.PDS(MAP),
//             DISP=SHR
//