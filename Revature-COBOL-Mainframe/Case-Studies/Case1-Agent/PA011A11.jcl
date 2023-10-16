//* PROCEDURE FOR Q1 STEP005 - PREDELETION FOR PS2-PS7
//PA011A11 PROC
//PSTEP005 EXEC PGM=IEFBR14
//DD2      DD DSN=ARI011.KELVIN.AGTCMN.PS2,
//            DISP=(MOD,DELETE,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,BLKSIZE=800,RECFM=FB,DSORG=PS)
//DD3      DD DSN=ARI011.KELVIN.AGTCMN.ACTIVE.PS3,
//            DISP=(MOD,DELETE,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,BLKSIZE=800,RECFM=FB,DSORG=PS)
//DD4      DD DSN=ARI011.KELVIN.AGTCMN.INACTIVE.PS4,
//            DISP=(MOD,DELETE,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,BLKSIZE=800,RECFM=FB,DSORG=PS)
//DD5      DD DSN=ARI011.KELVIN.AGTCMN.PS5,
//            DISP=(MOD,DELETE,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,BLKSIZE=800,RECFM=FB,DSORG=PS)
//DD6      DD DSN=ARI011.KELVIN.AGTCMN.PS6,
//            DISP=(MOD,DELETE,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,BLKSIZE=800,RECFM=FB,DSORG=PS)
//DD7      DD DSN=ARI011.KELVIN.AGTCMN.PS7,
//            DISP=(MOD,DELETE,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,BLKSIZE=800,RECFM=FB,DSORG=PS)
//SYSPRINT DD SYSOUT=*
//* PROCEDURE FOR Q1 STEP010 - SORT ACCORDING TO AGENT CODE -> PS2
//PSTEP010  EXEC PGM=SORT
//SORTIN    DD DSN=ARI011.KELVIN.L1G.AGTCMN.PS1,
//             DISP=SHR
//SORTOUT   DD DSN=ARI011.KELVIN.AGTCMN.PS2,
//             DISP=(NEW,CATLG),
//             SPACE=(TRK,(1,1)),
//             DCB=*.PSTEP005.DD2
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSIN     DD DUMMY
//*SYSIN     DD DSN=ARI011.KELVIN.L0.AGENT.PDS(CA011A11),DISP=SHR
//* PROCEDURE FOR Q1 STEP020 - ELIMINATE DUPLICATE RECORDS FOR AGENTCODE
//PSTEP020  EXEC PGM=SORT
//SORTIN    DD DSN=ARI011.KELVIN.AGTCMN.PS2,
//             DISP=SHR
//SORTOUT   DD DSN=ARI011.KELVIN.AGTCMN.PS2,
//             DISP=SHR
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSIN     DD DSN=ARI011.KELVIN.L0.AGENT.PDS(CA021A11),DISP=SHR
//* PROCEDURE FOR Q1 STEP030 - SPLIT THE RECORDS BASED ON POLICY
//PSTEP030  EXEC PGM=SORT
//SORTIN    DD DSN=ARI011.KELVIN.AGTCMN.PS2,
//             DISP=SHR
//SORTOF01  DD DSN=ARI011.KELVIN.AGTCMN.ACTIVE.PS3,
//             DISP=(NEW,CATLG),
//             SPACE=(TRK,(1,1)),
//             DCB=*.PSTEP005.DD3
//SORTOF02  DD DSN=ARI011.KELVIN.AGTCMN.INACTIVE.PS4,
//             DISP=(NEW,CATLG),
//             SPACE=(TRK,(1,1)),
//             DCB=*.PSTEP005.DD4
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSIN     DD DSN=ARI011.KELVIN.L0.AGENT.PDS(CA031A11),DISP=SHR
//* ****  PUTTING IN SEQUENCE NUMBERS  *********************************
//PSTEP031  EXEC PGM=SORT
//SORTIN    DD DSN=ARI011.KELVIN.AGTCMN.ACTIVE.PS3,
//             DISP=SHR
//SORTOUT   DD DSN=ARI011.KELVIN.AGTCMN.ACTIVE.PS3,
//             DISP=SHR
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSIN     DD DSN=ARI011.KELVIN.L0.AGENT.PDS(CA041A11),DISP=SHR
//PSTEP032  EXEC PGM=SORT
//SORTIN    DD DSN=ARI011.KELVIN.AGTCMN.INACTIVE.PS4,
//             DISP=SHR
//SORTOUT   DD DSN=ARI011.KELVIN.AGTCMN.INACTIVE.PS4,
//             DISP=SHR
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSIN     DD DSN=ARI011.KELVIN.L0.AGENT.PDS(CA041A11),DISP=SHR
//* *******************************************************************
//* PROCEDURE FOR Q1 STEP040 - MERGE THE RECORDS BACK -> PS5
//PSTEP041  EXEC PGM=IEBGENER
//SYSUT1    DD DSN=ARI011.KELVIN.AGTCMN.ACTIVE.PS3,
//             DISP=SHR
//SYSUT2    DD DSN=ARI011.KELVIN.AGTCMN.PS5,
//             DISP=(NEW,CATLG),
//             SPACE=(TRK,(1,1)),
//             DCB=*.PSTEP005.DD5
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSIN     DD DUMMY
//PSTEP042  EXEC PGM=IEBGENER
//SYSUT1    DD DSN=ARI011.KELVIN.AGTCMN.INACTIVE.PS4,
//             DISP=SHR
//SYSUT2    DD DSN=ARI011.KELVIN.AGTCMN.PS5,
//             DISP=(MOD,CATLG),
//             SPACE=(TRK,(1,1)),
//             DCB=*.PSTEP005.DD5
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSIN     DD DUMMY
//*  SECOND STEP OF Q1 STEP040
//PSTEP043  EXEC PGM=SORT
//SORTIN    DD DSN=ARI011.KELVIN.AGTCMN.PS5,
//             DISP=SHR
//SORTOUT   DD DSN=ARI011.KELVIN.AGTCMN.PS5,
//             DISP=SHR
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSIN     DD DSN=ARI011.KELVIN.L0.AGENT.PDS(CA051A11),DISP=SHR
//* PROCEDURE FOR Q1 STEP050 - ADDING PREFIX BASED ON POLICY-TYPE
//PSTEP050  EXEC PGM=SORT
//SORTIN    DD DSN=ARI011.KELVIN.AGTCMN.PS2,
//             DISP=SHR
//SORTOUT   DD DSN=ARI011.KELVIN.AGTCMN.PS6,
//             DISP=(NEW,CATLG),
//             SPACE=(TRK,(1,1)),
//             DCB=*.PSTEP005.DD6
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSIN     DD DSN=ARI011.KELVIN.L0.AGENT.PDS(CA061A11),DISP=SHR
//* PROCEDURE FOR Q1 STEP060 - ADDING 099.01 TO POLICY COMMISSION
//PSTEP060  EXEC PGM=SORT
//SORTIN    DD DSN=ARI011.KELVIN.AGTCMN.PS2,
//             DISP=SHR
//SORTOUT   DD DSN=ARI011.KELVIN.AGTCMN.PS7,
//             DISP=(NEW,CATLG),
//             SPACE=(TRK,(1,1)),
//             DCB=*.PSTEP005.DD7
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSIN     DD DSN=ARI011.KELVIN.L0.AGENT.PDS(CA071A11),DISP=SHR
// PEND