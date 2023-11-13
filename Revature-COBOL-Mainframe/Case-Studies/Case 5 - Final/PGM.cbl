       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM.
      *   PROGRAM NAME: INSURANCE SYSTEM
      *   INPUT: INPUT FROM CICS TERMINAL
      *   OUTPUT: DB2 DATABASE, CICS TERMINAL,
      *   DESCRIPTION: MINICS AN INSURANCE SYSTEM WITH DB2, CICS, AND
      *       COBOL PROCESSING.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 VARS.
          05 CICS-VARS.
             10 CICS-SYS-VARS.
                15 WS-CLEAR      PIC X(75) VALUE ' '.
                15 WS-MSG        PIC X(75) VALUE ' '.
                15 WS-TRASH      PIC X(75) VALUE ' '.
             10 CICS-RESP-CODES.
                15 WS-RESP       PIC S9(8) USAGE COMP.
                15 WS-RESP-P     PIC X(10).
                15 WS-RESP2      PIC S9(8) USAGE COMP.
                15 WS-RESP2-P    PIC X(10).
             10 CICS-MAPS.
                15 CICS-LOGIN-MAP.
                   20 WS-LID-I      PIC X(10).
                   20 WS-LTYPE-I    PIC X(1).
                   20 WS-LPWD-I     PIC X(08).
                15 CICS-REGIST-MAP.
                   20 WS-RID-I      PIC X(10).
                   20 WS-RNAME-I    PIC X(20).
                   20 WS-RPWD-I     PIC X(08).
                   20 WS-RTYPE-I    PIC X(1).
                15 CICS-AGTM-MAP.
                   20 WS-AMOP-I     PIC X(1).
                15 CICS-AGT1-MAP.
                   20 WS-A1DATE-I   PIC X(10).
                15 CICS-AGT3-MAP.
                   20 WS-A3YON-I    PIC X(01).             
                   20 WS-A3PWD-I    PIC X(08).
                   20 WS-A3CID-I    PIC X(10).
                   20 WS-A3NAME-I   PIC X(20).
                   20 WS-A3PNUM-I   PIC X(12).
                   20 WS-A3DOB-I    PIC X(10).
                   20 WS-A3SSN-I    PIC X(11).
                   20 WS-A3PTYPE-I  PIC X(03).
                   20 WS-A3TEN-I    PIC X(02).
                   20 WS-A3SUM-I    PIC 9(07).
                15 CICS-POLM-MAP.
                   20 WS-PMOP-I     PIC X(1).
          05 DB2-SYS-VARS.
             10 WS-ERR-MSG.
                15 WS-ERR-LENGTH    PIC S9(04) COMP VALUE 800.
                15 WS-ERR-TEXT      PIC X(80) OCCURS 10 TIMES.
             10 WS-ERR-LRECL        PIC S9(09) COMP VALUE 80.
             10 WS-SQL-CODE-P       PIC X(10).
         05 COBOL-VARS.
            10 COBOL-PROCESS-VARS.
               15 PROCESS-VARS.                   
                  20 DATE-TIME.
                     25 WS-ABS-TIME     PIC 9(15).
                     25 WS-CURRENT-DATE PIC X(10).
                     25 WS-CUR-YR-2-DY  PIC X(10).
                     25 WS-DAY-X29      PIC 9(02).
                     25 WS-MONTH-X29    PIC 9(02).
                  20 WS-PWD-COUNT       PIC 9(1).
                  20 WS-USER-EXIST      PIC 9(1).
                  20 WS-PWD-CHECK       PIC 9(1).
                  20 CURRENT-USER.
                     25 WS-CUR-ID       PIC X(10).
                     25 WS-CUR-NAME     PIC X(20).
                     25 WS-CUR-PASSWORD PIC X(08).
                     25 WS-CUR-TYPE     PIC X(01).
                  20 GENERATION-VARS.
                     25 WS-GEND-PID     PIC X(10).
                     25 WS-GEND-CID     PIC X(10).
                     25 WS-GEND-AID     PIC X(10).
                  20 BR3-1-VARS.
                     25 WS-BR1-PREM-AMNT   PIC 9(07).
                  20 BR3-2-KSDS-LAYOUT.
                     25 KSDS-POL-ID        PIC X(10).
                     25 F                  PIC X(01) VALUE ' '.
                     25 KSDS-CUST-ID       PIC X(10).
                     25 F                  PIC X(01) VALUE ' '.
                     25 KSDS-AGENT-ID      PIC X(10).
                     25 F                  PIC X(01) VALUE ' '.
                     25 KSDS-NAME          PIC X(20).
                     25 F                  PIC X(01) VALUE ' '.
                     25 KSDS-PHONE         PIC X(12).
                     25 F                  PIC X(01) VALUE ' '.
                     25 KSDS-DOB           PIC X(10).
                     25 F                  PIC X(01) VALUE ' '.
                     25 KSDS-SSN           PIC X(11).
                     25 F                  PIC X(01) VALUE ' '.
                     25 KSDS-POL-TYPE      PIC X(03).
                     25 F                  PIC X(01) VALUE ' '.
                     25 KSDS-TENURE-YEARS  PIC X(02).
                     25 F                  PIC X(01) VALUE ' '.
                     25 KSDS-SUM-ASSURED   PIC X(07).
                     25 F                  PIC X(16) VALUE ' '.
                  20 BR3-3-CALCULATIONS.                      
                     25 WS-SUM-ASSURED-V   PIC 9(07)V.
                     25 WS-PREMIUM-AMNT-V  PIC 9(07)V.
                     25 WS-TENURE-YEARS    PIC 9(02).
                     25 WS-TENURE-YEARS-S  PIC X(02).
                     25 WS-END-YEARS       PIC 9(04).
                     25 WS-END-YEARS-S     PIC X(04).
                     25 WS-END-MONTHS      PIC 9(02).
                     25 WS-END-MONTHS-S    PIC X(02).
                  20 WS-PREMIUM-PAY-DATE PIC X(10).
                  20 WS-MATURITY-DATE    PIC X(10).
                  20 POL2-VARS.
                     25 WS-POL2-PREM-AMNT   PIC 9(07).
                     25 WS-POL2-SUM-ASSURED PIC 9(07).
      * SQL/DB2 MEMEBERS
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           EXEC SQL
               INCLUDE TBACCNTS
           END-EXEC.                                          
           EXEC SQL
               INCLUDE TBPOLDTL
           END-EXEC.
           EXEC SQL
               INCLUDE TBHOLDTL
           END-EXEC.
      * DECLARE CURSORS
           EXEC SQL
               DECLARE CURSOR_AGT1 CURSOR FOR
                 SELECT
                    A.CUST_ID
                   ,A.PREM_PAY_DATE
                   ,A.PREMIUM_AMNT
                   ,B.PHONE
                 FROM
                   TB_POLICY_DETAILS A
                   JOIN
                   TB_POLICY_HOLDER_DETAILS B
                   ON A.POL_ID = B.POL_ID
                 WHERE           
                   A.PREM_PAY_DATE < :HV-POL-PAID-DATE
                   AND
                   A.AGENT_ID = :HV-POL-AGENT-ID
           END-EXEC.
           EXEC SQL
               DECLARE CURSOR_AGT2 CURSOR FOR
                 SELECT B.*
                 FROM
                   TB_POLICY_DETAILS A
                   JOIN
                   TB_POLICY_HOLDER_DETAILS B
                   ON A.POL_ID = B.POL_ID
                 WHERE A.PREM_PAY_DATE < :HV-POL-PAID-DATE
           END-EXEC.
           EXEC SQL
               DECLARE CURSOR_POL1 CURSOR FOR
                 SELECT POL_ID, PREM_PAY_DATE, PREMIUM_AMNT
                 FROM TB_POLICY_DETAILS
                 WHERE CUST_ID = :HV-POL-CUST-ID
           END-EXEC.
           EXEC SQL
               DECLARE CURSOR_POL2 CURSOR FOR
                 SELECT
                    A.*
                   ,B.NAME
                   ,B.PHONE
                   ,B.DOB
                   ,B.TENURE_YEARS
                   ,B.SUM_ASSURED
                 FROM
                   TB_POLICY_DETAILS A
                   JOIN
                   TB_POLICY_HOLDER_DETAILS B
                   ON A.POL_ID = B.POL_ID
                 WHERE A.CUST_ID = :HV-POL-CUST-ID
           END-EXEC.
      * CICS MEMBERS
           COPY ARI011.
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM 2000-START-CICS
      * 3000 RESERVED
           PERFORM 4000-END-CICS

           PERFORM 9000-TERM.
       0000-MAIN-END. EXIT.

       1000-INIT.
      * MAP INIT
           MOVE LOW-VALUES TO LOGINO, LOGINO.
           MOVE LOW-VALUES TO REGISTI, REGISTO.
           MOVE LOW-VALUES TO AGTMI, AGTMO.
           MOVE LOW-VALUES TO AGT1I, AGT1O.
           MOVE LOW-VALUES TO AGT2I, AGT2O.
           MOVE LOW-VALUES TO AGT3I, AGT3O.
           MOVE LOW-VALUES TO AGT4I, AGT4O.
           MOVE LOW-VALUES TO POLMI, POLMO.
           MOVE LOW-VALUES TO POL1I, POL1O.
           MOVE LOW-VALUES TO POL2I, POL2O.
      * SQL HOST VARIBLES
           MOVE LOW-VALUES TO DCLTB-POLICY-DETAILS.
           MOVE LOW-VALUES TO DCLTB-ACCOUNTS.
           MOVE LOW-VALUES TO DCLTB-POLICY-HOLDER-DETAILS.
      * PROCESS VARIABLES
           MOVE LOW-VALUES TO CICS-MAPS.
           MOVE LOW-VALUES TO PROCESS-VARS.
           MOVE 3 TO  WS-PWD-COUNT.
       1000-INIT-END. EXIT.

       2000-START-CICS.
           PERFORM 2100-LOGIN.
           PERFORM 2200-SPLIT-BRANCH.

       2000-START-CICS-END. EXIT.

       2100-LOGIN.
      * GET DATE
           EXEC CICS ASKTIME ABSTIME(WS-ABS-TIME) END-EXEC.
           EXEC CICS FORMATTIME
               ABSTIME(WS-ABS-TIME)
               DATESEP('-')
               DDMMYYYY(WS-CURRENT-DATE)
               YYYYMMDD(WS-CUR-YR-2-DY)
           END-EXEC.
           MOVE WS-CURRENT-DATE TO LDATEO
      * GET LOGIN INFORMATION
           EXEC CICS SEND MAP('LOGIN') MAPSET('ARI011') END-EXEC.
           PERFORM RECEIVE-LOGIN-MAP.
           PERFORM 2110-CHECK-USER.
           PERFORM 2120-PASSWORD-CHECK.
       2100-LOGIN-END. EXIT.

       2110-CHECK-USER.
           MOVE WS-LID-I TO HV-ACC-ID
           EXEC SQL
             SELECT ID, NAME, PASSWORD, TYPE INTO
                :HV-ACC-ID
               ,:HV-ACC-NAME
               ,:HV-ACC-PASSWORD
               ,:HV-ACC-TYPE
             FROM TB_ACCOUNTS WHERE ID =:HV-ACC-ID
           END-EXEC.
           MOVE  HV-ACC-ID       TO WS-CUR-ID
           MOVE  HV-ACC-NAME     TO WS-CUR-NAME
           MOVE  HV-ACC-PASSWORD TO WS-CUR-PASSWORD
           MOVE  HV-ACC-TYPE     TO WS-CUR-TYPE
           IF (SQLCODE = 0)
               CONTINUE
           ELSE
               PERFORM 2111-CREATE-NEW-USER
           END-IF.
       2110-CHECK-USER-END. EXIT.

       2111-CREATE-NEW-USER.
           PERFORM SEND-REGIST-MAP
           PERFORM RECEIVE-REGIST-MAP
           MOVE WS-RID-I      TO HV-ACC-ID
           MOVE WS-RNAME-I    TO HV-ACC-NAME-TEXT
             MOVE LENGTH OF WS-RNAME-I TO HV-ACC-NAME-LEN
           MOVE WS-RPWD-I     TO HV-ACC-PASSWORD
           MOVE WS-RTYPE-I    TO HV-ACC-TYPE
           EXEC SQL
                INSERT INTO TB_ACCOUNTS VALUES (
                 :HV-ACC-ID
                ,:HV-ACC-NAME
                ,:HV-ACC-PASSWORD
                ,:HV-ACC-TYPE
                )
            END-EXEC.
            PERFORM H003-SQL-HANDLER
            MOVE 'CREATED NEW USER. EXITING.....' TO RMSGO
            PERFORM SEND-REGIST-MAP
            PERFORM 9000-TERM.
       2111-CREATE-NEW-USER-END. EXIT.

       2120-PASSWORD-CHECK.
           IF (WS-PWD-CHECK = 1)
               CONTINUE
           ELSE
               PERFORM RECEIVE-LOGIN-MAP
               IF (WS-LPWD-I NOT EQUAL WS-CUR-PASSWORD)
                   COMPUTE WS-PWD-COUNT = WS-PWD-COUNT - 1
               ELSE MOVE 1 TO WS-PWD-CHECK END-IF
               IF (WS-PWD-COUNT = 0)
                   STRING 'YOU HAVE ENTERED    '    DELIMITED BY SIZE
                          'TOO MANY PASSWORDS. '    DELIMITED BY SIZE
                          'EXITING PROGRAM...  '    DELIMITED BY SIZE
                          INTO LMSGO
                   END-STRING
                   EXEC CICS SEND MAP('LOGIN') MAPSET('ARI011') END-EXEC
                   PERFORM 9000-TERM
               ELSE
                   STRING 'INCORRECT PASSWORD. '    DELIMITED BY SIZE
                          'YOU HAVE '               DELIMITED BY SIZE
                          WS-PWD-COUNT              DELIMITED BY SIZE
                          ' ATTEMPTS LEFT.'         DELIMITED BY SIZE
                          INTO LMSGO
                   END-STRING
                   EXEC CICS SEND MAP('LOGIN') MAPSET('ARI011') END-EXEC
                  GO TO 2120-PASSWORD-CHECK
               END-IF
           END-IF.
       2120-PASSWORD-CHECK-END. EXIT.

       2200-SPLIT-BRANCH.
           EVALUATE TRUE
           WHEN (WS-CUR-TYPE = 'A')
               PERFORM 3100-PROCCESS-AGENT
           WHEN (WS-CUR-TY E = 'P')
               PERFORM 3200-PROCCESS-HOLDER
           END-EVALUATE.
       2200-SPLIT-BRANCH-END. EXIT.

       3100-PROCCESS-AGENT.
           PERFORM SEND-AGTM-MAP.
           PERFORM RECEIVE-AGTM-MAP.
           EVALUATE TRUE
           WHEN(WS-AMOP-I = '1')
               PERFORM 3110-A-OPTION1
           WHEN(WS-AMOP-I = '2')
               PERFORM 3120-A-OPTION2
           WHEN(WS-AMOP-I = '3')
               PERFORM 3130-A-OPTION3
           WHEN OTHER
               MOVE LOW-VALUES TO WS-MSG
               MOVE 'PROCESS-AGENT, BAD' TO WS-MSG
               EXEC CICS SEND FROM(WS-MSG) ERASE END-EXEC
               PERFORM 9000-TERM
           END-EVALUATE.
       3100-PROCCESS-AGENT-END. EXIT.

       3110-A-OPTION1.
           PERFORM SEND-AGT1-MAP.
           PERFORM RECEIVE-AGT1-MAP.
           MOVE WS-A1DATE-I TO HV-POL-PAID-DATE
           MOVE WS-CUR-ID   TO HV-POL-AGENT-ID
           EXEC SQL OPEN CURSOR_AGT1 END-EXEC.
           PERFORM UNTIL SQLCODE = 100
               EXEC SQL
                   FETCH CURSOR_AGT1 INTO
                        :HV-POL-CUST-ID
                       ,:HV-POL-PREM-PAY-DATE
                       ,:HV-POL-PREMIUM-AMNT
                       ,:HV-HOL-PHONE
               END-EXEC
               MOVE HV-POL-CUST-ID       TO A1CIDO
               MOVE HV-POL-PREM-PAY-DATE TO A1PDATEO
               MOVE HV-POL-PREMIUM-AMNT  TO WS-BR1-PREM-AMNT
                 MOVE WS-BR1-PREM-AMNT TO A1AMNTO
               MOVE HV-HOL-PHONE         TO A1PNUMO
               EXEC CICS SEND MAP('AGT1') MAPSET('ARI011') END-EXEC
               EXEC CICS DELAY INTERVAL(000003) END-EXEC
           END-PERFORM.
           EXEC SQL CLOSE CURSOR_AGT1 END-EXEC.
       3110-A-OPTION1-END. EXIT.

       3120-A-OPTION2.
           MOVE WS-CUR-YR-2-DY TO HV-POL-PAID-DATE
           EXEC SQL OPEN CURSOR_AGT2 END-EXEC.
           PERFORM H001-CLEAR-SCREEN
           PERFORM UNTIL SQLCODE = 100
               EXEC SQL
                   FETCH CURSOR_AGT2 INTO
                        :DCLTB-POLICY-HOLDER-DETAILS
               END-EXEC
               IF SQLCODE = 100
                 CONTINUE
               ELSE
                 MOVE HV-HOL-POL-ID       TO A2PIDO  , KSDS-POL-ID
                 MOVE HV-HOL-CUST-ID      TO A2CIDO  , KSDS-CUST-ID
                 MOVE HV-HOL-AGENT-ID     TO A2AIDO  , KSDS-AGENT-ID
                 MOVE HV-HOL-NAME-TEXT    TO A2NAMEO , KSDS-NAME
                 MOVE HV-HOL-PHONE        TO A2PNUMO , KSDS-PHONE
                 MOVE HV-HOL-DOB          TO A2DOBO  , KSDS-DOB
                 MOVE HV-HOL-SSN          TO A2SSNO  , KSDS-SSN
                 MOVE HV-HOL-POL-TYPE     TO A2PTYPEO, KSDS-POL-TYPE
                 MOVE HV-HOL-TENURE-YEARS TO A2TENO  , KSDS-TENURE-YEARS
                 MOVE HV-HOL-SUM-ASSURED  TO A2SUMO  , KSDS-SUM-ASSURED
      * WRITE TO KSDS
                 EXEC CICS WRITE
                     FILE('ARI011F')
                     FROM(BR3-2-KSDS-LAYOUT)
                     RIDFLD(KSDS-POL-ID)
                     RESP(WS-RESP) RESP2(WS-RESP2)
                 END-EXEC
                 PERFORM H002-RESP-HANDLER
                 STRING 'POLICY ID: '        DELIMITED BY SIZE
                        HV-HOL-POL-ID        DELIMITED BY SIZE
                        INTO A2MSG1O
                 END-STRING
                 EXEC CICS SEND MAP('AGT2') MAPSET('ARI011') END-EXEC
                 EXEC CICS DELAY INTERVAL(000002) END-EXEC
               END-IF
           END-PERFORM.
           EXEC SQL CLOSE CURSOR_AGT1 END-EXEC.
       3120-A-OPTION2-END. EXIT.

       3130-A-OPTION3.
           PERFORM SEND-AGT3-MAP.
           PERFORM RECEIVE-AGT3-MAP.
      * CREATE NEW USER
           IF(WS-A3YON-I = 'Y') PERFORM 3131-CREATE-NEW-USER END-IF.
      * INSERT INTO TBPOLDTL, AND TBHOLDTL
           STRING WS-A3NAME-I(1:2)        DELIMITED BY SIZE
                  WS-A3PTYPE-I(1:2)       DELIMITED BY SIZE
                  WS-A3SSN-I(5:2)         DELIMITED BY SIZE
                  WS-A3SSN-I(8:4)         DELIMITED BY SIZE
                  INTO WS-GEND-PID
           END-STRING.
           PERFORM 3133-CREATE-NEW-HOLDTL.
           PERFORM 3132-CREATE-NEW-POLDTL.
       3130-A-OPTION3-END. EXIT.

       3131-CREATE-NEW-USER.
           STRING WS-A3DOB-I(1:4)        DELIMITED BY SIZE
                  WS-A3DOB-I(6:2)        DELIMITED BY SIZE
                  'REVC'                 DELIMITED BY SIZE
                  INTO WS-GEND-CID
           END-STRING.
           MOVE WS-GEND-CID   TO HV-ACC-ID
           MOVE WS-A3NAME-I   TO HV-ACC-NAME-TEXT
             MOVE LENGTH OF WS-A3NAME-I TO HV-ACC-NAME-LEN
           MOVE WS-A3PWD-I    TO HV-ACC-PASSWORD
           MOVE 'P'           TO HV-ACC-TYPE
           EXEC SQL
               INSERT INTO TB_ACCOUNTS VALUES (
                :HV-ACC-ID
               ,:HV-ACC-NAME
               ,:HV-ACC-PASSWORD
               ,:HV-ACC-TYPE
               )
           END-EXEC.
      * CHANGING WS-A3CID-I TO NEWLY CREATED CID
           MOVE WS-GEND-CID TO WS-A3CID-I.
       3131-CREATE-NEW-USER-END. EXIT.

       3133-CREATE-NEW-HOLDTL.
           MOVE WS-GEND-PID      TO HV-HOL-POL-ID
           MOVE WS-A3CID-I       TO HV-HOL-CUST-ID
           MOVE WS-CUR-ID        TO HV-HOL-AGENT-ID
           MOVE WS-A3NAME-I      TO HV-HOL-NAME-TEXT
             MOVE LENGTH OF WS-A3NAME-I TO HV-HOL-NAME-LEN
           MOVE WS-A3PNUM-I      TO HV-HOL-PHONE
           MOVE WS-A3DOB-I       TO HV-HOL-DOB
           MOVE WS-A3SSN-I       TO HV-HOL-SSN
           MOVE WS-A3PTYPE-I     TO HV-HOL-POL-TYPE
           MOVE WS-A3TEN-I       TO HV-HOL-TENURE-YEARS
           MOVE WS-A3SUM-I       TO WS-SUM-ASSURED-V
             MOVE WS-SUM-ASSURED-V TO HV-HOL-SUM-ASSURED
           EXEC SQL
               INSERT INTO TB_POLICY_HOLDER_DETAILS VALUES (
                :HV-HOL-POL-ID
               ,:HV-HOL-CUST-ID
               ,:HV-HOL-AGENT-ID
               ,:HV-HOL-NAME
               ,:HV-HOL-PHONE
               ,:HV-HOL-DOB
               ,:HV-HOL-SSN
               ,:HV-HOL-POL-TYPE      
               ,:HV-HOL-TENURE-YEARS
               ,:HV-HOL-SUM-ASSURED
               )
           END-EXEC.
       3133-CREATE-NEW-HOLDTL-END. EXIT.

       3132-CREATE-NEW-POLDTL.
      * CALCULATE PREMIUM AMOUNT
           MOVE WS-A3SUM-I TO WS-SUM-ASSURED-V
           MOVE WS-A3TEN-I TO WS-TENURE-YEARS
           COMPUTE WS-PREMIUM-AMNT-V =
               WS-SUM-ASSURED-V / ( WS-TENURE-YEARS * 12 ).
      * CALCULATE THE PREMIUM PAY DATE
           MOVE WS-CUR-YR-2-DY(9:2) TO WS-DAY-X29
           MOVE WS-CUR-YR-2-DY(6:2) TO WS-MONTH-X29
           IF(WS-DAY-X29 < 5) THEN
               MOVE WS-CUR-YR-2-DY(1:4) TO WS-END-YEARS-S
               MOVE WS-CUR-YR-2-DY(6:2) TO WS-END-MONTHS-S
           ELSE
               IF(WS-MONTH-X29  = 12) THEN
                   MOVE WS-CUR-YR-2-DY(1:4) TO WS-END-YEARS
                   ADD 1 TO WS-END-YEARS
                   MOVE WS-END-YEARS TO WS-END-YEARS-S
                   MOVE '01' TO WS-END-MONTHS-S
               ELSE
                   MOVE WS-CUR-YR-2-DY(1:4) TO WS-END-YEARS-S
                   MOVE WS-CUR-YR-2-DY(6:2) TO WS-END-MONTHS
                   ADD 1 TO WS-END-MONTHS
                   MOVE WS-END-MONTHS TO WS-END-MONTHS-S
               END-IF
           END-IF
           STRING WS-END-YEARS-S  DELIMITED BY SIZE
                  '-'             DELIMITED BY SIZE
                  WS-END-MONTHS-S DELIMITED BY SIZE
                  '-'             DELIMITED BY SIZE
                  '05'            DELIMITED BY SIZE
                  INTO WS-PREMIUM-PAY-DATE
           END-STRING.
      * MATURITY DATE
           MOVE WS-CUR-YR-2-DY(1:4) TO WS-END-YEARS
           MOVE WS-A3TEN-I TO WS-TENURE-YEARS
           ADD WS-TENURE-YEARS TO WS-END-YEARS
           MOVE WS-END-YEARS TO WS-END-YEARS-S
           STRING WS-END-YEARS-S           DELIMITED BY SIZE
                  WS-PREMIUM-PAY-DATE(5:6) DELIMITED BY SIZE
                  INTO WS-MATURITY-DATE
           END-STRING.
      * INSERT INTO TBPOLDTL
           MOVE WS-GEND-PID         TO HV-POL-POL-ID
           MOVE WS-A3CID-I          TO HV-POL-CUST-ID
           MOVE WS-CUR-ID           TO HV-POL-AGENT-ID
           MOVE WS-A3PTYPE-I        TO HV-POL-POL-TYPE
           MOVE WS-CUR-YR-2-DY      TO HV-POL-PAID-DATE
           MOVE WS-PREMIUM-AMNT-V   TO HV-POL-PREMIUM-AMNT
           MOVE WS-PREMIUM-PAY-DATE TO HV-POL-PREM-PAY-DATE
           MOVE WS-PREMIUM-PAY-DATE TO HV-POL-POL-STRT-DATE
           MOVE WS-MATURITY-DATE    TO HV-POL-MATURITY-DATE
           MOVE WS-A3SSN-I          TO HV-POL-SSN
           EXEC SQL
               INSERT INTO TB_POLICY_DETAILS VALUES (
                :HV-POL-POL-ID
               ,:HV-POL-CUST-ID
               ,:HV-POL-AGENT-ID
               ,:HV-POL-POL-TYPE
               ,:HV-POL-PAID-DATE
               ,:HV-POL-PREMIUM-AMNT
               ,:HV-POL-PREM-PAY-DATE
               ,:HV-POL-POL-STRT-DATE
               ,:HV-POL-MATURITY-DATE
               ,:HV-POL-SSN
               )
           END-EXEC.
       3132-CREATE-NEW-POLDTL-NED. EXIT.

       3200-PROCCESS-HOLDER.
           PERFORM SEND-POLM-MAP.
           PERFORM RECEIVE-POLM-MAP.
           EVALUATE TRUE
           WHEN(WS-PMOP-I = '1')
               PERFORM 3210-P-OPTION1
           WHEN(WS-PMOP-I = '2')
               PERFORM 3220-P-OPTION2
           WHEN OTHER
               MOVE LOW-VALUES TO WS-MSG
               MOVE 'PROCESS-HOLDER, BAD' TO WS-MSG
               EXEC CICS SEND FROM(WS-MSG) ERASE END-EXEC
               PERFORM 9000-TERM
           END-EVALUATE.
       3200-PROCCESS-HOLDER-END. EXIT.

       3210-P-OPTION1.
           PERFORM SEND-POL1-MAP
      * UPDATE THE TBPOLDTL WITH CURRENT DATE AND NEXT PAY_DATE
      * CREATE NEXT PREMIMUN_PAY_DATE
           IF (WS-CUR-YR-2-DY(6:2) = '12') THEN
               MOVE WS-CUR-YR-2-DY(1:4) TO WS-END-YEARS
               ADD 1 TO WS-END-YEARS
               MOVE WS-END-YEARS TO WS-END-YEARS-S
               STRING WS-END-YEARS-S DELIMITED BY SIZE
                      '-'            DELIMITED BY SIZE
                      '01'           DELIMITED BY SIZE
                      '-'            DELIMITED BY SIZE
                      '05'           DELIMITED BY SIZE
                      INTO WS-PREMIUM-PAY-DATE
               END-STRING
           ELSE
               MOVE WS-CUR-YR-2-DY(6:2) TO WS-END-MONTHS
               ADD 1 TO WS-END-MONTHS
               MOVE WS-END-MONTHS TO WS-END-MONTHS-S
               STRING WS-CUR-YR-2-DY(1:4) DELIMITED BY SIZE
      * UN* TO INSERT DEFAULT RECS     STRING '2000' DELIMITED BY SIZE
                      '-'                 DELIMITED BY SIZE
                      WS-END-MONTHS-S     DELIMITED BY SIZE
                      '-'                 DELIMITED BY SIZE
                      '05'                DELIMITED BY SIZE
                      INTO WS-PREMIUM-PAY-DATE
               END-STRING
           END-IF.
           MOVE WS-CUR-YR-2-DY      TO HV-POL-PAID-DATE
           MOVE WS-PREMIUM-PAY-DATE TO HV-POL-PREM-PAY-DATE
           MOVE WS-CUR-ID           TO HV-POL-CUST-ID
           EXEC SQL
             UPDATE TB_POLICY_DETAILS
             SET
                PAID_DATE     = :HV-POL-PAID-DATE
               ,PREM_PAY_DATE = :HV-POL-PREM-PAY-DATE
             WHERE CUST_ID = :HV-POL-CUST-ID
           END-EXEC.
           PERFORM H003-SQL-HANDLER
      * GRAB RECORDS AND PRINT THEM TO SCREEN
           MOVE WS-CUR-ID   TO HV-POL-CUST-ID
           EXEC SQL OPEN CURSOR_POL1 END-EXEC.
           PERFORM UNTIL SQLCODE = 100
               EXEC SQL
                   FETCH CURSOR_POL1 INTO
                        :HV-POL-POL-ID
                       ,:HV-POL-PREM-PAY-DATE
                       ,:HV-POL-PREMIUM-AMNT
               END-EXEC
               MOVE HV-POL-POL-ID        TO P1PIDO
               MOVE HV-POL-PREM-PAY-DATE TO P1PDATEO
               MOVE HV-POL-PREMIUM-AMNT  TO WS-BR1-PREM-AMNT
                 MOVE WS-BR1-PREM-AMNT TO P1AMNTO
               EXEC CICS SEND MAP('POL1') MAPSET('ARI011') END-EXEC
               EXEC CICS DELAY INTERVAL(000003) END-EXEC
           END-PERFORM.
           EXEC SQL CLOSE CURSOR_POL1 END-EXEC.
       3210-P-OPTION1-END. EXIT.

       3220-P-OPTION2.
           PERFORM SEND-POL2-MAP
      * PULL FROM TBPOLDTL JOIN TBHOLDTL
           MOVE WS-CUR-ID TO HV-POL-CUST-ID
           EXEC SQL OPEN CURSOR_POL2 END-EXEC.
           PERFORM UNTIL SQLCODE = 100
               EXEC SQL
                   FETCH CURSOR_POL2 INTO
                        :DCLTB-POLICY-DETAILS
                       ,:HV-HOL-NAME
                       ,:HV-HOL-PHONE
                       ,:HV-HOL-DOB
                       ,:HV-HOL-TENURE-YEARS
                       ,:HV-HOL-SUM-ASSURED
               END-EXEC
               MOVE HV-POL-POL-ID            TO P2PIDO
               MOVE HV-POL-CUST-ID           TO P2CIDO
               MOVE HV-POL-AGENT-ID          TO P2AIDO
               MOVE HV-POL-POL-TYPE          TO P2PTYPEO
               MOVE HV-POL-PAID-DATE         TO P2DATEO
               MOVE HV-POL-PREMIUM-AMNT TO WS-POL2-PREM-AMNT
                 MOVE WS-POL2-PREM-AMNT      TO P2AMNTO
               MOVE HV-POL-PREM-PAY-DATE     TO P2PDATEO
               MOVE HV-POL-POL-STRT-DATE     TO P2SDATEO
               MOVE HV-POL-MATURITY-DATE     TO P2MDATEO
               MOVE HV-POL-SSN               TO P2SSNO
               MOVE HV-HOL-NAME-TEXT         TO P2NAMEO
               MOVE HV-HOL-PHONE             TO P2PNUMO
               MOVE HV-HOL-DOB               TO P2DOBO
               MOVE HV-HOL-TENURE-YEARS      TO P2TENO
               MOVE HV-HOL-SUM-ASSURED TO WS-POL2-SUM-ASSURED
                 MOVE WS-POL2-SUM-ASSURED    TO P2SUMO                       
               EXEC CICS SEND MAP('POL2') MAPSET('ARI011') END-EXEC
               EXEC CICS DELAY INTERVAL(000005) END-EXEC
           END-PERFORM.
           EXEC SQL CLOSE CURSOR_POL2 END-EXEC.
       3220-P-OPTION2-END. EXIT.

       4000-END-CICS.
           PERFORM H001-CLEAR-SCREEN
           EXEC CICS SEND MAP('AGT4') MAPSET('ARI011') END-EXEC.
       4000-END-CICS-END. EXIT.

       9000-TERM.
           EXEC CICS RETURN END-EXEC.
           STOP RUN.
       9000-TERM-END. EXIT.
      ******************************************************************
      * HELPER FUNCTIONS                                               *
      ******************************************************************
       H001-CLEAR-SCREEN.
           EXEC CICS SEND
               FROM(WS-CLEAR)
               LENGTH(LENGTH OF WS-CLEAR)
               ERASE
           END-EXEC.
       H001-CLEAR-SCREEN-END. EXIT.

       H002-RESP-HANDLER.
           IF WS-RESP = DFHRESP(NORMAL)
               CONTINUE
           ELSE
               MOVE WS-RESP TO WS-RESP-P
               MOVE WS-RESP2 TO WS-RESP2-P
               MOVE SPACES TO WS-MSG
               STRING 'RESP: '   DELIMITED BY SIZE
                      WS-RESP-P  DELIMITED BY SIZE
                      ', RESP2: '   DELIMITED BY SIZE
                      WS-RESP2-P  DELIMITED BY SIZE
                      INTO WS-MSG
               END-STRING
               EXEC CICS SEND
                   FROM(WS-MSG)
                   ERASE
               END-EXEC
               EXEC CICS RECEIVE
                   INTO(WS-TRASH)
               END-EXEC
               PERFORM 9000-TERM
           END-IF.
       H002-RESP-HANDLER-END. EXIT.

       H003-SQL-HANDLER.
           IF SQLCODE = 0
               CONTINUE
           ELSE
               MOVE LOW-VALUES TO WS-ERR-MSG
               CALL 'DSNTIAR' USING SQLCA WS-ERR-MSG WS-ERR-LRECL
               EXEC CICS SEND
                   FROM(WS-ERR-MSG)
                   ERASE
               END-EXEC
               PERFORM 9000-TERM
           END-IF.
       H003-SQL-HANDLER-END. EXIT.
      ******************************************************************
      * SEND/RECEIVE MAPS FUNCTIONS                                    *
      ******************************************************************
       SEND-LOGIN-MAP.
           PERFORM H001-CLEAR-SCREEN
           EXEC CICS SEND MAP('LOGIN') MAPSET('ARI011') END-EXEC.
       SEND-LOGIN-MAP-END. EXIT.

       RECEIVE-LOGIN-MAP.
           EXEC CICS RECEIVE MAP('LOGIN') MAPSET('ARI011') END-EXEC.
           MOVE LIDI     TO WS-LID-I
           MOVE LTYPEI   TO WS-LTYPE-I
           MOVE LPWDI    TO WS-LPWD-I.
       RECEIVE-LOGIN-MAP-END. EXIT.

       SEND-REGIST-MAP.
           PERFORM H001-CLEAR-SCREEN
           EXEC CICS SEND MAP('REGIST') MAPSET('ARI011') END-EXEC.
       SEND-REGIST-MAP-END. EXIT.

       RECEIVE-REGIST-MAP.
           EXEC CICS RECEIVE MAP('REGIST') MAPSET('ARI011') END-EXEC.
           MOVE RIDI     TO WS-RID-I
           MOVE RNAMEI   TO WS-RNAME-I
           MOVE RPWDI    TO WS-RPWD-I
           MOVE RTYPEI   TO WS-RTYPE-I.
       RECEIVE-REGIST-MAP-END. EXIT.

       SEND-AGTM-MAP.
           PERFORM H001-CLEAR-SCREEN
           EXEC CICS SEND MAP('AGTM') MAPSET('ARI011') END-EXEC.
       SEND-AGTM-MAP-END. EXIT.

       RECEIVE-AGTM-MAP.
           EXEC CICS RECEIVE MAP('AGTM') MAPSET('ARI011') END-EXEC.
           MOVE AMOPI    TO WS-AMOP-I.
       RECEIVE-AGTM-MAP-END. EXIT.                          

       SEND-AGT1-MAP.
           PERFORM H001-CLEAR-SCREEN
           EXEC CICS SEND MAP('AGT1') MAPSET('ARI011') END-EXEC.
       SEND-AGT1-MAP-END. EXIT.

       RECEIVE-AGT1-MAP.
           EXEC CICS RECEIVE MAP('AGT1') MAPSET('ARI011') END-EXEC.
           MOVE A1DATEI  TO WS-A1DATE-I.
       RECEIVE-AGT1-MAP-END. EXIT.

       SEND-AGT2-MAP.
           PERFORM H001-CLEAR-SCREEN
           EXEC CICS SEND MAP('AGT2') MAPSET('ARI011') END-EXEC.
       SEND-AGT2-MAP-END. EXIT.

       RECEIVE-AGT2-MAP.
           EXEC CICS RECEIVE MAP('AGT2') MAPSET('ARI011') END-EXEC.
       RECEIVE-AGT2-MAP-END. EXIT.

       SEND-AGT3-MAP.
           PERFORM H001-CLEAR-SCREEN
           EXEC CICS SEND MAP('AGT3') MAPSET('ARI011') END-EXEC.
       SEND-AGT3-MAP-END. EXIT.

       RECEIVE-AGT3-MAP.
           EXEC CICS RECEIVE MAP('AGT3') MAPSET('ARI011') END-EXEC.
           MOVE A3YONI   TO WS-A3YON-I
           MOVE A3POCI(1:8)  TO WS-A3PWD-I
           MOVE A3POCI       TO WS-A3CID-I
           MOVE A3NAMEI  TO WS-A3NAME-I
           MOVE A3PNUMI  TO WS-A3PNUM-I
           MOVE A3DOBI   TO WS-A3DOB-I
           MOVE A3SSNI   TO WS-A3SSN-I
           MOVE A3PTYPEI TO WS-A3PTYPE-I
           MOVE A3TENI   TO WS-A3TEN-I
           MOVE A3SUMI   TO WS-A3SUM-I.
       RECEIVE-AGT3-MAP-END. EXIT.

       SEND-POLM-MAP.
           PERFORM H001-CLEAR-SCREEN
           EXEC CICS SEND MAP('POLM') MAPSET('ARI011') END-EXEC.
       SEND-POLM-MAP-END. EXIT.

       RECEIVE-POLM-MAP.
           EXEC CICS RECEIVE MAP('POLM') MAPSET('ARI011') END-EXEC.
           MOVE PMOPI    TO WS-PMOP-I.
       RECEIVE-POLM-MAP-END. EXIT.

       SEND-POL1-MAP.
           PERFORM H001-CLEAR-SCREEN
           EXEC CICS SEND MAP('POL1') MAPSET('ARI011') END-EXEC.
       SEND-POL1-MAP-END. EXIT.

       RECEIVE-POL1-MAP.
           EXEC CICS RECEIVE MAP('POL1') MAPSET('ARI011') END-EXEC.
       RECEIVE-POL1-MAP-END. EXIT.

       SEND-POL2-MAP.
           PERFORM H001-CLEAR-SCREEN
           EXEC CICS SEND MAP('POL2') MAPSET('ARI011') END-EXEC.
       SEND-POL2-MAP-END. EXIT.

       RECEIVE-POL2-MAP.
           EXEC CICS RECEIVE MAP('POL2') MAPSET('ARI011') END-EXEC.
       RECEIVE-POL2-MAP-END. EXIT.                  