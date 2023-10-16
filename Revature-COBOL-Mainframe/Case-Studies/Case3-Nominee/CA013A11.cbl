       IDENTIFICATION DIVISION.
       PROGRAM-ID. CA013A11.
      *   PROGRAM NAME: COBOL MAIN PROGRAM
      *   INPUT:
      *       ARI011.KELVIN.ASGMTL2.NOMINEE.PS2       AS: INDD1
      *   OUTPUT:
      *       ARI011.KELVIN.ASGMTL2.NOMINEE.KSDS      AS: OUTKSDS
      *       ARI011.KELVIN.ASGMTL2.NOMINEE.ERR       AS: OUTERRPS
      *       ARI011.KELVIN.ASGMTL2.NOMINEE.ECL       AS: OUTECLPS
      *       ARI011.KELVIN.ASGMTL2.NOMINEE.NCL       AS: OUTNCLPS
      *   DESCRIPTION:  ?
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT I-PS2 ASSIGN TO INDD1
               ORGANIZATION IS SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-FS-PS2.
           SELECT O-KSDS ASSIGN TO OUTKSDS
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS O-KSDS-ID
               FILE STATUS IS WS-FS-KSDS.
           SELECT O-ERR ASSIGN TO OUTERRPS
               ORGANIZATION IS SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-FS-ERR.
           SELECT O-ECL ASSIGN TO OUTECLPS
               ORGANIZATION IS SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-FS-ECL.
           SELECT O-NCL ASSIGN TO OUTNCLPS
               ORGANIZATION IS SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-FS-NCL.
       DATA DIVISION.
       FILE SECTION.
       FD I-PS2.
       01 I-PS2-REC.
           05 I-PS2-ID          PIC X(05).
           05 FILLER            PIC X(01).
           05 I-PS2-DOB.
               10 I-PS2-DAY     PIC X(02).
               10 I-PS2-DASH1   PIC X(01).
               10 I-PS2-MONTH   PIC X(02).
               10 I-PS2-DASH2   PIC X(01).
               10 I-PS2-YEAR    PIC X(04).
           05 FILLER            PIC X(01).
           05 I-PS2-RELATION    PIC A(04).
           05 FILLER            PIC X(01).
           05 I-PS2-SALARY      PIC 9(06).9(02).
           05 FILLER            PIC X(49).
       FD O-KSDS.
       01 O-KSDS-REC.
           05 O-KSDS-ID         PIC X(05).
           05 FILLER            PIC X(01).
           05 O-KSDS-DOB.
               10 O-KSDS-DAY    PIC 9(02).
               10 O-KSDS-DASH1  PIC X(01).
              10 O-KSDS-MONTH  PIC 9(02).
              10 O-KSDS-DASH2  PIC X(01).
              10 O-KSDS-YEAR   PIC 9(04).
          05 FILLER            PIC X(01).
          05 O-KSDS-RELATION   PIC A(04).
          05 FILLER            PIC X(01).
          05 O-KSDS-SALARY     PIC 9(06).9(02).
          05 FILLER            PIC X(01).
          05 O-KSDS-DISCOUNT   PIC 9(06).
          05 FILLER            PIC X(01).
          05 O-KSDS-PAID       PIC 9(06).9(02).
          05 FILLER            PIC X(01).
          05 O-KSDS-CLAIM      PIC A(01).
          05 FILLER            PIC X(30).
       FD O-ERR.
       01 O-ERR-REC.
           05 O-ERR-ID          PIC X(05).
           05 FILLER            PIC X(01).
           05 O-ERR-DOB.
               10 O-ERR-DAY     PIC 9(02).
               10 O-ERR-DASH1   PIC X(01).
               10 O-ERR-MONTH   PIC 9(02).
               10 O-ERR-DASH2   PIC X(01).
               10 O-ERR-YEAR    PIC 9(04).
           05 FILLER            PIC X(01).
           05 O-ERR-RELATION    PIC A(04).
           05 FILLER            PIC X(01).
           05 O-ERR-SALARY      PIC 9(06).9(02).
           05 FILLER            PIC X(49).
       FD O-ECL.
       01 O-ECL-REC.
           05 O-ECL-ID          PIC X(05).
           05 FILLER            PIC X(01).
           05 O-ECL-DOB.
               10 O-ECL-DAY     PIC 9(02).
               10 O-ECL-DASH1   PIC X(01).
               10 O-ECL-MONTH   PIC 9(02).
               10 O-ECL-DASH2   PIC X(01).
               10 O-ECL-YEAR    PIC 9(04).
           05 FILLER            PIC X(01).
           05 O-ECL-RELATION    PIC A(04).
           05 FILLER            PIC X(01).
           05 O-ECL-SALARY      PIC 9(06).9(02).
           05 FILLER            PIC X(01).
           05 O-ECL-CLAIM       PIC A(01).
           05 FILLER            PIC X(47).
       FD O-NCL.
       01 O-NCL-REC.
           05 O-NCL-ID          PIC X(05).
           05 FILLER            PIC X(01).
           05 O-NCL-DOB.
               10 O-NCL-DAY     PIC 9(02).
               10 O-NCL-DASH1   PIC X(01).
               10 O-NCL-MONTH   PIC 9(02).
               10 O-NCL-DASH2   PIC X(01).
               10 O-NCL-YEAR    PIC 9(04).
           05 FILLER            PIC X(01).
           05 O-NCL-RELATION    PIC A(04).
           05 FILLER            PIC X(01).
           05 O-NCL-SALARY      PIC 9(06).9(02).
           05 FILLER            PIC X(01).
           05 O-NCL-CLAIM       PIC A(01).
           05 FILLER            PIC X(47).
       WORKING-STORAGE SECTION.
       01 WS-VARS.
           05 WS-STATUS-CODES.
               10 WS-FS-PS2  PIC 9(02).
                   88 FS-PS2-SUCC VALUE 00.
                   88 FS-PS2-EOF  VALUE 10.
               10 WS-FS-KSDS PIC 9(02).
                   88 FS-KSDS-SUCC VALUE 00.
                   88 FS-KSDS-EOF  VALUE 10.
               10 WS-FS-ERR  PIC 9(02).
                   88 FS-ERR-SUCC VALUE 00.
                   88 FS-ERR-EOF  VALUE 10.
               10 WS-FS-ECL  PIC 9(02).
                   88 FS-ECL-SUCC VALUE 00.
                   88 FS-ECL-EOF  VALUE 10.
               10 WS-FS-NCL  PIC 9(02).
                   88 FS-NCL-SUCC VALUE 00.
                   88 FS-NCL-EOF  VALUE 10.
           05 WS-DEFINES.
               10 SUCCESS        PIC 9(02) VALUE 00.
               10 END-OF-FILE    PIC 9(02) VALUE 10.
               10 DISCPAID       PIC X(08) VALUE 'CA023A11'.
           05 WS-PGM-VARS.
               10 WS-REC-COUNT   PIC 9(03) VALUE ZEROES.
               10 WS-INDEX       PIC 9(02) VALUE ZEROES.
               10 WS-INCOME      PIC 9(06)V9(02).
               10 WS-DISCOUNT    PIC 9(06).
               10 WS-PAID        PIC 9(06)V9(02).
               10 WS-CLAIM-TEMP  PIC A(01).
           05 WS-ARRAY OCCURS 1 TO 50 TIMES DEPENDING ON WS-REC-COUNT.
               10 WS-ID          PIC X(05).
               10 WS-DOB         PIC X(10).
               10 WS-RELATION    PIC A(04).
               10 WS-SALARY      PIC 9(06).9(02).
               10 WS-CLAIM       PIC A(01).
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 0000-CALC-INPUT-LGTH
           PERFORM 1000-INIT-OPEN
           PERFORM 2000-PROCESS-REC
           PERFORM 3000-WRITE-ECL-NCL

           PERFORM 9000-TERM.
       0000-MAIN-END. EXIT.

       0000-CALC-INPUT-LGTH.
           OPEN INPUT I-PS2
           PERFORM UNTIL FS-PS2-EOF
               READ I-PS2
               IF (FS-PS2-EOF) EXIT
               ELSE ADD 1 TO WS-REC-COUNT END-IF
           END-PERFORM.
           DISPLAY 'NUMBER OF INPUT RECORDS: ' WS-REC-COUNT
           CLOSE I-PS2.
       0000-CALC-INPUT-LGTH-END. EXIT.

       1000-INIT-OPEN.
           OPEN INPUT I-PS2
           PERFORM 1100-OPEN-PS2-VALID
           OPEN OUTPUT O-KSDS
           PERFORM 1200-OPEN-KSDS-VALID
           OPEN OUTPUT O-ERR
           PERFORM 1300-OPEN-ERR-VALID
           OPEN OUTPUT O-ECL
           PERFORM 1400-OPEN-ECL-VALID
           OPEN OUTPUT O-NCL
           PERFORM 1500-OPEN-NCL-VALID.
       1000-INIT-OPEN-END. EXIT.

       1100-OPEN-PS2-VALID.
           IF FS-PS2-SUCC DISPLAY 'OPENED PS2' EXIT
           ELSE DISPLAY 'ERROR OPENING PS2. EC: ' WS-FS-PS2
               PERFORM 9000-TERM END-IF.
       1100-OPEN-PS2-VALID-END. EXIT.

       1200-OPEN-KSDS-VALID.
           IF FS-KSDS-SUCC DISPLAY 'OPENED KSDS' EXIT
           ELSE DISPLAY 'ERROR OPENING KSDS. EC: ' WS-FS-KSDS
               PERFORM 9000-TERM END-IF.
       1200-OPEN-KSDS-VALID-END. EXIT.

       1300-OPEN-ERR-VALID.
           IF FS-ERR-SUCC DISPLAY 'OPENED ERR' EXIT
           ELSE DISPLAY 'ERROR OPENING ERR. EC: ' WS-FS-ERR
               PERFORM 9000-TERM END-IF.
       1300-OPEN-ERR-VALID-END. EXIT.

       1400-OPEN-ECL-VALID.
           IF FS-ECL-SUCC DISPLAY 'OPENED ECL' EXIT
           ELSE DISPLAY 'ERROR OPENING ECL. EC: ' WS-FS-ECL
               PERFORM 9000-TERM END-IF.
       1400-OPEN-ECL-VALID-END. EXIT.

       1500-OPEN-NCL-VALID.
           IF FS-NCL-SUCC DISPLAY 'OPENED NCL' EXIT
           ELSE DISPLAY 'ERROR OPENING NCL. EC: ' WS-FS-NCL
               PERFORM 9000-TERM END-IF.               
       1500-OPEN-NCL-VALID-END. EXIT.

       2000-PROCESS-REC.
           PERFORM UNTIL FS-PS2-EOF
      * READ A RECORD FROM INPUT
               READ I-PS2
               IF (WS-FS-PS2 NOT EQUAL TO END-OF-FILE) THEN
                   PERFORM 2100-VALID-INPUT-REC
               END-IF
           END-PERFORM.
       2000-PROCESS-REC-END. EXIT.

       2100-VALID-INPUT-REC.
           IF (I-PS2-DAY IS NOT NUMERIC OR
               I-PS2-MONTH IS NOT NUMERIC OR
               I-PS2-YEAR IS NOT NUMERIC OR
               I-PS2-RELATION = '    ' OR
               I-PS2-SALARY(1:6) IS NOT NUMERIC OR
               I-PS2-SALARY(8:2) IS NOT NUMERIC
              ) THEN
               PERFORM 2110-REC-TO-ERR
           ELSE
               PERFORM 2120-PROC-VALID-REC
           END-IF.
       2100-VALID-INPUT-REC-END. EXIT.

       2110-REC-TO-ERR.
           DISPLAY 'FOUND RECORD ERROR. REC: ' I-PS2-REC
           MOVE I-PS2-REC TO O-ERR-REC
           WRITE O-ERR-REC
           DISPLAY 'WROTE RECORD TO ERR FILE.'
           DISPLAY '--------------------------------------------------'.
       2110-REC-TO-ERR-END. EXIT.

       2120-PROC-VALID-REC.
           DISPLAY 'FOUND VALID RECORD. REC: ' I-PS2-REC
           PERFORM 2121-CHECK-ELIG
           PERFORM 2122-CALC-DISCPAID
           PERFORM 2123-RELATION-INC
           PERFORM 2124-WRITE-KSDS
           PERFORM 2125-ADD-TO-ARR.
       2120-PROC-VALID-REC-END. EXIT.

       2121-CHECK-ELIG.
           IF (I-PS2-YEAR <= 1985) THEN
               DISPLAY I-PS2-YEAR '<=1985, MOVED N TO CLAIM'
               MOVE 'N' TO O-KSDS-CLAIM, WS-CLAIM-TEMP
           ELSE
               DISPLAY I-PS2-YEAR '>1985, MOVED Y TO CLAIM'
               MOVE 'Y' TO O-KSDS-CLAIM, WS-CLAIM-TEMP
           END-IF.
       2121-CHECK-ELIG-END. EXIT.

       2122-CALC-DISCPAID.
           MOVE I-PS2-SALARY TO WS-INCOME
           DISPLAY '   CALCULATING DISCOUNT AND PAID:'
           CALL DISCPAID USING WS-INCOME, WS-DISCOUNT, WS-PAID.
           DISPLAY '      PAID     = ' WS-PAID
           DISPLAY '      DISCOUNT = ' WS-DISCOUNT.
       2122-CALC-DISCPAID-END. EXIT.

       2123-RELATION-INC.
           DISPLAY '   RELATION IS : ' I-PS2-RELATION
           EVALUATE TRUE
           WHEN (I-PS2-RELATION = 'SELF')
               COMPUTE WS-INCOME = WS-INCOME + ( 0.4 * WS-PAID )
               DISPLAY '      40% INCR, NEW INCOME: ' WS-INCOME
           WHEN (I-PS2-RELATION = 'HUS')
               COMPUTE WS-INCOME = WS-INCOME + ( 0.3 * WS-PAID )
               DISPLAY '      30% INCR, NEW INCOME: ' WS-INCOME
           WHEN OTHER
               COMPUTE WS-INCOME = WS-INCOME + ( 0.3 * WS-PAID )
               DISPLAY '      30% INCR, NEW INCOME: ' WS-INCOME
           END-EVALUATE.
       2123-RELATION-INC-END. EXIT.

       2124-WRITE-KSDS.
           MOVE I-PS2-ID       TO O-KSDS-ID
      *    KEY PLACED, FOR KSDS WRITE
           MOVE I-PS2-DOB      TO O-KSDS-DOB
           MOVE I-PS2-RELATION TO O-KSDS-RELATION
           MOVE WS-INCOME      TO O-KSDS-SALARY
           MOVE WS-DISCOUNT    TO O-KSDS-DISCOUNT
           MOVE WS-PAID        TO O-KSDS-PAID
      *    CLAIM FIELD WAS ALREADY MOVED
           DISPLAY 'WRITING TO KSDS, REC: ' O-KSDS-REC
           WRITE O-KSDS-REC.
           DISPLAY '--------------------------------------------------'
       2124-WRITE-KSDS-END. EXIT.

       2125-ADD-TO-ARR.
           ADD 1 TO WS-INDEX
           MOVE I-PS2-ID       TO WS-ID(WS-INDEX)
           MOVE I-PS2-DOB      TO WS-DOB(WS-INDEX)
           MOVE I-PS2-RELATION TO WS-RELATION(WS-INDEX)
           MOVE WS-INCOME      TO WS-SALARY(WS-INDEX)
           MOVE WS-CLAIM-TEMP  TO WS-CLAIM(WS-INDEX).
       2125-ADD-TO-ARR-END. EXIT.

       3000-WRITE-ECL-NCL.
           DISPLAY ' '
           DISPLAY 'POPULATING ECL AND NCL'
           PERFORM WS-INDEX TIMES
               DISPLAY 'REC: ' WS-ARRAY(WS-INDEX)
               DISPLAY '   CLAIM: ' WS-CLAIM(WS-INDEX)
               IF (WS-CLAIM(WS-INDEX) = 'Y')
                   PERFORM 3100-WRITE-ECL
               ELSE
                   PERFORM 3200-WRITE-NCL
               END-IF
               SUBTRACT 1 FROM WS-INDEX
           END-PERFORM.
       3000-WRITE-ECL-NCL-END. EXIT.

       3100-WRITE-ECL.
           MOVE WS-ID(WS-INDEX)       TO O-ECL-ID
           MOVE WS-DOB(WS-INDEX)      TO O-ECL-DOB
           MOVE WS-RELATION(WS-INDEX) TO O-ECL-RELATION
           MOVE WS-SALARY(WS-INDEX)   TO O-ECL-SALARY
           MOVE WS-CLAIM(WS-INDEX)    TO O-ECL-CLAIM
           DISPLAY ' WRITING TO ECL, REC: ' O-ECL-REC
           DISPLAY '--------------------------------------------------'.
           WRITE O-ECL-REC.
       3100-WRITE-ECL-END. EXIT.

       3200-WRITE-NCL.
           MOVE WS-ID(WS-INDEX)       TO O-NCL-ID
           MOVE WS-DOB(WS-INDEX)      TO O-NCL-DOB
           MOVE WS-RELATION(WS-INDEX) TO O-NCL-RELATION
           MOVE WS-SALARY(WS-INDEX)   TO O-NCL-SALARY
           MOVE WS-CLAIM(WS-INDEX)    TO O-NCL-CLAIM
           DISPLAY ' WRITING TO NCL, REC: ' O-NCL-REC
           DISPLAY '--------------------------------------------------'.
           WRITE O-NCL-REC.
       3200-WRITE-NCL-END. EXIT.

       9000-TERM.
           CLOSE I-PS2
           CLOSE O-KSDS
           CLOSE O-ERR
           CLOSE O-ECL
           CLOSE O-NCL
           DISPLAY 'STOPPING PROGRAM'
           STOP RUN.
       9000-TERM-END. EXIT.                             