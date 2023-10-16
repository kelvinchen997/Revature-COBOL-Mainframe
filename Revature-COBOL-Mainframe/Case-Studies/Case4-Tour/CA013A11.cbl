       IDENTIFICATION DIVISION.
       PROGRAM-ID. CA013A11.
      *   PROGRAM NAME: CA013A11
      *   INPUT: SQL SELECT FROM TOUR_DETAILS
      *   OUTPUT: TOUR.PS, ODEL.PS, SEASON_DISCOUNT, TOUR_DETAILS
      *   DESCRIPTION: ?
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT O-TOUR-PS ASSIGN TO OUTTOUR
               ORGANIZATION IS SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS FS-TOUR.
           SELECT O-ODEL-PS ASSIGN TO OUTODEL
               ORGANIZATION IS SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS FS-ODEL.
       DATA DIVISION.
       FILE SECTION.
       FD O-TOUR-PS.
       01 O-TOUR-REC.
           05 O-TOUR-TOUR-PLACE   PIC X(15).
           05 FILLER              PIC X(01).
           05 O-TOUR-FINAL-PRICE  PIC 9(07).
           05 FILLER              PIC X(01).
           05 O-TOUR-TOUR-GUIDE   PIC X(10).
           05 FILLER              PIC X(01).
           05 O-TOUR-DISCOUNT     PIC 9(09).
           05 FILLER              PIC X(01).
           05 O-TOUR-DATE.
               10 O-TOUR-YEAR  PIC 9(04).
               10 O-TOUR-DASH1 PIC X(01).
               10 O-TOUR-MONTH PIC 9(02).
               10 O-TOUR-DASH2 PIC X(01).
               10 O-TOUR-DAY   PIC 9(02).
           05 FILLER              PIC X(01).
           05 O-TOUR-GROUP-DIS    PIC 9(09).
           05 FILLER              PIC X(01).
           05 O-TOUR-GROUP-SIZE   PIC 9(02).
           05 FILLER              PIC X(12).
       FD O-ODEL-PS.
       01 O-ODEL-REC.
           05 O-ODEL-TOUR-PLACE   PIC X(15).
           05 FILLER              PIC X(01).
           05 O-ODEL-TOUR-GUIDE   PIC X(10).
           05 FILLER              PIC X(01).
           05 O-ODEL-LANGUAGE     PIC X(03).
           05 FILLER              PIC X(01).
           05 O-ODEL-DATE.
               10 O-ODEL-YEAR  PIC 9(04).
               10 O-ODEL-DASH1 PIC X(01).
               10 O-ODEL-MONTH PIC 9(02).
               10 O-ODEL-DASH2 PIC X(01).
               10 O-ODEL-DAY   PIC 9(02).
           05 FILLER              PIC X(01).
           05 O-ODEL-GROUP-SIZE   PIC 9(02).
           05 FILLER              PIC X(01).
           05 O-ODEL-PRICE-PHEAD  PIC 9(05).
           05 FILLER              PIC X(30).
       WORKING-STORAGE SECTION.
       77 WS-IS-NULL               PIC 9(01).
       01 WS-VARS.
           05 WS-ERR-MSG.
               10 WS-ERR-LENGTH    PIC S9(04) COMP VALUE 800.
               10 WS-ERR-TEXT      PIC X(80) OCCURS 10 TIMES.
           05 WS-ERR-LRECL         PIC S9(09) COMP VALUE 80.
           05 WS-INDICATORS.
               10 TOUR-GUIDE-NULL  PIC S9(04) COMP.
           05 WS-DEFINES.
               10 FILE-SUCCESS     PIC 9(02) VALUE 00.
               10 FILE-EOF         PIC 9(02) VALUE 10.
               10 DB2-SUCCESS      PIC 9(03) VALUE 000.
               10 DB2-EOF          PIC 9(03) VALUE 100.
               10 CALC-DISCOUNT    PIC X(08) VALUE 'CA023A11'.
               10 CALC-FINAL-PRICE PIC X(08) VALUE 'CA033A11'.
           05 WS-FILE-STATUS.
               10 FS-TOUR          PIC 9(02).
               10 FS-ODEL          PIC 9(02).
           05 WS-SUBPROGRAM-VARS.
               10 GROUP-SIZE     PIC S9(9) USAGE COMP.
               10 LANGUAGE       PIC X(3).
               10 PRICE-PER-HEAD PIC S9(9) USAGE COMP.
               10 DISCOUNT       PIC 9(09).
               10 FINAL-PRICE    PIC 9(07).
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           EXEC SQL
               INCLUDE DC013A11
           END-EXEC.
           EXEC SQL
               INCLUDE DC023A11
           END-EXEC.
      * DECLARE CURSOR TOUR_DETAILS INPUT
           EXEC SQL
               DECLARE CURSOR_TOUR_DETIALS CURSOR FOR
                   SELECT
                       *
                       FROM TOUR_DETAILS                                        
                       WHERE GROUP_SIZE > 1 AND
                             PRICE_PER_HEAD > 1000
           END-EXEC.
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-OPEN-IO
           PERFORM 2000-PROCESS-REC
           PERFORM 9000-TERM.
       0000-MAIN-END. EXIT.

       1000-OPEN-IO.
           PERFORM 1100-CURSOR-OPEN
           PERFORM 1200-OPEN-TOUR-PS
           PERFORM 1300-OPEN-ODEL-PS.
       1000-OPEN-IO-END. EXIT.

       1100-CURSOR-OPEN.
           EXEC SQL
               OPEN CURSOR_TOUR_DETIALS
           END-EXEC.                       
           EVALUATE TRUE
           WHEN SQLCODE = DB2-SUCCESS
               DISPLAY 'OPENED CURSOR & PERFORMED THE SELECT.'
           WHEN OTHER
               DISPLAY 'FAILED CURSOR OPEN OR SQL SELECT ERR.'
               DISPLAY 'SQL ERROR CODE: ' SQLCODE
               CALL 'DSNTIAR' USING SQLCA WS-ERR-MSG WS-ERR-LRECL
               DISPLAY WS-ERR-MSG
               PERFORM 9000-TERM
           END-EVALUATE.
       1100-CURSOR-OPEN-END. EXIT.

       1200-OPEN-TOUR-PS.
           OPEN OUTPUT O-TOUR-PS
           IF FS-TOUR = FILE-SUCCESS DISPLAY 'OPENED TOUR.PS' EXIT
           ELSE DISPLAY 'ERROR OPENING TOUR.PS, EC: ' FS-TOUR
               PERFORM 9000-TERM END-IF.
       1200-OPEN-TOUR-PS-END. EXIT.

       1300-OPEN-ODEL-PS.           
           OPEN OUTPUT O-ODEL-PS
           IF FS-ODEL = FILE-SUCCESS DISPLAY 'OPENED ODEL.PS' EXIT
           ELSE DISPLAY 'ERROR OPENING ODEL.PS, EC: ' FS-TOUR
               PERFORM 9000-TERM END-IF.
       1300-OPEN-ODEL-PS-END. EXIT.

       2000-PROCESS-REC.
           PERFORM UNTIL SQLCODE = DB2-EOF
               PERFORM 2100-FETCH-CURSOR
               IF SQLCODE = DB2-EOF
                   CONTINUE
               ELSE
                  PERFORM 2200-DISPLAY-REC
               END-IF
           END-PERFORM.
       2000-PROCESS-REC-END. EXIT.

       2100-FETCH-CURSOR.
           EXEC SQL
               FETCH CURSOR_TOUR_DETIALS INTO
                    :HV-TOUR-TOUR-PLACE
                   ,:HV-TOUR-TOUR-GUIDE :TOUR-GUIDE-NULL
                   ,:HV-TOUR-LANGUAGE
                   ,:HV-TOUR-TOUR-DATE
                   ,:HV-TOUR-GROUP-SIZE
                   ,:HV-TOUR-PRICE-PER-HEAD
           END-EXEC.
           EVALUATE TRUE
           WHEN SQLCODE = DB2-SUCCESS
               DISPLAY 'FETCHED REC: ' DCLTOUR-DETAILS
           WHEN SQLCODE = DB2-EOF
               DISPLAY 'END OF DB2 DATABASE REACHED'
           WHEN OTHER
               DISPLAY 'SQL ERROR: ' SQLCODE
               CALL 'DSNTIAR' USING SQLCA WS-ERR-MSG WS-ERR-LRECL
               DISPLAY WS-ERR-MSG
           END-EVALUATE.
       2100-FETCH-CURSOR-END. EXIT.

       2200-DISPLAY-REC.
           PERFORM 2210-CHECK-NULL.
           DISPLAY '  TOUR PLACE:     ' HV-TOUR-TOUR-PLACE
           DISPLAY '  TOUR GUIDE:     ' HV-TOUR-TOUR-GUIDE
           DISPLAY '  LANGUAGE:       ' HV-TOUR-LANGUAGE
           DISPLAY '  TOUR DATE:      ' HV-TOUR-TOUR-DATE
           DISPLAY '  GROUP SIZE:     ' HV-TOUR-GROUP-SIZE
           DISPLAY '  PRICE PER HEAD: ' HV-TOUR-PRICE-PER-HEAD
           IF WS-IS-NULL = 1 THEN
               DISPLAY 'NULL FOUND.'
               PERFORM 2211-WRITE-TO-ODEL
           ELSE
               PERFORM 2212-CALC-NUMBERS
               PERFORM 2213-INSERT-TO-SDIS
               PERFORM 2214-WRITE-TO-TOUR
               DISPLAY '----------------------------------------------'
           END-IF.
       2200-DISPLAY-REC-END. EXIT.

       2210-CHECK-NULL.
           EVALUATE TRUE
          WHEN(TOUR-GUIDE-NULL = -1)
               MOVE 1 TO WS-IS-NULL
           WHEN OTHER
               MOVE 0 TO WS-IS-NULL
           END-EVALUATE.
       2210-CHECK-NULL-END. EXIT.

       2211-WRITE-TO-ODEL.
           MOVE HV-TOUR-TOUR-PLACE     TO O-ODEL-TOUR-PLACE
           MOVE 'YTD'                  TO O-ODEL-TOUR-GUIDE
           MOVE HV-TOUR-LANGUAGE       TO O-ODEL-LANGUAGE
           MOVE HV-TOUR-TOUR-DATE      TO O-ODEL-DATE
           MOVE HV-TOUR-GROUP-SIZE     TO O-ODEL-GROUP-SIZE
           MOVE HV-TOUR-PRICE-PER-HEAD TO O-ODEL-PRICE-PHEAD
           WRITE O-ODEL-REC
           DISPLAY 'REC MOVED TO ODEL.PS.'
           DISPLAY 'DELETING REC FROM TABLE: TOUR_DETAILS'
           EXEC SQL
               DELETE FROM TOUR_DETAILS
                   WHERE TOUR_PLACE = :HV-TOUR-TOUR-PLACE
           END-EXEC.
           DISPLAY '---------------------------------------------'.
       2211-WRITE-TO-ODEL-END. EXIT.

       2212-CALC-NUMBERS.
           MOVE HV-TOUR-GROUP-SIZE     TO GROUP-SIZE
           MOVE HV-TOUR-LANGUAGE       TO LANGUAGE
           MOVE HV-TOUR-PRICE-PER-HEAD TO PRICE-PER-HEAD
      * CALC DISCOUNT
           PERFORM 2212A-CALC-DISCOUNT
           MOVE DISCOUNT               TO O-TOUR-DISCOUNT
      * CALC FINAL PRICE
           PERFORM 2212B-CALC-FINAL-PRICE
           MOVE FINAL-PRICE            TO O-TOUR-FINAL-PRICE
      * CALC GROUP PRICE
           COMPUTE O-TOUR-GROUP-DIS =
               HV-TOUR-GROUP-SIZE * O-TOUR-DISCOUNT
           DISPLAY 'DISCOUNT:       ' O-TOUR-DISCOUNT
           DISPLAY 'FINAL PRICE:    ' O-TOUR-FINAL-PRICE
           DISPLAY 'GROUP DISCOUNT: ' O-TOUR-GROUP-DIS.
       2212-CALC-NUMBERS-END. EXIT.

       2212A-CALC-DISCOUNT.
           EVALUATE TRUE
           WHEN (GROUP-SIZE = 5)
               COMPUTE DISCOUNT = PRICE-PER-HEAD * 0.01
           WHEN (GROUP-SIZE > 5   AND
                 GROUP-SIZE <= 10 AND
                 LANGUAGE = 'ENG')
               COMPUTE DISCOUNT = PRICE-PER-HEAD * 0.02
           WHEN (GROUP-SIZE > 10  AND
                 GROUP-SIZE <= 20)
               COMPUTE DISCOUNT = PRICE-PER-HEAD * 0.03
           WHEN (GROUP-SIZE > 20  AND
                 GROUP-SIZE <= 30 AND
                 LANGUAGE = 'ENG')
               COMPUTE DISCOUNT = PRICE-PER-HEAD * 0.018
           WHEN (GROUP-SIZE > 20  AND
                 GROUP-SIZE <= 30 AND
                 LANGUAGE = 'TAM')
               COMPUTE DISCOUNT = PRICE-PER-HEAD * 0.015
           WHEN OTHER
               COMPUTE DISCOUNT = 0
           END-EVALUATE.
       2212A-CALC-DISCOUNT-END. EXIT.

       2212B-CALC-FINAL-PRICE.
           EVALUATE TRUE
           WHEN (DISCOUNT >= 10   AND
                 DISCOUNT <= 20)
               COMPUTE FINAL-PRICE =
                   PRICE-PER-HEAD - (GROUP-SIZE * DISCOUNT) - 10
           WHEN (DISCOUNT >  20   AND
                 DISCOUNT <= 40)
               COMPUTE FINAL-PRICE =
                   PRICE-PER-HEAD - (GROUP-SIZE * DISCOUNT) - 12
           WHEN (DISCOUNT >  40   AND
                 DISCOUNT <= 50)
               COMPUTE FINAL-PRICE =
                   PRICE-PER-HEAD - (GROUP-SIZE * DISCOUNT) - 13
           WHEN (DISCOUNT >  50   AND
                 DISCOUNT <= 100)
               COMPUTE FINAL-PRICE =
                   PRICE-PER-HEAD - (GROUP-SIZE * DISCOUNT) - 9
           WHEN OTHER
               DISPLAY 'THIS SHOULD NOT HAPPEN.'
               COMPUTE FINAL-PRICE = 0
           END-EVALUATE.
       2212B-CALC-FINAL-PRICE-END. EXIT.

       2213-INSERT-TO-SDIS.
           MOVE HV-TOUR-TOUR-PLACE TO HV-SDIS-TOUR-PLACE
           MOVE FINAL-PRICE        TO HV-SDIS-FINAL-PRICE
           MOVE HV-TOUR-TOUR-GUIDE TO HV-SDIS-GUIDE
           MOVE DISCOUNT           TO HV-SDIS-DISCOUNT
           MOVE HV-TOUR-TOUR-DATE  TO HV-SDIS-DATE
           MOVE O-TOUR-GROUP-DIS   TO HV-SDIS-GROUP-DISCOUNT
           EXEC SQL
               INSERT INTO SEASON_DISCOUNT VALUES (
                :HV-SDIS-TOUR-PLACE
               ,:HV-SDIS-FINAL-PRICE
               ,:HV-SDIS-GUIDE
               ,:HV-SDIS-DISCOUNT
               ,:HV-SDIS-DATE
               ,:HV-SDIS-GROUP-DISCOUNT
               )
           END-EXEC.
       2213-INSERT-TO-SDIS-END. EXIT.

       2214-WRITE-TO-TOUR.
           MOVE HV-TOUR-TOUR-PLACE TO O-TOUR-TOUR-PLACE
      *    05 O-TOUR-FINAL-PRICE  PIC 9(07).
           MOVE HV-TOUR-TOUR-GUIDE TO O-TOUR-TOUR-GUIDE
      *    05 O-TOUR-DISCOUNT     PIC 9(09).
           MOVE HV-TOUR-TOUR-DATE  TO O-TOUR-DATE
      *    05 O-TOUR-GROUP-DIS    PIC 9(09).
           MOVE HV-TOUR-GROUP-SIZE TO O-TOUR-GROUP-SIZE
           WRITE O-TOUR-REC
           DISPLAY 'WRITTEN TO TOUR.PS, REC: ' O-TOUR-REC
           DISPLAY '--------------------------------------------------'.
       2214-WRITE-TO-TOUR-END. EXIT.

       9000-TERM.
           EXEC SQL
               CLOSE CURSOR_TOUR_DETIALS
           END-EXEC.
           CLOSE O-TOUR-PS
           CLOSE O-ODEL-PS
           DISPLAY 'STOPPING PROGRAM'
           STOP RUN.
       9000-TERM-END. EXIT.                                                                                                                                      