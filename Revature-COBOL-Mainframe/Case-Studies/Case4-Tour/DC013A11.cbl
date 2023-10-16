      ******************************************************************
      * DCLGEN TABLE(TOUR_DETAILS)                                     *
      *        LIBRARY(ARI011.KELVIN.ASGMTL3.TOUR.PDS(DC013A11))       *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(HV-TOUR-)                                         *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE TOUR_DETAILS TABLE
           ( TOUR_PLACE                     CHAR(15) NOT NULL,
             TOUR_GUIDE                     CHAR(10),
             LANGUAGE                       CHAR(3) NOT NULL,
             TOUR_DATE                      DATE NOT NULL,
             GROUP_SIZE                     INTEGER NOT NULL,
             PRICE_PER_HEAD                 INTEGER NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TOUR_DETAILS                       *
      ******************************************************************
       01  DCLTOUR-DETAILS.
           10 HV-TOUR-TOUR-PLACE   PIC X(15).
           10 HV-TOUR-TOUR-GUIDE   PIC X(10).
           10 HV-TOUR-LANGUAGE     PIC X(3).
           10 HV-TOUR-TOUR-DATE    PIC X(10).
           10 HV-TOUR-GROUP-SIZE   PIC S9(9) USAGE COMP.
           10 HV-TOUR-PRICE-PER-HEAD
              PIC S9(9) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************