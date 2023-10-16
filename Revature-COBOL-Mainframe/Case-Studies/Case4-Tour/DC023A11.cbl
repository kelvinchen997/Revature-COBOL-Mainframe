      ******************************************************************
      * DCLGEN TABLE(SEASON_DISCOUNT)                                  *
      *        LIBRARY(ARI011.KELVIN.ASGMTL3.TOUR.PDS(DC023A11))       *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(HV-SDIS-)                                         *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE SEASON_DISCOUNT TABLE
           ( TOUR_PLACE                     CHAR(15) NOT NULL,
             FINAL_PRICE                    INTEGER NOT NULL,
             GUIDE                          CHAR(10),
             DISCOUNT                       INTEGER NOT NULL,
             DATE                           DATE NOT NULL,
             GROUP_DISCOUNT                 INTEGER NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE SEASON_DISCOUNT                    *
      ******************************************************************
       01  DCLSEASON-DISCOUNT.
           10 HV-SDIS-TOUR-PLACE   PIC X(15).
           10 HV-SDIS-FINAL-PRICE  PIC S9(9) USAGE COMP.
           10 HV-SDIS-GUIDE        PIC X(10).
           10 HV-SDIS-DISCOUNT     PIC S9(9) USAGE COMP.
           10 HV-SDIS-DATE         PIC X(10).
           10 HV-SDIS-GROUP-DISCOUNT
              PIC S9(9) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************