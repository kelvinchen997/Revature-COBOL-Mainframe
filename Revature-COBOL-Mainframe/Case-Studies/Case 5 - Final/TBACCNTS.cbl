      ******************************************************************
      * DCLGEN TABLE(TB_ACCOUNTS)                                      *
      *        LIBRARY(ARI011.KELVIN.TRNING.FINAL.PDS(TBACCNTS))       *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(HV-ACC-)                                          *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE TB_ACCOUNTS TABLE
           ( ID                             CHAR(10) NOT NULL,
             NAME                           VARCHAR(20),
             PASSWORD                       CHAR(8),
             TYPE                           CHAR(1)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TB_ACCOUNTS                        *
      ******************************************************************
       01  DCLTB-ACCOUNTS.
           10 HV-ACC-ID            PIC X(10).
           10 HV-ACC-NAME.
              49 HV-ACC-NAME-LEN   PIC S9(4) USAGE COMP.
              49 HV-ACC-NAME-TEXT  PIC X(20).
           10 HV-ACC-PASSWORD      PIC X(8).
           10 HV-ACC-TYPE          PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************      