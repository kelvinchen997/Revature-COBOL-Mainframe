      ******************************************************************
      * DCLGEN TABLE(TB_POLICY_DETAILS)                                *
      *        LIBRARY(ARI011.KELVIN.TRNING.FINAL.PDS(TBPOLDTL))       *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(HV-POL-)                                          *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE TB_POLICY_DETAILS TABLE
           ( POL_ID                         CHAR(10) NOT NULL,
             CUST_ID                        CHAR(10),
             AGENT_ID                       CHAR(10),
             POL_TYPE                       CHAR(3),
             PAID_DATE                      DATE,
             PREMIUM_AMNT                   DECIMAL(7, 0),
             PREM_PAY_DATE                  DATE,
             POL_STRT_DATE                  DATE,
             MATURITY_DATE                  DATE,
             SSN                            CHAR(11)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TB_POLICY_DETAILS                  *
      ******************************************************************
       01  DCLTB-POLICY-DETAILS.
           10 HV-POL-POL-ID        PIC X(10).
           10 HV-POL-CUST-ID       PIC X(10).
           10 HV-POL-AGENT-ID      PIC X(10).
           10 HV-POL-POL-TYPE      PIC X(3).
           10 HV-POL-PAID-DATE     PIC X(10).
           10 HV-POL-PREMIUM-AMNT  PIC S9(7)V USAGE COMP-3.
           10 HV-POL-PREM-PAY-DATE PIC X(10).
           10 HV-POL-POL-STRT-DATE PIC X(10).
           10 HV-POL-MATURITY-DATE PIC X(10).
           10 HV-POL-SSN           PIC X(11).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *             
      ******************************************************************
