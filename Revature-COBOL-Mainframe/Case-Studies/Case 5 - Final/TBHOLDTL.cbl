     ******************************************************************
      * DCLGEN TABLE(TB_POLICY_HOLDER_DETAILS)                         *
      *        LIBRARY(ARI011.KELVIN.TRNING.FINAL.PDS(TBHOLDTL))       *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(HV-HOL-)                                          *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE TB_POLICY_HOLDER_DETAILS TABLE
           ( POL_ID                         CHAR(10),
             CUST_ID                        CHAR(10),
             AGENT_ID                       CHAR(10),
             NAME                           VARCHAR(20),
             PHONE                          CHAR(12),
             DOB                            DATE,
             SSN                            CHAR(11),
             POL_TYPE                       CHAR(3),
             TENURE_YEARS                   CHAR(2),
             SUM_ASSURED                    DECIMAL(7, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TB_POLICY_HOLDER_DETAILS           *
      ******************************************************************
       01  DCLTB-POLICY-HOLDER-DETAILS.
           10 HV-HOL-POL-ID        PIC X(10).
           10 HV-HOL-CUST-ID       PIC X(10).
           10 HV-HOL-AGENT-ID      PIC X(10).
           10 HV-HOL-NAME.
              49 HV-HOL-NAME-LEN   PIC S9(4) USAGE COMP.
              49 HV-HOL-NAME-TEXT  PIC X(20).
           10 HV-HOL-PHONE         PIC X(12).
           10 HV-HOL-DOB           PIC X(10).
           10 HV-HOL-SSN           PIC X(11).
           10 HV-HOL-POL-TYPE      PIC X(3).
           10 HV-HOL-TENURE-YEARS  PIC X(2).
           10 HV-HOL-SUM-ASSURED   PIC S9(7)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************                        