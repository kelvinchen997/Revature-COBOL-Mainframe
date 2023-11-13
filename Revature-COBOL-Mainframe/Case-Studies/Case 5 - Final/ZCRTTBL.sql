CREATE TABLE TB_ACCOUNTS
    (
       ID        CHAR(10) NOT NULL PRIMARY KEY
      ,NAME      VARCHAR(20)
      ,PASSWORD  CHAR(08)
      ,TYPE      CHAR(01) CHECK (TYPE IN ('A','P'))
    ) IN ARI011DB.ARI011TS;
CREATE UNIQUE INDEX UID_1 ON TB_ACCOUNTS(ID);
------------------------------------------------------------------------
CREATE TABLE TB_POLICY_DETAILS
    (
       POL_ID        CHAR(10) NOT NULL PRIMARY KEY
      ,CUST_ID       CHAR(10)
      ,AGENT_ID      CHAR(10)
      ,POL_TYPE      CHAR(03)
      ,PAID_DATE     DATE
      ,PREMIUM_AMNT  DECIMAL(7,0)
      ,PREM_PAY_DATE DATE
      ,POL_STRT_DATE DATE
      ,MATURITY_DATE DATE
      ,SSN           CHAR(11)
      CHECK (POL_TYPE IN ('ACC','LIF','HEA','VEH'))
    ) IN ARI011DB.ARI011TS;
CREATE UNIQUE INDEX UID_2 ON TB_POLICY_DETAILS(POL_ID);
------------------------------------------------------------------------
CREATE TABLE TB_POLICY_HOLDER_DETAILS
    (
       POL_ID        CHAR(10)
      ,CUST_ID       CHAR(10)
      ,AGENT_ID      CHAR(10)
      ,NAME          VARCHAR(20)
      ,PHONE         CHAR(12)
      ,DOB           DATE
      ,SSN           CHAR(11)
      ,POL_TYPE      CHAR(03)
      ,TENURE_YEARS  CHAR(2)
      ,SUM_ASSURED   DECIMAL(7,0)
      CHECK (POL_TYPE IN ('ACC','LIF','HEA','VEH'))
    ) IN ARI011DB.ARI011TS;      
------------------------------------------------------------------------    