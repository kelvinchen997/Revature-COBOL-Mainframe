       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMMAP.
      *   PROGRAM NAME: PROGRAM CHECK MAPS
      *   INPUT: INPUT FROM CICS TERMINAL
      *   OUTPUT: DB2 DATABASE, CICS TERMINAL,
      *   DESCRIPTION: JUST DISPLAYS ALL MAPS IN MAPSET(ARI011)
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 VARS.
          05 CICS-VARS.
             10 CICS-SYS-VARS.
                15 WS-CLEAR      PIC X(75) VALUE ' '.
           COPY ARI011.
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT

           PERFORM 9000-TERM.
       0000-MAIN-END. EXIT.

       1000-INIT.
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
           EXEC CICS SEND
               MAP('LOGIN') MAPSET('ARI011')
           END-EXEC.
           EXEC CICS RECEIVE
               MAP('LOGIN') MAPSET('ARI011')
           END-EXEC.
           PERFORM H001-CLEAR-SCREEN.
           EXEC CICS SEND       
               MAP('REGIST') MAPSET('ARI011')
           END-EXEC.
           EXEC CICS RECEIVE
               MAP('REGIST') MAPSET('ARI011')
           END-EXEC.
           PERFORM H001-CLEAR-SCREEN.
           EXEC CICS SEND
               MAP('AGTM') MAPSET('ARI011')
           END-EXEC.
           EXEC CICS RECEIVE
               MAP('AGTM') MAPSET('ARI011')
           END-EXEC.
           PERFORM H001-CLEAR-SCREEN.
           EXEC CICS SEND
               MAP('AGT1') MAPSET('ARI011')
           END-EXEC.
           EXEC CICS RECEIVE
               MAP('AGT1') MAPSET('ARI011')
           END-EXEC.
           PERFORM H001-CLEAR-SCREEN.           
           EXEC CICS SEND
               MAP('AGT2') MAPSET('ARI011')
           END-EXEC.
           EXEC CICS RECEIVE
               MAP('AGT2') MAPSET('ARI011')
           END-EXEC.
           PERFORM H001-CLEAR-SCREEN.
           EXEC CICS SEND
               MAP('AGT3') MAPSET('ARI011')
           END-EXEC.
           EXEC CICS RECEIVE
               MAP('AGT3') MAPSET('ARI011')
           END-EXEC.
           PERFORM H001-CLEAR-SCREEN.
           EXEC CICS SEND
               MAP('AGT4') MAPSET('ARI011')
           END-EXEC.
           EXEC CICS RECEIVE
               MAP('AGT4') MAPSET('ARI011')
           END-EXEC.
           PERFORM H001-CLEAR-SCREEN.
           EXEC CICS SEND
               MAP('POLM') MAPSET('ARI011')
           END-EXEC.
           EXEC CICS RECEIVE
               MAP('POLM') MAPSET('ARI011')
           END-EXEC.
           PERFORM H001-CLEAR-SCREEN.
           EXEC CICS SEND
               MAP('POL1') MAPSET('ARI011')
           END-EXEC.
           EXEC CICS RECEIVE
               MAP('POL1') MAPSET('ARI011')
           END-EXEC.
           PERFORM H001-CLEAR-SCREEN.
           EXEC CICS SEND
               MAP('POL2') MAPSET('ARI011')
           END-EXEC.
           EXEC CICS RECEIVE
               MAP('POL2') MAPSET('ARI011')
           END-EXEC.
           PERFORM H001-CLEAR-SCREEN.
       1000-INIT-END. EXIT.


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
