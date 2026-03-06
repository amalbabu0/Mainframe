       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEEK3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN DD1
           ORGANIZATION SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE.
       01 OUTREC       
           10 DST-NAME    PIC A(10).
           10 VOTE-2019   PIC 9(10).
           10 VOTE-2020   PIC 9(10).
       WORKING-STORAGE SECTION.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           EXEC SQL.
               INCLUDE TABLE1
           END-EXEC.
           EXEC SQL
               DECLARE CURSOR CR1 FOR 
               SELECT DST_NAME, VOTE_2019, VOTE_2020 FROM TABLE1 ORDER BY DST_NAME
           END-EXEC.
       01 WS-FS            PIC 99.
       01 WS-COMPUTE       PIC 99.
       PROCEDURE DIVISION.
       0000-MAIN-PARA.
            PERFORM 1000-INIT-PARA
               THRU 1000-INIT-EXIT.
            PERFORM 2000-PFM-PARA
               THRU 2000-PFM-EXIT.
            PERFORM 3000-TERM-PARA.
       1000-INIT-PARA.
            INITIALIZE WS-FS.
       1000-INIT-EXIT.
            EXIT.
       2000-PFM-PARA.
            PERFORM 2100-OPEN-PARA
               THRU 2100-OPEN-EXIT
            PERFORM 2200-FETCH-PARA
               THRU 2200-FETCH-EXIT
               UNTIL SQLCODE = 100
            PERFORM 2300-CLOSE-PARA
               THRU 2300-CLOSE-EXIT.
       2000-PFM-EXIT.
            EXIT.
       3000-TERM-PARA.
            STOP RUN.
       2100-OPEN-PARA.
      *-----------------------------------------------------------
            EXEC SQL
               OPEN CR1
            END-EXEC.
             EVALUATE SQLCODE
                WHEN 00
                    DISPLAY "CURSOR OPEN SUCCUES"
                WHEN OTHER
                    DISPLAY "ERROR ON OPENING CURSOR " SQLCODE
             END-EVALUATE.
      *-----------------------------------------------------------
             OPEN OUTPUT OUTFILE.
             EVALUATE WS-FS.
                WHEN 00
                    DISPLAY "FILE OPEN SUCCUES"
                WHEN OTHER
                    DISPLAY "ERROR ON OPENING FILE " WS-FS.
             END-EVALUATE.
      *-----------------------------------------------------------
       2100-OPEN-EXIT.
            EXIT.
       2200-FETCH-PARA.
            EXEC SQL
               FETCH CR1 INTO :HV-DST-NAME,
                              :HV-VOTE-2019,
                              :HV-VOTE-2020
            END-EXEC.
      *-----------------------------------------------------------
            EVALUATE SQLCODE
                WHEN 00
                    COMPUTE WS-COMPUTE = ((HV-VOTE-2019 - HV-VOTE-2020) /
                                          VHV-VOTE-2019) * 100.
      *--------------------------------------------------------------
                    EVALUATE TRUE
                       WHEN WS-COMPUTE IS POSITIVE
                           MOVE DCLTABLE1 TO OUTREC.
                           STRING HV-DST-NAME DELIMITED BY SIZE
                                  "-DEC"      DELIMITED BY SIZE
                                  INTO DST-NAME
                           END-STRING.
                       WHEN WS-COMPUTE IS NEGATIVE
                           MOVE DCLTABLE1 TO OUTREC.
                           STRING HV-DST-NAME DELIMITED BY SIZE
                                  "-INC"      DELIMITED BY SIZE
                                  INTO DST-NAME
                           END-STRING.
                       WHEN OTHER
                            MOVE DCLTABLE1 TO OUTREC.
                    END-EVALUATE
                   WRITE OUTREC
	  *-----------------------------------------------------
                WHEN 100
                    DISPLAY "END REACHED"
                WHEN OTHER
                    DISPLAY "ERROR ON FETCH" SQLCODE
             END-EVALUATE.
       200-FETCH-EXIT.     
            EXIT
       2300-CLOSE-PARA.
      *-----------------------------------------------------------
            EXEC SQL
               CLOSE CR1
            END-EXEC.
             EVALUATE SQLCODE
                WHEN 00
                    DISPLAY "CURSOR CLOSE SUCCESS"
                WHEN OTHER
                    DISPLAY "ERROR ON CLOSING CURSOR " SQLCODE
             END-EVALUATE.
	  *--------------------------------------------------------
             OPEN OUTPUT OUTFILE.
             EVALUATE WS-FS.
                WHEN 00
                    DISPLAY "FILE CLOSED SUCCUES"
                WHEN OTHER
                    DISPLAY "ERROR ON CLOSING FILE " WS-FS.
             END-EVALUATE.
