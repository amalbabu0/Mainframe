       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEEK8.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN DD1
           ORGANIZATION SEQUENTIAL
           ACCESS MODE SEQUENTIAL
           FILE STATUS WS-FS.
       DATA DIVISION.
       FILE DIVISION.
       FD INFILE.
       01 OUTREC 
           05 IN-ACCT-NO           PIC 9(10).
           05 IN-CUST-NAME         PIC X(30).
           05 IN-MERCHANT-NAME     PIC X(30).
           05 IN-TRANS-NO          PIC 9(6).
           05 IN-TRANS-AMT         PIC 9(7)V99.
           05 IN-TRANS-DATE        PIC 9(8).
       WORKING-STORAGE SECTION.
       01 ACCT-TABLE.
          05 ACCT-ENTRY OCCURS 100 TIMES INDEXED BY ACCT-IDX.
             10 ACCT-NO        PIC 9(10).
             10 CUSTOMER-NAME  PIC X(30).
             10 TRANS-INFO OCCURS 200 TIMES INDEXED BY TRANS-IDX.
                15 MERCHANT-NAME      PIC X(30).
                15 TRANS-NO           PIC 9(6).
                15 TRANS-AMT          PIC 9(7)V99.
                15 TRANS-DATE         PIC 9(8).
       01 WS-FS          PIC 99.
       PROCEDURE DIVISION.
       0000-MAIN-PARA.
           PERFORM 1000-INIT-PARA.
           PERFORM 2000-PRFM-PARA.
           PERFORM 3000-TERM-PARA.
       1000-INIT-PARA.
            INITIALIZE WS-FS.
            SET ACCT-IDX  TO 1.
            SET TRANS-IDX TO 1.
       2000-PRFM-PARA.
            PERFORM 2100-OPEN-PARA.
            PERFORM 2200-READ-PARA.
            PERFORM 2300-CLOSE-PARA.
       3000-TERM-PARA.
            STOP RUN.
       2100-OPEN-PARA.
            OPEN INPUT INFILE.
            EVALUATE WS-FS
               WHEN 0
                   DISPLAY "OPEN SUSS"
               WHEN OTHER 
                   DISPLAY " ERROR OPEN"
            END-EVALUATE.
       2200-READ-PARA.
            READ INFILE.
            EVALUATE WS-FS
               WHEN 0
                    PERFORM UNTIL ACCT-IDX > 3
                           MOVE IN-ACCT-NO   TO ACCT-NO(ID-ACCT)
                           MOVE IN-CUST-NAME TO CUSTOMER-NAME
                           SET ACCT-IDX UP BY 1
                           PERFORM UNTIL TRANS-IDX > 4
                               MOVE IN-MERCHANT-NAME TO MERCHANT-NAME(ACCT-IDX,ID-TXN)
                               MOVE IN-TRANS-NO      TO IN-TRANS-NO(ACCT-IDX,ID-TXN)
                               MOVE IN-TRANS-AMT     TO TRANS-AMT(ACCT-IDX,ID-TXN)
                               MOVE IN-TRANS-DATE    TO TRANS-DATE(ACCT-IDX,ID-TXN)
                               SET TRANS-IDX UP BY 1
                           END-PERFORM
                    END-PERFORM.
               WHEN 10
                   DISPLAY  " NO RECORD FOUND "
            END-EVALUATE.
       2300-CLOSE-PARA.
            OPEN CLOSE INFILE.
            EVALUATE WS-FS
               WHEN 0
                   DISPLAY "CLOSE SUSS"
               WHEN OTHER 
                   DISPLAY " ERROR CLOSE"
            END-EVALUATE.
