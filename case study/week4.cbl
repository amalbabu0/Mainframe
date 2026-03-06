COBOL
************************************************************************ 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEEK4.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *------------------------------------------------------
           SELECT INFILE ASSIGN DD1
           ORGANIZATION SEQUENTIAL
           ACCESS MODE SEQUENTIAL
           FILE STATUS WS-FS1.
      *------------------------------------------------------
           SELECT OUTFILE ASSIGN DD2
           ORGANIZATION SEQUENTIAL
           ACCESS MODE SEQUENTIAL
           FILE STATUS WS-FS2.
      *------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *------------------------------------------------------
       FD INFILE.
       01 INREC.
           10 I-TRN-DTE	       PIC X(10)  
           10 I-ACCT-NBR	   PIC X(19)
           10 I-REF-NBR	       PIC X(23)
           10 I-TRN-AMT	       PIC S9(10)V9(2)
           10 I-FEE-PGM-IND	   PIC X(03)
      *------------------------------------------------------
       FD OUTFILE.
       01 OUTREC               
           10 O-TRN-DTE	       PIC X(10)  
           10 O-REF-NBR	       PIC X(23)
           10 O-FEE-PGM-IND	   PIC X(3)
           10 ORIG-CURR-CDE    PIC X(3)
           10 FILLER           PIC X(41)
      *------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 WS-FS1               PIC 99.
       01 WS-FS2               PIC 99.
       01 WS-CTR               PIC 999.
       01 WS-D1
           10 D1-YEAR          PIC 9(4).
           10 D1-MONTH         PIC 9(2).
           10 D1-DAY           PIC 9(2).
       01 WS-D2
           10 D2-YEAR          PIC 9(4).
           10 D2-MONTH         PIC 9(2).
           10 D2-DAY           PIC 9(2).
       01 PIO0FRD-O-FRD-IND    PIC X(02)
       LINKAGE SECTION.
       01 LK-PARM-DATE         PIC X(10).
       PROCEDURE DIVISION USING LK-PARM-DATE.
       0000-MAIN-PARA.
            PERFORM 1000-INIT-PARA
               THRU 1000-INIT-EXIT
            PERFORM 2000-PFM-PARA
               THRU 2000-PFM-EXIT
            PERFORM 3000-TERM-PARA.
       1000-INIT-PARA.
            INITIALIZE WS-FS1 WS-FS2 WS-CTR.
       1000-INIT-EXIT.
            EXIT.
       2000-PFM-PARA.
            PERFORM 2100-OPEN-PARA
               THRU 2100-OPEN-EXIT
            PERFORM 2200-READ-PARA
               THRU 2200-READ-EXIT
               UNTIL WS-FS1 = 10
            PERFORM 2300-CLOSE-PARA
               THRU 2300-CLOSE-EXIT.
       2000-PFM-EXIT.
            EXIT.
       3000-TERM-PARA.
            STOP RUN.
       2100-OPEN-PARA.
      *------------------------------------------------------
            OPEN INPUT INFILE.
            EVALUATE TRUE
               WHEN WS-FS1 = 00
                   DISPLAY "OPEN INFILE SUCCESS"
               WHEN WS-FS1 = 10 AND WS-CTR = 0
                   DISPLAY "NO RECORD FOUND"
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
               WHEN OTHER
                   DISPLAY "OPEN ERROR INFILE: " WS-FS1
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
            END-EVALUATE
      *------------------------------------------------------
            OPEN OUTPUT OUTFILE.
            EVALUATE WS-FS2
               WHEN 00
                   DISPLAY "OPEN OUTFILE SUCCESS"
               WHEN OTHER
                   DISPLAY "OPEN ERROR OUTFILE: " WS-FS2
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
            END-EVALUATE
      *------------------------------------------------------
       2100-OPEN-EXIT.
            EXIT.
       2200-READ-PARA.
            READ INFILE.
            EVALUATE WS-FS1
               WHEN 00
                   ADD 1 TO WS-CTR.
                   PERFORM 2210-VALIDATE-PARA
                      THRU 2210-VALIDATE-EXIT.
               WHEN 10
                   DISPLAY "END REACHED"
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
               WHEN OTHER
                   DISPLAY "OPEN ERROR INFILE: " WS-FS1
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
            END-EVALUATE
       2200-READ-EXIT.
            EXIT.
       2300-CLOSE-PARA.
      *------------------------------------------------------
            CLOSE INFILE.
            EVALUATE TRUE
               WHEN WS-FS1 = 00
                   DISPLAY "CLOSE INFILE SUCCESS"
               WHEN OTHER
                   DISPLAY "CLOSE ERROR INFILE: " WS-FS1
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
            END-EVALUATE
      *------------------------------------------------------
            CLOSE OUTFILE.
            EVALUATE WS-FS2
               WHEN 00
                   DISPLAY "CLOSE OUTFILE SUCCESS"
               WHEN OTHER
                   DISPLAY "CLOSE ERROR OUTFILE: " WS-FS2
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
            END-EVALUATE
      *------------------------------------------------------
       2300-CLOSE-EXIT.
            EXIT.
       2210-VALIDATE-PARA.
            UNSTRING I-TRN-DTE DELIMITED BY '/'
               INTO D1-YEAR D1-MONTH D1-DAY
            END-UNSTRING.
            UNSTRING LK-PARM-DATE DELIMITED BY '/'
               INTO D2-YEAR D2-MONTH D2-DAY
            END-UNSTRING.
            CALL 'P400FRD' USING I-ACCT-NBR PIO0FRD-O-FRD-IND.
            EVALUATE TRUE
               WHEN (WS-D2 < WS-D1) AND (PIO0FRD-O-FRD-IND = 'FD' OR 
                                         PIO0FRD-O-FRD-IND = 'FS')
                   CONTINUE
               WHEN OTHER
                   PERFORM 2213-MOVE-PARA
                      THRU 2213-MOVE-EXIT
            END-EVALUATE.
            PERFORM 2221-WRITE-PARA
               THRU 2221-WRITE-EXIT.
       2210-VALIDATE-EXIT.
            EXIT.
       2213-MOVE-PARA.
            MOVE I-TRN-DTE     TO O-TRN-DTE.
            MOVE I-REF-NBR     TO O-REF-NBR.
            MOVE I-FEE-PGM-IND TO O-FEE-PGM-IND.
            PERFORM 2211-ASSIGN-PARA
               THRU 2211-ASSIGN-EXIT.
       2213-MOVE-EXIT.
            EXIT.
       2211-ASSIGN-PARA.
            EVALUATE TRUE
               WHEN I-FEE-PGM-IND = 'A00'
                   MOVE 010 TO ORIG-CURR-CDE
               WHEN I-FEE-PGM-IND = 'F10'
                   MOVE 125 TO ORIG-CURR-CDE
               WHEN I-FEE-PGM-IND = '840'
                   MOVE 445 TO ORIG-CURR-CDE
               WHEN I-FEE-PGM-IND = '947'
                   MOVE 922 TO ORIG-CURR-CDE
               WHEN OTHER
                   MOVE SPACES TO ORIG-CURR-CDE
            END-EVALUATE.
       2211-ASSIGN-EXIT.
            EXIT.
       2221-WRITE-PARA.
            WRITE OUTREC.
               EVALUATE TRUE
               WHEN WS-FS2 = 00
                   DISPLAY "WRITE OUTFILE SUCCESS"
               WHEN OTHER
                   DISPLAY "WRITE ERROR OUTFILE: " WS-FS1
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
            END-EVALUATE
       2221-WRITE-EXIT.
            EXIT.



SQL
************************************************************************
       SELECT ACCT, CUST.NAME, STATE, PHONE RANK()
           OVER (PARTITION BY STATE ORDER BY CUST.NAME ASC)
               AS STATE_RANK FROM BANK_MASTER;