       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEEK8.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *-----------------------------------------------------------------
           SELECT WESTUS ASSIGN DD1
           ORGANIZATION SEQUENTIAL
           ACCESS MODE SEQUENTIAL
           FILE STATUS WS-FS1.
      *-----------------------------------------------------------------
           SELECT EASTUS ASSIGN DD2
           ORGANIZATION SEQUENTIAL
           ACCESS MODE SEQUENTIAL
           FILE STATUS WS-FS2.
      *-----------------------------------------------------------------
           SELECT OWEATUS ASSIGN DD3
           ORGANIZATION SEQUENTIAL
           ACCESS MODE SEQUENTIAL
           FILE STATUS WS-FS3.
      *-----------------------------------------------------------------
           SELECT OEASTUS ASSIGN DD4
           ORGANIZATION SEQUENTIAL
           ACCESS MODE SEQUENTIAL
           FILE STATUS WS-FS4.
      *-----------------------------------------------------------------
           SELECT SORTFILE ASSIGN DD5.
      *-----------------------------------------------------------------
           SELECT MERGEFILE ASSIGN DD6.
      *-----------------------------------------------------------------
       SELECT OUTFILE ASSIGN DD7
           ORGANIZATION SEQUENTIAL
           ACCESS MODE SEQUENTIAL
           FILE STATUS WS-FS7.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE DIVISION.
      *-----------------------------------------------------------------
       FD WESTUS.
       01 WESTREC.
            10 W-ACCT                PIC X(10).
            10 F                     PIC X(70).         
      *-----------------------------------------------------------------
       FD EASTUS.
       01 EASTREC.
            10 E-ACCT                PIC X(10).
            10 F                     PIC X(70).
      *-----------------------------------------------------------------
       FD OWESTUS.
       01 OWESTREC                   PIC X(80).
      *-----------------------------------------------------------------
       FD OEASTUS.
       01 OEASTREC                   PIC X(80).
      *-----------------------------------------------------------------
       SD SORTFILE.
       01 SORTREC                    PIC X(80).
      *-----------------------------------------------------------------
       SD MERGEFILE.
       01 MERGEREC.
            10 M-ACCT                PIC X(10).
            10 F                     PIC X(70).
      *-----------------------------------------------------------------
       FD OUTFILE.
       01 OUTREC                    PIC X(80).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 WS-FS1                     PIC 99.
       01 WS-FS2                     PIC 99.
       01 WS-FS3                     PIC 99.
       01 WS-FS4                     PIC 99.
       01 WS-FS7                     PIC 99.
       LINKAGE SECTION.
       01 LK-HEADER                  PIC X(20).
       PROCEDURE DIVISION USING LK-HEADER.
       0000-MAIN-PARA.
           PERFORM 1000-INIT-PARA.
           PERFORM 2000-PRFM-PARA.
           PERFORM 3000-TERM-PARA.
       1000-INIT-PARA.
            INITIALIZE WS-FS.
       2000-PRFM-PARA.
            PERFORM 2100-OPEN-PARA.
            PERFORM 2200-SORT-PARA.
            PERFORM 2300-CLOSE-PARA.
       3000-TERM-PARA.
            STOP RUN.
       2100-OPEN-PARA.
            OPEN INPUT OUTFILE.
            EVALUATE TRUE
               WHEN WS-FS7 = 00
                   DISPLAY "OPEN OUTFILE SUCCESS"
               WHEN OTHER
                   DISPLAY "OPEN ERROR OUTFILE: " WS-FS7
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
            END-EVALUATE.
            MOVE WS-HEADER TO OUTREC.
            WRITE OUTREC.
            EXIT.
       2200-SORT-PARA.
            SORT SORTFILE ON ASCENDING KEY W-ACCT
                 USING WESTUS GIVING OWESTUS.
            SORT SORTFILE ON ASCENDING KEY E-ACCT
                 USING EASTUS GIVING OEASTUS.
            PERFORM 2210-MERGE-PARA.
            EXIT.
       2300-CLOSE-PARA.
            CLOSE MERGEFILE.
            EVALUATE TRUE
               WHEN WS-FS6 = 00
                   DISPLAY "CLOSE MERGEFILE SUCCESS"
               WHEN OTHER
                   DISPLAY "CLOSE ERROR MERGEFILE: " WS-FS6
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
            END-EVALUATE.
            EXIT.
       2210-MERGE-PARA.
            MERGE MERGFILE ON ASCENDING KEY M-ACCT 
                 USING OWESTUS, OEASTUS GIVING OUTFILE.
            EXIT.