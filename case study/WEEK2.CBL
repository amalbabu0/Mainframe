       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEEK1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *------------------------------------------
           SELECT INFILE ASSIGN TO DD1
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS WS-FS1.
      *------------------------------------------
           SELECT OUTFILE ASSIGN TO DD2
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS O-EMP-ID
              FILE STATUS IS WS-FS2.
      *------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *------------------------------------------
       FD  INFILE.
       01  INREC.
           10 I-EMP-ID        PIC 9(6).
           10 FILLER          PIC X.
           10 I-EMP-NAME      PIC A(10).
           10 FILLER          PIC X.
           10 I-AGE           PIC 99.
           10 FILLER          PIC X.
           10 I-GENDER        PIC A.
           10 FILLER          PIC X.
           10 I-SALARY-MAY    PIC 9(10).
           10 FILLER          PIC X.
           10 I-SALARY-JUNE   PIC 9(10).
           10 FILLER          PIC X.
           10 I-SALARY-JULY   PIC 9(10).
           10 FILLER          PIC X(25).
      *------------------------------------------
       FD  OUTFILE.
       01  OUTREC.
           10 O-EMP-ID        PIC 9(6).
           10 FILLER          PIC X.
           10 O-EMP-NAME      PIC A(10).
           10 FILLER          PIC X.
           10 O-AGE           PIC 99.
           10 FILLER          PIC X.
           10 O-GENDER        PIC A.
           10 FILLER          PIC X.
           10 O-SALARY-JULY   PIC 9(10).
           10 FILLER          PIC X(67).
      *------------------------------------------
       WORKING-STORAGE SECTION.
       01  WS-FS1           PIC 99.
       01  WS-FS2           PIC 99.
       PROCEDURE DIVISION.
       0000-MAIN-PARA.
            PERFORM 1000-INIT-PARA
               THRU 1000-INIT-EXIT.
            PERFORM 2000-PFM-PARA
               THRU 2000-PFM-EXIT
            PERFORM 3000-TERM-PARA.
       1000-INIT-PARA.
            INITIALIZE WS-FS1 WS-FS2.
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
            OPEN INPUT INFILE.
            EVALUATE WS-FS1
               WHEN 00
                   DISPLAY "OPEN INFILE SUCCESS"
               WHEN 10
                   DISPLAY "END REACHED"
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
               WHEN OTHER
                   DISPLAY "INFILE OPEN ERROR " WS-FS1
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
            END-EVALUATE.
            OPEN I-O OUTFILE.
            EVALUATE WS-FS2
               WHEN 00
                   DISPLAY "OPEN OUTFILE SUCCESS"
               WHEN OTHER
                   DISPLAY "OUTFILE OPEN ERROR " WS-FS2
                   PERFORM 2300-CLOSE-PARA THRU 2300-CLOSE-EXIT
            END-EVALUATE.
       2100-OPEN-EXIT.
            EXIT.
       2200-READ-PARA.
            READ INFILE.
            EVALUATE TRUE
                WHEN WS-FS1 = 00
                     MOVE I-EMP-ID TO O-EMP-ID
                     READ OUTFILE KEY IS O-EMP-ID
                     EVALUATE WS-FS2
                         WHEN 00
                              MOVE I-SALARY-JULY TO O-SALARY-JULY
                              REWRITE OUTREC
                         WHEN OTHER
                              DISPLAY "OUTFILE READ ERROR " WS-FS2
                              CONTINUE
                     END-EVALUATE
                WHEN WS-FS1 = 10
                     CONTINUE
                WHEN OTHER
                     DISPLAY "INFILE READ ERROR " WS-FS1
                     PERFORM 2300-CLOSE-PARA THRU 2300-CLOSE-EXIT
            END-EVALUATE.
       2200-READ-EXIT.
            EXIT.
       2300-CLOSE-PARA.
            CLOSE INFILE.
            EVALUATE WS-FS1
               WHEN 00
                   DISPLAY "CLOSE INFILE SUCCESS"
               WHEN OTHER
                   DISPLAY "CLOSE INFILE ERROR " WS-FS1
            END-EVALUATE.
      *---------------------------------------------------------------
            CLOSE OUTFILE.
            EVALUATE WS-FS2
               WHEN 00
                   DISPLAY "CLOSE OUTFILE SUCCESS"
               WHEN OTHER
                   DISPLAY "CLOSE OUTFILE ERROR " WS-FS2
            END-EVALUATE.
       2300-CLOSE-EXIT.
            EXIT.
