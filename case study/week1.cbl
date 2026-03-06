       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEEK1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *-----------------------------------------------
           SELECT INFILE1 ASSIGN TO DD1
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS1.
      *-----------------------------------------------
           SELECT INFILE2 ASSIGN TO DD2
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS2.
      *-----------------------------------------------
           SELECT OUTFILE ASSIGN TO DD3
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS3.
      *-----------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE1.
       01  INREC1                  PIC X(80).
       FD  INFILE2.
       01  INREC2                  PIC X(80).
       FD  OUTFILE.
       01  OUTREC                  PIC X(80).
       WORKING-STORAGE SECTION.
       01  WS-FS1                  PIC XX.
       01  WS-FS2                  PIC XX.
       01  WS-FS3                  PIC XX.
       PROCEDURE DIVISION.
       0000-MAIN-PARA.
            PERFORM 1000-INIT-PARA
               THRU 1000-INIT-EXIT
            PERFORM 2000-PFM-PARA
               THRU 2000-PFM-EXIT
            PERFORM 3000-TERM-PARA.
       1000-INIT-PARA.
            INITIALIZE WS-FS1 WS-FS2 WS-FS3.
       1000-INIT-EXIT.
            EXIT.
       2000-PFM-PARA.
            PERFORM 2100-OPEN-PARA
               THRU 2100-OPEN-EXIT
            PERFORM 2200-READ-PARA
               THRU 2200-READ-EXIT
               UNTIL WS-FS1 = "10" AND WS-FS2 = "10"
            PERFORM 2300-CLOSE-PARA
               THRU 2300-CLOSE-EXIT.
       2000-PFM-EXIT.
            EXIT.
       3000-TERM-PARA.
            STOP RUN.
       2100-OPEN-PARA.
            OPEN INPUT INFILE1.
            EVALUATE WS-FS1
               WHEN "00"
                   DISPLAY "OPEN INFILE1 SUCCESS"
               WHEN "10"
                   DISPLAY "INFILE1 AT END ON OPEN"
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
               WHEN OTHER
                   DISPLAY "OPEN ERROR INFILE1: " WS-FS1
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
            END-EVALUATE
      *-----------------------------------------------
            OPEN INPUT INFILE2.
            EVALUATE WS-FS2
                WHEN "00"
                     DISPLAY "OPEN INFILE2 SUCCESS"
                WHEN "10"
                     DISPLAY "INFILE2 AT END ON OPEN"
                     PERFORM 2300-CLOSE-PARA
                        THRU 2300-CLOSE-EXIT
               WHEN OTHER
                   DISPLAY "OPEN ERROR INFILE2: " WS-FS2
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
            END-EVALUATE
      *-----------------------------------------------
            OPEN OUTPUT OUTFILE.
            EVALUATE WS-FS3
                WHEN "00"
                     DISPLAY "OPEN OUTFILE SUCCESS"
                WHEN "10"
                     DISPLAY "OUTFILE AT END ON OPEN"
                     PERFORM 2300-CLOSE-PARA
                        THRU 2300-CLOSE-EXIT
                WHEN OTHER
                     DISPLAY "OPEN ERROR OUTFILE: " WS-FS3
                     PERFORM 2300-CLOSE-PARA
                        THRU 2300-CLOSE-EXIT
            END-EVALUATE.
      *-----------------------------------------------
       2100-OPEN-EXIT.
            EXIT.
       2200-READ-PARA.
            READ INFILE1
            READ INFILE2
            EVALUATE TRUE
                WHEN WS-FS1 = "00" AND WS-FS2 = "00"
                     EVALUATE TRUE
                         WHEN INREC1 NOT = INREC2
                              MOVE INREC2 TO OUTREC
                              WRITE OUTREC
                         WHEN OTHER
                              CONTINUE
                     END-EVALUATE
                WHEN WS-FS1 = "10" AND WS-FS2 = "10"
                     CONTINUE
                WHEN OTHER
                     MOVE "INVALID" TO OUTREC
                     WRITE OUTREC
                     DISPLAY "STATUS:" WS-FS1 " " WS-FS2 " " WS-FS3
                     PERFORM 2300-CLOSE-PARA
                        THRU 2300-CLOSE-EXIT
            END-EVALUATE.
       2200-READ-EXIT.
            EXIT.
       2300-CLOSE-PARA.
            CLOSE INFILE1
            EVALUATE WS-FS1
               WHEN "00"
                   DISPLAY "CLOSE INFILE1 SUCCESS"
               WHEN OTHER
                   DISPLAY "CLOSE ERROR INFILE1: " WS-FS1
            END-EVALUATE
            CLOSE INFILE2
            EVALUATE WS-FS2
               WHEN "00"
                   DISPLAY "CLOSE INFILE2 SUCCESS"
               WHEN OTHER
                   DISPLAY "CLOSE ERROR INFILE2: " WS-FS2
            END-EVALUATE
            CLOSE OUTFILE
            EVALUATE WS-FS3
               WHEN "00"
                   DISPLAY "CLOSE OUTFILE SUCCESS"
               WHEN OTHER
                   DISPLAY "CLOSE ERROR OUTFILE: " WS-FS3
            END-EVALUATE.
       2300-CLOSE-EXIT.
            EXIT.
