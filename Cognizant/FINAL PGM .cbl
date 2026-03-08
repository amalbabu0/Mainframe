       IDENTIFICATION DIVISION.                                         00010001
       PROGRAM-ID. PGM.                                                 00020002
       ENVIRONMENT DIVISION.                                            00030002
       INPUT-OUTPUT SECTION.                                            00040002
       FILE-CONTROL.                                                    00050002
           SELECT TI001-PS ASSIGN TO INPEMPPS                           00060021
           ORGANIZATION IS SEQUENTIAL                                   00070002
           ACCESS IS SEQUENTIAL                                         00080002
           FILE STATUS IS WS-FST-TI001                                  00090002
           .                                                            00100002
           SELECT TO001-PS ASSIGN TO OUTEMPFN                           00110021
           ORGANIZATION IS SEQUENTIAL                                   00120002
           ACCESS IS SEQUENTIAL                                         00130002
           FILE STATUS IS WS-FST-TO001                                  00140002
           .                                                            00150002
           SELECT TO002-PS ASSIGN TO OUTEMPPR                           00160021
           ORGANIZATION IS SEQUENTIAL                                   00170002
           ACCESS IS SEQUENTIAL                                         00180002
           FILE STATUS IS WS-FST-TO002                                  00190002
           .                                                            00200002
           SELECT TO003-PS ASSIGN TO OUTEMPRV                           00210021
           ORGANIZATION IS SEQUENTIAL                                   00220002
           ACCESS IS SEQUENTIAL                                         00230002
           FILE STATUS IS WS-FST-TO003                                  00240002
           .                                                            00250002
       DATA DIVISION.                                                   00260002
       FILE SECTION.                                                    00270002
       FD TI001-PS                                                      00280021
           RECORDING MODE IS F                                          00281021
           RECORD CONTAINS 80 CHARACTERS.                               00282021
       01 TI001-PS-REC.                                                 00290021
         05 TI001-EMPID        PIC X(05).                               00300021
         05 F                  PIC X(01).                               00301002
         05 TI001-EMPFNAME     PIC X(10).                               00310021
         05 F                  PIC X(01).                               00311002
         05 TI001-EMPLNAME     PIC X(10).                               00320021
         05 F                  PIC X(01).                               00330002
         05 TI001-EMPRATING    PIC X(01).                               00340021
         05 F                  PIC X(01).                               00350002
         05 TI001-EMPSALARY    PIC 9(06).                               00360021
       FD TO001-PS                                                      00370021
           RECORDING MODE IS F                                          00371016
           RECORD CONTAINS 80 CHARACTERS.                               00372015
       01 TO001-PS-REC.                                                 00380033
         05 TO001-NEMPID        PIC X(06).                              00390021
         05 F                   PIC X(01).                              00391002
         05 TO001-EMPFNAME      PIC X(10).                              00400021
         05 F                   PIC X(01).                              00410002
         05 TO001-EMPLNAME      PIC X(10).                              00420021
         05 F                   PIC X(01).                              00430002
         05 TO001-EMPRATING     PIC X(01).                              00440021
         05 F                   PIC X(01).                              00450002
         05 TO001-NEMPSALARY    PIC 9(07).9(02).                        00460038
         05 F                   PIC X(01).                              00461035
         05 COLMN               PIC X(14).                              00462036
       FD TO002-PS                                                      00470021
           RECORDING MODE IS F                                          00471016
           RECORD CONTAINS 80 CHARACTERS.                               00472015
       01 TO002-PS-REC.                                                 00480033
         05 TO002-NEMPID        PIC X(06).                              00490021
         05 F                   PIC X(01).                              00500002
         05 TO002-EMPFNAME      PIC X(10).                              00510021
         05 F                   PIC X(01).                              00520002
         05 TO002-EMPLNAME      PIC X(10).                              00530021
         05 F                   PIC X(01).                              00540002
         05 TO002-EMPRATING     PIC X(01).                              00550021
         05 F                   PIC X(01).                              00560002
         05 TO002-NEMPSALARY    PIC 9(07).9(02).                        00570038
         05 F                   PIC X(01).                              00571035
         05 COLMN               PIC X(14).                              00572036
       FD TO003-PS                                                      00580021
           RECORDING MODE IS F                                          00581016
           RECORD CONTAINS 80 CHARACTERS.                               00582015
       01 TO003-PS-REC.                                                 00590033
         05 TO003-NEMPID        PIC X(06).                              00600021
         05 F                   PIC X(01).                              00610002
         05 TO003-EMPFNAME      PIC X(10).                              00620021
         05 F                   PIC X(01).                              00630002
         05 TO003-EMPLNAME      PIC X(10).                              00640021
         05 F                   PIC X(01).                              00650002
         05 TO003-EMPRATING     PIC X(01).                              00660021
         05 F                   PIC X(01).                              00670002
         05 TO003-NEMPSALARY    PIC 9(07).9(02).                        00680038
         05 F                   PIC X(01).                              00681035
         05 COLMN               PIC X(14).                              00682036
       WORKING-STORAGE SECTION.                                         00690002
       01 WS-VARS.                                                      00700002
         05 WS-FST-TI001 PIC 9(02).                                     00710002
           88 C05-TI001-SUCCESS VALUE 00.                               00711021
           88 C05-TI001-EOF     VALUE 10.                               00712021
         05 WS-FST-TO001 PIC 9(02).                                     00720002
           88 C05-TO001-SUCCESS VALUE 00.                               00720122
           88 C05-TO001-EOF     VALUE 10.                               00720222
         05 WS-FST-TO002 PIC 9(02).                                     00730002
           88 C05-TO002-SUCCESS VALUE 00.                               00731022
           88 C05-TO002-EOF     VALUE 10.                               00732022
         05 WS-FST-TO003 PIC 9(02).                                     00740006
           88 C05-TO003-SUCCESS VALUE 00.                               00741022
           88 C05-TO003-EOF     VALUE 10.                               00742022
         05 HEADER.                                                     00743033
           10 NEMPID     PIC X(10) VALUE 'NEW_EMP_ID'.                  00744033
           10 F          PIC X(01) VALUE SPACES.                        00745033
           10 EMPFNAME   PIC X(09) VALUE 'EMP_FNAME'.                   00746033
           10 F          PIC X(01) VALUE SPACES.                        00747033
           10 EMPLNAME   PIC X(09) VALUE 'EMP_LNAME'.                   00748033
           10 F          PIC X(01) VALUE SPACES.                        00749033
           10 EMPRATING  PIC X(10) VALUE 'EMP_RATING'.                  00749134
           10 F          PIC X(01) VALUE SPACES.                        00749233
           10 NEMPSALARY PIC X(14) VALUE 'NEW_EMP_SALARY'.              00749334
           10 F          PIC X(24) VALUE SPACES.                        00749433
         05 ARRAY OCCURS 5 TIMES INDEXED BY WS-INDEX.                   00749540
           10 ARR-NEMPID     PIC X(06).                                 00749640
           10 ARR-EMPFNAME   PIC X(10).                                 00749740
           10 ARR-EMPLNAME   PIC X(10).                                 00749840
           10 ARR-EMPRATING  PIC X(01).                                 00749940
           10 ARR-NEMPSALARY PIC 9(07).9(02).                           00750046
         05 WS-COUNTER PIC 9(02).                                       00750139
       PROCEDURE DIVISION.                                              00751002
       0000-MAIN-PARA.                                                  00760002
           PERFORM 1000-INIT-PARA                                       00770002
              THRU 1000-INIT-PARA-EXIT                                  00780002
           PERFORM 3000-PROC-PARA                                       00790002
              THRU 3000-PROC-PARA-EXIT                                  00800002
           PERFORM 9000-TERM-PARA                                       00810002
           .                                                            00830002
       1000-INIT-PARA.                                                  00840002
           CONTINUE                                                     00850002
           .                                                            00860002
       1000-INIT-PARA-EXIT.                                             00870002
           EXIT                                                         00880002
           .                                                            00890002
       3000-PROC-PARA.                                                  00900002
           PERFORM 3100-OPEN-PARA                                       00910002
              THRU 3100-OPEN-PARA-EXIT                                  00920002
           PERFORM 3150-HEADER-WRITE-PARA                               00921040
              THRU 3150-HEADER-WRITE-PARA-EXIT                          00922040
           PERFORM 3200-READ-PARA                                       00930002
              THRU 3200-READ-PARA-EXIT                                  00940002
             UNTIL C05-TI001-EOF                                        00950021
           PERFORM 3250-ARRAY-WRITE-PARA                                00951040
           VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5              00953040
           PERFORM 3300-CLOSE-PARA                                      00960002
              THRU 3300-CLOSE-PARA-EXIT                                 00970002
           .                                                            00980002
       3000-PROC-PARA-EXIT.                                             00990005
           EXIT                                                         01000002
           .                                                            01010002
       3100-OPEN-PARA.                                                  01020002
           OPEN INPUT TI001-PS                                          01030021
           EVALUATE TRUE                                                01040002
           WHEN C05-TI001-SUCCESS                                       01050021
             DISPLAY 'TI001-PS OPENED'                                  01060021
           WHEN OTHER                                                   01070002
             DISPLAY 'TI001-PS OPEN FAILED ' WS-FST-TI001               01080021
             PERFORM 9000-TERM-PARA                                     01090002
           END-EVALUATE                                                 01100002
      *                                                                 01100121
           OPEN OUTPUT TO001-PS                                         01101021
           EVALUATE TRUE                                                01102002
           WHEN C05-TO001-SUCCESS                                       01103021
             DISPLAY 'TO001-PS OPENED'                                  01104021
           WHEN OTHER                                                   01105002
             DISPLAY 'TO001-PS OPEN FAILED ' WS-FST-TO001               01106021
             PERFORM 9000-TERM-PARA                                     01107002
           END-EVALUATE                                                 01108002
           OPEN OUTPUT TO002-PS                                         01109021
           EVALUATE TRUE                                                01109102
           WHEN C05-TO002-SUCCESS                                       01109221
             DISPLAY 'TO002-PS OPENED'                                  01109321
           WHEN OTHER                                                   01109402
             DISPLAY 'TO002-PS OPEN FAILED ' WS-FST-TO002               01109521
             PERFORM 9000-TERM-PARA                                     01109602
           END-EVALUATE                                                 01109702
           OPEN OUTPUT TO003-PS                                         01109821
           EVALUATE TRUE                                                01109902
           WHEN C05-TO003-SUCCESS                                       01110021
             DISPLAY 'TO003-PS OPENED'                                  01120021
           WHEN OTHER                                                   01130002
             DISPLAY 'TO003-PS OPEN FAILED ' WS-FST-TO003               01140021
             PERFORM 9000-TERM-PARA                                     01150002
           END-EVALUATE                                                 01160002
           .                                                            01161004
       3100-OPEN-PARA-EXIT.                                             01170002
           EXIT                                                         01180002
           .                                                            01190002
       3300-CLOSE-PARA.                                                 01200002
           CLOSE TI001-PS TO001-PS TO002-PS TO003-PS                    01210021
           .                                                            01220002
       3300-CLOSE-PARA-EXIT.                                            01230002
           EXIT                                                         01240002
           .                                                            01250002
       3200-READ-PARA.                                                  01260002
           MOVE SPACES TO TI001-PS-REC TO001-PS-REC                     01270021
           TO002-PS-REC TO003-PS-REC                                    01280021
           READ TI001-PS                                                01281021
           EVALUATE TRUE                                                01290002
           WHEN C05-TI001-SUCCESS                                       01300021
             ADD 1 TO WS-COUNTER                                        01301038
             PERFORM 3210-VALID-PARA                                    01310002
                THRU 3210-VALID-PARA-EXIT                               01311002
           WHEN C05-TI001-EOF                                           01320021
             IF WS-COUNTER = 00                                         01321038
               DISPLAY 'EMPTY INPUT FILE'                               01322038
             ELSE                                                       01323038
               DISPLAY 'ALL RECORDS PROCESSED'                          01330038
               DISPLAY 'TOTAL RECORDS: ' WS-COUNTER                     01330147
             END-IF                                                     01331038
           WHEN OTHER                                                   01340002
             DISPLAY 'TI001-PS READ FAILED ' WS-FST-TI001               01350021
           END-EVALUATE                                                 01360007
           .                                                            01370002
       3200-READ-PARA-EXIT.                                             01380002
           EXIT                                                         01390002
           .                                                            01400002
       3210-VALID-PARA.                                                 01410002
           EVALUATE TRUE                                                01420002
           WHEN TI001-EMPID IS GREATER THAN SPACES AND                  01430021
                TI001-EMPFNAME IS ALPHABETIC       AND                  01440021
                TI001-EMPLNAME IS ALPHABETIC       AND                  01450021
               (TI001-EMPRATING IS EQUAL TO 'P' OR                      01460021
                TI001-EMPRATING IS EQUAL TO 'R')   AND                  01470021
                TI001-EMPSALARY IS NUMERIC                              01480021
             PERFORM 3220-DAPR-PARA                                     01490040
                THRU 3220-DAPR-PARA-EXIT                                01500040
           WHEN OTHER                                                   01510024
             DISPLAY TI001-EMPID ' IS INVALID'                          01520022
           END-EVALUATE                                                 01530002
           .                                                            01540003
       3210-VALID-PARA-EXIT.                                            01550003
           EXIT                                                         01560003
           .                                                            01570003
       3220-DAPR-PARA.                                                  01571040
           MOVE 'C' TO ARR-NEMPID(WS-COUNTER)(1:1)                      01571141
           MOVE TI001-EMPID TO ARR-NEMPID(WS-COUNTER)(2:5)              01571242
           MOVE TI001-EMPFNAME TO ARR-EMPFNAME(WS-COUNTER)              01571341
           MOVE TI001-EMPLNAME TO ARR-EMPLNAME(WS-COUNTER)              01571441
           MOVE TI001-EMPRATING TO ARR-EMPRATING(WS-COUNTER)            01571541
           EVALUATE TRUE                                                01572024
           WHEN TI001-EMPRATING = 'P'                                   01573024
             COMPUTE ARR-NEMPSALARY(WS-COUNTER) = 1.4 * TI001-EMPSALARY 01574040
           WHEN TI001-EMPRATING = 'R'                                   01580424
             COMPUTE ARR-NEMPSALARY(WS-COUNTER) = 1.3 * TI001-EMPSALARY 01580542
           END-EVALUATE                                                 01582441
           .                                                            01582541
       3220-DAPR-PARA-EXIT.                                             01582640
           EXIT                                                         01582725
           .                                                            01582825
       3150-HEADER-WRITE-PARA.                                          01587140
           MOVE HEADER TO TO001-PS-REC                                  01587233
           WRITE TO001-PS-REC                                           01587333
           MOVE HEADER TO TO002-PS-REC                                  01587433
           WRITE TO002-PS-REC                                           01587533
           MOVE HEADER TO TO003-PS-REC                                  01587633
           WRITE TO003-PS-REC                                           01587733
           .                                                            01587833
       3150-HEADER-WRITE-PARA-EXIT.                                     01587940
           EXIT                                                         01588033
           .                                                            01588133
       3250-ARRAY-WRITE-PARA.                                           01588241
           EVALUATE TRUE                                                01588341
           WHEN ARRAY(WS-INDEX)(27:1) = 'P'                             01588441
             PERFORM 3260-PWRITE-PARA                                   01588541
                THRU 3260-PWRITE-PARA-EXIT                              01588641
           WHEN ARRAY(WS-INDEX)(27:1) = 'R'                             01588741
             PERFORM 3270-RWRITE-PARA                                   01588841
                THRU 3270-RWRITE-PARA-EXIT                              01588941
           END-EVALUATE                                                 01589041
           .                                                            01589141
       3260-PWRITE-PARA.                                                01589243
           MOVE ARR-NEMPID(WS-INDEX) TO TO001-NEMPID TO002-NEMPID       01589343
           MOVE ARR-EMPFNAME(WS-INDEX) TO TO001-EMPFNAME TO002-EMPFNAME 01589443
           MOVE ARR-EMPLNAME(WS-INDEX) TO TO001-EMPLNAME                01589543
                                          TO002-EMPLNAME                01589643
           MOVE ARR-EMPRATING(WS-INDEX) TO TO001-EMPRATING              01589743
                                           TO002-EMPRATING              01589843
           MOVE ARR-NEMPSALARY(WS-INDEX) TO TO001-NEMPSALARY            01589943
                                            TO002-NEMPSALARY            01590043
      *                                                                 01590143
           WRITE TO001-PS-REC                                           01590243
           WRITE TO002-PS-REC                                           01590343
           .                                                            01590443
       3260-PWRITE-PARA-EXIT.                                           01590543
           EXIT                                                         01590643
           .                                                            01590743
       3270-RWRITE-PARA.                                                01590843
           MOVE ARR-NEMPID(WS-INDEX) TO TO001-NEMPID TO003-NEMPID       01590943
           MOVE ARR-EMPFNAME(WS-INDEX) TO TO001-EMPFNAME TO003-EMPFNAME 01591043
           MOVE ARR-EMPLNAME(WS-INDEX) TO TO001-EMPLNAME                01591143
                                          TO003-EMPLNAME                01591243
           MOVE ARR-EMPRATING(WS-INDEX) TO TO001-EMPRATING              01591343
                                           TO003-EMPRATING              01591443
           MOVE ARR-NEMPSALARY(WS-INDEX) TO TO001-NEMPSALARY            01591543
                                            TO003-NEMPSALARY            01591643
      *                                                                 01591743
           WRITE TO001-PS-REC                                           01591843
           WRITE TO003-PS-REC                                           01591943
           .                                                            01592043
       3270-RWRITE-PARA-EXIT.                                           01592143
           EXIT                                                         01592243
           .                                                            01592343
       9000-TERM-PARA.                                                  01592703
           STOP RUN                                                     01593003
           .                                                            01600003
