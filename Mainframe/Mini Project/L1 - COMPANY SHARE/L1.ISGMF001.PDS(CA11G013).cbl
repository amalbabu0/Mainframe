       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CA11G013.                                            00020024
       ENVIRONMENT DIVISION.                                            00030000
       INPUT-OUTPUT SECTION.                                            00040000
       FILE-CONTROL.                                                    00050000
           SELECT TI001-PS   ASSIGN       TO INPSHRPS                   00060000
                             ACCESS       IS SEQUENTIAL                 00070000
                             ORGANIZATION IS SEQUENTIAL                 00080000
                             FILE STATUS  IS WS05-FST-TI001.            00090000
           SELECT TO001-KSDS ASSIGN       TO OUTSHRKS                   00100000
                             ACCESS       IS SEQUENTIAL                 00110000
                             ORGANIZATION IS INDEXED                    00120000
                             RECORD KEY   IS TO001-SHARE-NO             00130008
                             FILE STATUS  IS WS05-FST-TO001.            00140000
           SELECT TO002-PS   ASSIGN       TO ERRSHRPS                   00150000
                             ACCESS       IS SEQUENTIAL                 00160000
                             ORGANIZATION IS SEQUENTIAL                 00170000
                             FILE STATUS  IS WS05-FST-TO002.            00180000
       DATA DIVISION.                                                   00190000
       FILE SECTION.                                                    00200000
       FD TI001-PS.                                                     00210005
       01 TI001-PS-REC.                                                 00220005
            05 TI001-SHARE-NO      PIC X(05).                           00230000
            05 FILLER              PIC X(01).                           00240000
            05 TI001-PER-HELD      PIC 9(02).                           00250000
            05 FILLER              PIC X(01).                           00260000
            05 TI001-NO-OF-SHARES  PIC 9(03).                           00270000
            05 FILLER              PIC X(01).                           00280000
            05 TI001-CORP-NAME     PIC X(05).                           00290000
            05 FILLER              PIC X(01).                           00300000
            05 TI001-CURR-PRICE    PIC 9(03).9(02).                     00310000
            05 FILLER              PIC X(55).                           00320000
       FD TO001-KSDS.                                                   00321005
       01 TO001-KSDS-REC.                                               00322005
            05 TO001-SHARE-NO      PIC X(05).                           00323000
            05 FILLER              PIC X(01).                           00324000
            05 TO001-PER-HELD      PIC 9(02).                           00325000
            05 FILLER              PIC X(01).                           00326000
            05 TO001-NO-OF-SHARES  PIC 9(03).                           00327000
            05 FILLER              PIC X(01).                           00328000
            05 TO001-CORP-NAME     PIC X(05).                           00329000
            05 FILLER              PIC X(01).                           00329100
            05 TO001-CURR-PRICE    PIC 9(03).9(02).                     00329200
            05 FILLER              PIC X(01).                           00329300
            05 TO001-CLASS         PIC X(02).                           00329400
            05 FILLER              PIC X(01).                           00329500
            05 TO001-RANGE         PIC X(01).                           00329600
            05 FILLER              PIC X(01).                           00329700
            05 TO001-OUT-SHR-PRICE PIC 9(05).9(03).                     00329800
            05 FILLER              PIC X(01).                           00329900
            05 TO001-INC-TAX       PIC 9(04).9(03).                     00330009
            05 FILLER              PIC X(31).                           00340000
       FD TO002-PS.                                                     00350005
       01 TO002-PS-REC.                                                 00360005
            05 TO002-SHARE-NO      PIC X(05).                           00370000
            05 FILLER              PIC X(01).                           00380000
            05 TO002-PER-HELD      PIC 9(02).                           00390000
            05 FILLER              PIC X(01).                           00400000
            05 TO002-NO-OF-SHARES  PIC 9(03).                           00410000
            05 FILLER              PIC X(01).                           00420000
            05 TO002-CORP-NAME     PIC X(05).                           00430000
            05 FILLER              PIC X(01).                           00440000
            05 TO002-CURR-PRICE    PIC X(06).                           00450017
            05 FILLER              PIC X(01).                           00460000
            05 TO002-ERR-FIELD     PIC X(15).                           00470000
            05 FILLER              PIC X(39).                           00480000
       WORKING-STORAGE SECTION.                                         00490000
       01 WS01-VARS.                                                    00500000
           05 WS05-FST-TI001       PIC 9(02).                           00510000
           05 WS05-FST-TO001       PIC 9(02).                           00520000
           05 WS05-FST-TO002       PIC 9(02).                           00530000
           05 WS05-TI001-COUNT     PIC 9(02).                           00530125
           05 WS05-ERR-FIELD       PIC X(15).                           00531000
           05 WS05-CURR-PRICE      PIC 9(03)V9(02).                     00539603
           05 WS05-CLASS           PIC X(02).                           00539803
           05 WS05-RANGE           PIC X(01).                           00540003
           05 WS05-OUT-SHR-PRICE   PIC 9(05)V9(03).                     00540203
           05 WS05-INC-TAX         PIC 9(04)V9(03).                     00540403
           05 WS05-NEW-PRICE       PIC 9(03)V9(02).                     00540503
           05 WS05-TABLE OCCURS 100 TIMES INDEXED BY WS-INDEX.          00540619
               10 WS10-SHARE-NO     PIC X(05).                          00540722
               10 WS10-PER-HELD     PIC 9(02).                          00540818
               10 WS10-NO-OF-SHARES PIC 9(03).                          00540918
               10 WS10-CORP-NAME    PIC X(05).                          00541018
       PROCEDURE DIVISION.                                              00542000
       0000-MAIN-PARA.                                                  00550000
           PERFORM 1000-INIT-PARA                                       00560000
           PERFORM 3000-PROC-PARA                                       00570000
              THRU 3000-PROC-PARA-EXIT                                  00580000
           PERFORM 9000-TERM-PARA                                       00590000
           .                                                            00591000
       1000-INIT-PARA.                                                  00600000
           SET WS-INDEX TO 1                                            00601025
           CONTINUE                                                     00610000
           .                                                            00620000
       9000-TERM-PARA.                                                  00630000
           STOP RUN                                                     00640000
           .                                                            00650000
       3000-PROC-PARA.                                                  00660000
           PERFORM 3100-OPEN-PARA                                       00670000
              THRU 3100-OPEN-PARA-EXIT                                  00680000
           PERFORM 3200-READ-PARA                                       00690000
              THRU 3200-READ-PARA-EXIT                                  00700000
             UNTIL WS05-FST-TI001 = 10                                  00701012
           PERFORM 3300-CLOSE-PARA                                      00710000
              THRU 3300-CLOSE-PARA-EXIT                                 00720000
           .                                                            00730000
       3000-PROC-PARA-EXIT.                                             00740000
           EXIT                                                         00750000
           .                                                            00760000
       3100-OPEN-PARA.                                                  00770000
           OPEN INPUT TI001-PS                                          00780000
           EVALUATE TRUE                                                00790000
           WHEN WS05-FST-TI001 = 0                                      00800025
               DISPLAY 'PS FILE WAS OPENED.'                            00810025
           WHEN OTHER                                                   00820025
               DISPLAY 'PS FILE WAS FAILED TO OPEN. FST : '             00830025
                       WS05-FST-TI001                                   00840025
           END-EVALUATE                                                 00850000
           OPEN OUTPUT TO001-KSDS                                       00860000
           EVALUATE TRUE                                                00870000
           WHEN WS05-FST-TO001 = 0                                      00880025
               DISPLAY 'KSDS FILE WAS OPENED.'                          00890025
           WHEN OTHER                                                   00900025
               DISPLAY 'KSDS FILE WAS FAILED TO OPEN. FST : '           00910025
                       WS05-FST-TO001                                   00920025
           END-EVALUATE                                                 00930000
           OPEN OUTPUT TO002-PS                                         00940000
           EVALUATE TRUE                                                00950000
           WHEN WS05-FST-TO002 = 0                                      00960025
               DISPLAY 'ERROR FILE WAS OPENED.'                         00970025
           WHEN OTHER                                                   00980025
               DISPLAY 'ERROR FILE WAS FAILED TO OPEN. FST : '          00990025
                       WS05-FST-TO002                                   01000025
           END-EVALUATE                                                 01010000
           .                                                            01020000
       3100-OPEN-PARA-EXIT.                                             01030000
           EXIT                                                         01040000
           .                                                            01050000
       3200-READ-PARA.                                                  01060000
           MOVE SPACES TO TI001-PS-REC TO001-KSDS-REC TO002-PS-REC      01070000
           READ TI001-PS                                                01080000
           ADD 1 TO WS05-TI001-COUNT                                    01080125
           EVALUATE TRUE                                                01081000
           WHEN WS05-FST-TI001 = 0                                      01090025
               PERFORM 3210-VALIDATION-PARA                             01100025
                  THRU 3210-VALIDATION-PARA-EXIT                        01110025
           WHEN WS05-FST-TI001 = 10                                     01120025
               DISPLAY 'ALL RECORDS WERE READ.'                         01130025
           WHEN OTHER                                                   01140025
               DISPLAY 'PS FILE WAS FAILED TO BE READ. FST : '          01150025
                       WS05-FST-TI001                                   01160025
               PERFORM 9000-TERM-PARA                                   01161025
           END-EVALUATE                                                 01170000
           .                                                            01180000
       3200-READ-PARA-EXIT.                                             01190000
           EXIT                                                         01200000
           .                                                            01210000
       3210-VALIDATION-PARA.                                            01220000
           EVALUATE TRUE                                                01230000
           WHEN TI001-PER-HELD        IS NUMERIC             AND        01240025
                TI001-NO-OF-SHARES    IS NUMERIC             AND        01250025
                TI001-CORP-NAME       IS GREATER THAN SPACES AND        01260025
                TI001-CURR-PRICE(1:3) IS NUMERIC             AND        01270025
                TI001-CURR-PRICE(4:1) = '.' AND                         01280025
                TI001-CURR-PRICE(5:2) IS NUMERIC                        01290025
                PERFORM 3211-CALCULATION-PARA                           01300025
                   THRU 3211-CALCULATION-PARA-EXIT                      01310025
           WHEN OTHER                                                   01340025
                DISPLAY 'INVALID RECORD WAS FOUND AT : '                01341025
                        WS05-TI001-COUNT                                01341125
                PERFORM 3213-ERROR-PARA                                 01350025
                   THRU 3213-ERROR-PARA-EXIT                            01360025
           END-EVALUATE                                                 01370000
           .                                                            01380000
       3210-VALIDATION-PARA-EXIT.                                       01390000
           EXIT                                                         01400000
           .                                                            01410000
       3213-ERROR-PARA.                                                 01420000
           EVALUATE TRUE                                                01430000
           WHEN TI001-PER-HELD        IS NOT NUMERIC                    01440025
               MOVE 'PER-HELD'        TO WS05-ERR-FIELD                 01450025
           WHEN TI001-NO-OF-SHARES    IS NOT NUMERIC                    01460025
               MOVE 'NO-OF-SHARES'    TO WS05-ERR-FIELD                 01470025
           WHEN TI001-CORP-NAME       = SPACES                          01480025
               MOVE 'CORP-NAME'       TO WS05-ERR-FIELD                 01490025
           WHEN TI001-CURR-PRICE(1:3) IS NOT NUMERIC      OR            01500025
                TI001-CURR-PRICE(4:1) IS NOT EQUAL TO '.' OR            01510025
                TI001-CURR-PRICE(5:2) IS NOT NUMERIC                    01520025
               MOVE 'CURR-PRICE'      TO WS05-ERR-FIELD                 01530025
           END-EVALUATE                                                 01540000
           MOVE TI001-SHARE-NO        TO TO002-SHARE-NO                 01550015
           MOVE TI001-PER-HELD        TO TO002-PER-HELD                 01560015
           MOVE TI001-NO-OF-SHARES    TO TO002-NO-OF-SHARES             01570015
           MOVE TI001-CORP-NAME       TO TO002-CORP-NAME                01580015
           MOVE TI001-CURR-PRICE(1:3) TO TO002-CURR-PRICE(1:3)          01601115
           MOVE TI001-CURR-PRICE(4:1) TO TO002-CURR-PRICE(4:1)          01601215
           MOVE TI001-CURR-PRICE(5:2) TO TO002-CURR-PRICE(5:2)          01601315
           MOVE TI001-CURR-PRICE      TO TO002-CURR-PRICE               01601415
           MOVE WS05-ERR-FIELD        TO TO002-ERR-FIELD                01601515
           WRITE TO002-PS-REC                                           01601615
           EVALUATE TRUE                                                01601725
           WHEN WS05-FST-TO002 = 0                                      01601825
               DISPLAY 'ERROR FILE HAS BEEN WRITTEN.'                   01619025
           WHEN OTHER                                                   01619125
               DISPLAY 'ERROR FILE WAS FAILED TO BE WRITTEN. FST : '    01619225
                       WS05-FST-TO002                                   01619325
           END-EVALUATE                                                 01619425
           .                                                            01619625
       3213-ERROR-PARA-EXIT.                                            01620000
           EXIT                                                         01630000
           .                                                            01640000
       3211-CALCULATION-PARA.                                           01650000
           MOVE ZEROS TO WS05-NEW-PRICE                                 01650123
                         WS10-PER-HELD     (WS-INDEX)                   01650223
                         WS10-NO-OF-SHARES (WS-INDEX)                   01650323
           MOVE TI001-SHARE-NO     TO WS10-SHARE-NO     (WS-INDEX)      01650622
           MOVE TI001-PER-HELD     TO WS10-PER-HELD     (WS-INDEX)      01651022
           MOVE TI001-NO-OF-SHARES TO WS10-NO-OF-SHARES (WS-INDEX)      01652022
           MOVE TI001-CORP-NAME    TO WS10-CORP-NAME    (WS-INDEX)      01653022
           EVALUATE TRUE                                                01660000
           WHEN WS10-PER-HELD (WS-INDEX) > 10                           01670025
               COMPUTE WS05-NEW-PRICE = WS10-NO-OF-SHARES (WS-INDEX)    01680025
                           + ( WS10-NO-OF-SHARES (WS-INDEX) ** 0.5)     01690025
               EVALUATE TRUE                                            01710013
               WHEN WS10-CORP-NAME (WS-INDEX) = 'INFY  ' AND            01720023
                    WS10-PER-HELD  (WS-INDEX) > 15                      01721023
                   MOVE 'C1' TO WS05-CLASS                              01730002
                   MOVE 'H'  TO WS05-RANGE                              01740002
               WHEN WS10-CORP-NAME (WS-INDEX) = 'ACC   ' AND            01741023
                    WS10-PER-HELD  (WS-INDEX) > 15                      01741123
                   MOVE 'C2' TO WS05-CLASS                              01742002
                   MOVE 'H'  TO WS05-RANGE                              01743002
               WHEN WS10-CORP-NAME (WS-INDEX) = 'TCS   ' AND            01744023
                    WS10-PER-HELD  (WS-INDEX) > 10                      01744123
                   MOVE 'C3' TO WS05-CLASS                              01745002
                   MOVE 'M'  TO WS05-RANGE                              01746002
               WHEN OTHER                                               01747002
                   MOVE 'C4' TO WS05-CLASS                              01748002
                   MOVE 'L'  TO WS05-RANGE                              01749002
               END-EVALUATE                                             01750013
               CALL 'CA21G013' USING WS05-OUT-SHR-PRICE WS05-NEW-PRICE  01760013
                                     WS05-INC-TAX                       01770023
                                     WS10-NO-OF-SHARES (WS-INDEX)       01771023
                                     WS10-PER-HELD     (WS-INDEX)       01780023
               PERFORM 3212-WRITE-PARA                                  01781013
                  THRU 3212-WRITE-PARA-EXIT                             01781113
           END-EVALUATE                                                 01782013
           .                                                            01790025
       3211-CALCULATION-PARA-EXIT.                                      01800003
           EXIT                                                         01810003
           .                                                            01820003
       3212-WRITE-PARA.                                                 01830003
           MOVE WS10-SHARE-NO     (WS-INDEX) TO TO001-SHARE-NO          01840022
           MOVE WS10-PER-HELD     (WS-INDEX) TO TO001-PER-HELD          01850022
           MOVE WS10-NO-OF-SHARES (WS-INDEX) TO TO001-NO-OF-SHARES      01860022
           MOVE WS10-CORP-NAME    (WS-INDEX) TO TO001-CORP-NAME         01870022
           MOVE WS05-NEW-PRICE               TO TO001-CURR-PRICE        01880022
           MOVE WS05-CLASS                   TO TO001-CLASS             01890022
           MOVE WS05-RANGE                   TO TO001-RANGE             01900022
           MOVE WS05-OUT-SHR-PRICE           TO TO001-OUT-SHR-PRICE     01910022
           MOVE WS05-INC-TAX                 TO TO001-INC-TAX           01920022
           WRITE TO001-KSDS-REC                                         01930003
           SET WS-INDEX UP BY 1                                         01940025
           EVALUATE TRUE                                                01950050
           WHEN WS05-FST-TO001 = 0                                      01950072
               DISPLAY 'KSDS FILE HAS BEEN WRITTEN.'                    01960012
           WHEN OTHER                                                   01963112
               DISPLAY 'KSDS FILE WAS FAILED TO BE WRITTEN. FST : '     01965225
                       WS05-FST-TO001                                   01969302
           END-EVALUATE                                                 01969802
           .                                                            01970025
       3212-WRITE-PARA-EXIT.                                            01975003
           EXIT                                                         01980003
           .                                                            01990003
       3300-CLOSE-PARA.                                                 02000003
           CLOSE TI001-PS TO001-KSDS TO002-PS                           02010003
           .                                                            02020003
       3300-CLOSE-PARA-EXIT.                                            02030003
           EXIT                                                         02031007
           .                                                            02040003
