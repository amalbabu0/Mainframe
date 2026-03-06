       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEEK5.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           EXEC SQL 
               INCLUDE ORDER-DETAIL
           END-EXEC. 
           EXEC SQL
               DECLARE CR1 CURSOR FOR
               SELECT * FROM ORDER_DETAIL ORDER BY ORDER_NO
           END-EXEC.
       01 WS-ITEM-NAME         PIC S9(4) COMP.
       01 WS-TOTAL-PRICE       PIC 9(6)V99.
       01 WS-FINAL-PRICE       PIC 9(6)V99.
       01 WS-COUPON            PIC X(10).
       01 WS-DISCOUNT          PIC 99.
       01 WS-CP                PIC 99.
       PROCEDURE DIVISION.
       0000-MAIN-PARA.
            PERFORM 1000-INIT-PARA
               THRU 1000-INIT-EXIT.
            PERFORM 2000-PFM-PARA
               THRU 2000-PFM-EXIT
            PERFORM 3000-TERM-PARA.
       1000-INIT-PARA.
            INITIALIZE WS-ITEM-NAME.
       1000-INIT-EXIT.
            EXIT.
       2000-PFM-PARA.
            PERFORM 2100-OPEN-PARA
               THRU 2100-OPEN-EXIT.
            PERFORM 2200-FETCH-PARA
               THRU 2200-FETCH-EXIT UNTIL SQLCODE = 100.
            PERFORM 2300-CLOSE-PARA
               THRU 2300-CLOSE-EXIT.
       2000-PFM-EXIT.
            EXIT.
       3000-TERM-PARA.
            STOP RUN.
       2100-OPEN-PARA.
           EXEC SQL 
               OPEN CR1
           END-EXEC.
            EVALUATE TRUE
               WHEN SQLCODE = 00
                   DISPLAY "OPEN CR1 SUCCESS"
               WHEN OTHER
                   DISPLAY "OPEN ERROR CR1: " SQLCODE
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
            END-EVALUATE
       2100-OPEN-EXIT.
            EXIT.
       2200-FETCH-PARA.
            EXEC SQL
               FETCH CR1 INTO :HV-ORDER_NO,
                              :HV-ITEM_CODE,
                              :HV:ITEM_NAME :WS-ITEM-NAME,
                              :HV-NO_OF_PEICES,
                              :HV-PRICE,
                              :HV-TOTAL-PRICE,
                              :HV-FINAL_PRICE,
                              :HV-FREE_HOME_DEL
            END-EXEC.
            INITIALIZE WS-TOTAL-PRICE WS-FINAL-PRICE.
            EVALUATE TRUE
               WHEN SQLCODE = 00
                   EVALUATE WS-ITEM-NAME
                       WHEN -1
                           NEXT SENTENCE
                       WHEN 0
      *-----------------------------------------------------------------
                           COMPUTE WS-TOTAL-PRICE =
                                   HV-PRICE * HV-NO_OF_PEICES
                           EXEC SQL
                               UPDATE ORDER_DETAIL 
                                  SET TOTAL_PRICE = :WS-TOTAL-PRICE
                                WHERE ORDER_NO = :HV-ORDER_NO
                           END-EXEC
                           EXEC SQL 
                               COMMIT
                           END-EXEC
                           PERFORM 2210-FINAL-PARA
                              THRU 2210-FINAL-EXIT
                           EXEC SQL
                               UPDATE ORDER_DETAIL 
                                  SET FINAL_PRICE = :WS-FINAL-PRICE
                                WHERE ORDER_NO = :HV-ORDER_NO
                           END-EXEC
                           EXEC SQL 
                               COMMIT
                           END-EXEC
                           PERFORM 2220-HOME-PARA
                              THRU 2220-HOME-EXIT.
      *-----------------------------------------------------------------
                   END-EVALUATE
               WHEN SQLCODE = 100
                   DISPLAY "NO RECORD FOUND"
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
               WHEN OTHER
                   DISPLAY "FETCH ERROR: " SQLCODE
                   PERFORM 2300-CLOSE-PARA
                      THRU 2300-CLOSE-EXIT
            END-EVALUATE
       2200-FETCH-EXIT.
            EXIT.
       2300-CLOSE-PARA.
            EXEC SQL
               CLOSE CR1
            END-EXEC.
            EVALUATE TRUE
               WHEN SQLCODE = 00
                   DISPLAY "CLOSE CR1 SUCCESS"
               WHEN OTHER
                   DISPLAY "CLOSE ERROR CR1: " SQLCODE
                   PERFORM 3000-TERM-PARA
            END-EVALUATE
       2210-FINAL-PARA.
      *-----------------------------------------------------------------
            EXEC SQL 
                SELECT DISC_PER INTO :WS-DISCOUNT
                  FROM DISCOUNT_TABLE
                 WHERE D_ITRM_CODE = :HV-ITEM_CODE
            END-EXEC.
            EVALUATE TRUE
               WHEN SQLCODE = 00
                   COMPUTE WS-FINAL-PRICE = WS-TOTAL-PRICE 
                           - (WS-TOTAL-PRICE * WS-DISCOUNT / 100)
               WHEN OTHER
                   DISPLAY "NO DISCOUNT FOUND" 
                   CONTINUE
            END-EVALUATE.
      *-----------------------------------------------------------------
            EXEC SQL 
                SELECT COUPON_CODE INTO :WS-COUPON
                  FROM COUPON_TABLE
                 WHERE C_ITRM_CODE = :HV-ITEM_CODE
            END-EXEC.
            INITIALIZE WS-CP.
            EVALUATE TRUE
               WHEN SQLCODE = 00
                   MOVE WS-COUPON(6:2) TO WS-CP.
                   COMPUTE WS-FINAL-PRICE = WS-TOTAL-PRICE 
                           - (WS-TOTAL-PRICE * WS-CP / 100)
               WHEN OTHER
                   DISPLAY "NO COUPON FOUND" 
                   CONTINUE
            END-EVALUATE.
      *-----------------------------------------------------------------
       2210-FINAL-PARA.
            EXIT.
       2220-HOME-PARA.
            EVALUATE TRUE 
               WHEN WS-FINAL-PRICE >= 10000
                   EXEC SQL
                       UPDATE ORDER_TABLE 
                          SET FREE_HOME_DEL = 'Y'
                        WHERE ORDER_NO = :HV-ORDER_NO
                   END-EXEC
                   EXEC SQL 
                       COMMIT
                   END-EXEC
               WHEN OTHER 
                   CONTINUE
            END-EVALUATE
       2220-HOME-EXIT.
            EXIT.