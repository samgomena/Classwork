      ******************************************************************
      * Author(s): Charles Fox, Sam Gomena
      * Date: 4/7/2021
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAIRING-1.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           77 STARTING-CASH PIC 9(10)V99 VALUE 0.
           77 INTEREST-RATE PIC 9V99999.
           77 PERIODS PIC 99.
           77 TOTAL-INTEREST PIC $$$$,$$$.99.
           77 TOTAL-CASH PIC $$$$,$$$.99.
           77 INTEREST-CASH PIC 999V99.
           77 PERIOD-ITERATION PIC 99 VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM COLLECT-INFO.
           PERFORM COMPUTE-INTEREST PERIODS TIMES.
      *>      PERFORM DISPLAY-TOTAL.

           STOP RUN.

       COMPUTE-INTEREST.
           MULTIPLY STARTING-CASH BY INTEREST-RATE GIVING INTEREST-CASH.
           ADD INTEREST-CASH TO STARTING-CASH.
           MOVE INTEREST-CASH TO TOTAL-INTEREST.
           MOVE STARTING-CASH TO TOTAL-CASH.
           PERFORM DISPLAY-PERIOD.
           ADD 1 TO PERIOD-ITERATION.


       COLLECT-INFO.
           DISPLAY "Enter your starting cash".
           ACCEPT STARTING-CASH.

           DISPLAY "Enter the interest rate".
           ACCEPT INTEREST-RATE.

           DISPLAY "Enter the number of periods".
           ACCEPT PERIODS.

       DISPLAY-PERIOD.
           DISPLAY "Period: " PERIOD-ITERATION " interest: "
           TOTAL-INTEREST " total: " TOTAL-CASH.

       DISPLAY-TOTAL.
           DISPLAY "Total: " TOTAL-CASH.

       END PROGRAM PAIRING-1.
