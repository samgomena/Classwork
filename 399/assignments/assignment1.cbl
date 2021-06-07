      ******************************************************************
      * Author: Sam Gomena
      * Date: 4/14/2021
      * Purpose: Assignment 1
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGNMENT-1.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 STUDENT-NAME PIC X(10).
               88 STOP-RUN VALUE "STOP", "stop".
           01 ASSIGNMENT-DETAILS.
               03 CLASSNAME PIC X(5).
                   88 STOP-CALC VALUE "CALC", "calc".
                *>    88 STOP-RUN VALUE "STOP", "stop".
               03 FILLER PIC X.
               03 GRADE PIC XX.
                   88 PASSING VALUE    "A", "A-", "B+", "B", "B-", 
                       "C+", "C".
       01 NUM-CLASSES PIC 999 VALUE IS 0.
       01 TOTAL-GP PIC 99V99 VALUE 0.
       01 GPA PIC 9.99 VALUE 0.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM RUN-PROGRAM WITH TEST BEFORE UNTIL STOP-RUN.
           STOP RUN.

       RUN-PROGRAM.
           PERFORM GET-NAME.
           PERFORM GET-GRADES UNTIL STOP-CALC.
           PERFORM CALC.

       GET-NAME.
           DISPLAY "Enter a name followed by course numbers "
               "with grades (A-F), one per line.".
           DISPLAY "Enter CALC to calculate GPA or STOP to stop".
           ACCEPT STUDENT-NAME.

           IF STOP-RUN THEN
               STOP RUN.

       GET-GRADES.
           ACCEPT ASSIGNMENT-DETAILS.
           IF PASSING THEN
               ADD 1 TO NUM-CLASSES.
               EVALUATE GRADE
                   WHEN "A" ADD 4.00 TO TOTAL-GP
                   WHEN "A-" ADD 3.67 TO TOTAL-GP
                   WHEN "B+" ADD 3.33 TO TOTAL-GP
                   WHEN "B" ADD 3.00 TO TOTAL-GP
                   WHEN "B-" ADD 2.67 TO TOTAL-GP
                   WHEN "C+" ADD 2.33 TO TOTAL-GP
                   WHEN "C" ADD 2.00 TO TOTAL-GP
               END-EVALUATE.

           IF STOP-RUN THEN
               STOP RUN.
           
       CALC.
           DISPLAY "STUDENT NAME: " STUDENT-NAME.
           DIVIDE TOTAL-GP BY NUM-CLASSES GIVING GPA.
           DISPLAY "GPA: " GPA.
           DISPLAY " ".

        *>    Reset counters for additional students
           MOVE 0 TO NUM-CLASSES.
           MOVE 0 TO TOTAL-GP.
           MOVE " " TO CLASSNAME.

       END PROGRAM ASSIGNMENT-1.
