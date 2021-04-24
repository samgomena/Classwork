      ******************************************************************
      * Author: gomenas@pdx.edu
      * Date: 4/14/2021
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGNMENT-1.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 STUDENT-NAME PIC X(10).
           01 ASSIGNMENT-DETAILS.
               03 CLASS-NAME PIC X(5).
                   88 STOP-CALC VALUE "CALC".
                   88 STOP-RUN VALUE "STOP".
               03 FILLER PIC X.
               03 GRADE PIC XX.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM RUN-PROGRAM UNTIL STOP-RUN.
           STOP RUN.

       RUN-PROGRAM.
           PERFORM GET-NAME.
           PERFORM GET-GRADES UNTIL STOP-CALC.
           PERFORM CALC.

       GET-NAME.
           ACCEPT STUDENT-NAME.

       GET-GRADES.
           ACCEPT ASSIGNMENT-DETAILS.

       CALC.
           DISPLAY "STUDENT NAME: " STUDENT-NAME "ASSIGNMENT DETAILS: "
           CLASS-NAME " -- " GRADE.

       END PROGRAM ASSIGNMENT-1.
