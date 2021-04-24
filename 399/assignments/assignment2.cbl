      ******************************************************************
      * Author: gomenas@pdx.edu
      * Date: 4/26/21
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BY-DEPARTMENT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CR-FILE-UNSORTED ASSIGN TO "TEST-FILE.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CR-FILE-SORTED ASSIGN TO "COURSES-SORTED.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-FILE ASSIGN TO "WORK.tmp".
       DATA DIVISION.
       FILE SECTION.
       FD  CR-FILE-UNSORTED.
       01  UNSORTED-RECORD                 PIC X(206).
       FD  CR-FILE-SORTED.
       01  CLASS-REGISTRATION-RECORD.
           03  DEPT                        PIC X(6).
           03  COURSE-PREFIX               PIC 9.
           03  FILLER                      PIC X(130).
           03  SCH                         PIC X(3).
           03  FILLER                      PIC X(50).
       SD  SORT-FILE.
       01  SORT-RECORD.
           03  DEPT-KEY                    PIC X(6).
           03  FILLER                      PIC X(200).
       WORKING-STORAGE SECTION.
       77  SCH-COMP                        PIC 9(3).
       77  COURSE-TUITION                  PIC 9(5)V99 VALUE IS ZERO.
       77  COLLEGE-TOTAL                   PIC 9(9)V99 VALUE IS ZERO.
       77  DEPARTMENT-TOTAL                PIC 9(9)V99 VALUE IS ZERO.
       77  FILE-STATUS                     PIC X(5)    VALUE IS "FULL".
           88  END-OF-FILE                 VALUE "EMPTY".
       77  LAST-DEPT                       PIC X(6).

       01  OUTPUT-LINE.
           03  ACADEMIC-UNIT               PIC X(25).
           03  FILLER                      PIC X(4).
           03  MONEY                       PIC $$$,$$$,$$9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       010-MAIN.
           PERFORM 020-INITIALIZE.
           PERFORM 030-PROCESS-FILE.
           PERFORM 040-TIDY-UP.
           STOP RUN.

       020-INITIALIZE.
           SORT SORT-FILE ON ASCENDING KEY DEPT-KEY
               USING CR-FILE-UNSORTED
               GIVING CR-FILE-SORTED.
           OPEN INPUT CR-FILE-SORTED.

       030-PROCESS-FILE.
           READ CR-FILE-SORTED, AT END MOVE "EMPTY" TO FILE-STATUS.
           MOVE DEPT TO LAST-DEPT.
           PERFORM 035-PROCESS-RECORDS UNTIL END-OF-FILE.
           PERFORM 039-FINAL-DISPLAY.

       035-PROCESS-RECORDS.
           MOVE SCH TO SCH-COMP.
           IF COURSE-PREFIX IS LESS THAN 5
               MULTIPLY SCH-COMP BY 238.85 GIVING COURSE-TUITION
           ELSE
               MULTIPLY SCH-COMP BY 496.50 GIVING COURSE-TUITION.
           DISPLAY DEPT, "   ",
               COURSE-PREFIX, "   ", SCH, "   ", COURSE-TUITION.
           ADD COURSE-TUITION TO COLLEGE-TOTAL.
           ADD COURSE-TUITION TO DEPARTMENT-TOTAL.

           READ CR-FILE-SORTED,
               AT END MOVE "EMPTY" TO FILE-STATUS
               PERFORM 038-DETAIL-DISPLAY.

           IF DEPT NOT EQUAL LAST-DEPT
               PERFORM 038-DETAIL-DISPLAY.

       038-DETAIL-DISPLAY.
           MOVE LAST-DEPT TO ACADEMIC-UNIT
           MOVE DEPARTMENT-TOTAL TO MONEY.
           DISPLAY OUTPUT-LINE.

           MOVE DEPT TO LAST-DEPT.
           MOVE ZERO TO DEPARTMENT-TOTAL.

       039-FINAL-DISPLAY.
           MOVE "COLLEGE OF ENGINEERING" TO ACADEMIC-UNIT
           MOVE COLLEGE-TOTAL TO MONEY.
           DISPLAY OUTPUT-LINE.

       040-TIDY-UP.
           CLOSE CR-FILE-SORTED.
       END PROGRAM BY-DEPARTMENT.
