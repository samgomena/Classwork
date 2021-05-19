      ******************************************************************
      * Author: Err'one
      * Date: 5/12/2021
      * Purpose: Exercise 6
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCISE-6.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COURSES-RELATIVE ASSIGN TO "RelativeCourses"
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS DYNAMIC
               RELATIVE KEY IS SEQ-NUMBER.
       DATA DIVISION.
       FILE SECTION.
       FD COURSES-RELATIVE.
       COPY CoursesFormat REPLACING STUDENT-RECORD
               BY RELATIVE-STUDENT-RECORD.
       WORKING-STORAGE SECTION.
       77 SEQ-NUMBER PIC 9(5) VALUE IS ZERO.
       77 FILE-STATUS PIC 9 VALUE IS 0.
           88 EOF VALUE IS 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT COURSES-FILE.
           OPEN OUTPUT COURSES-RELATIVE.
           READ COURSES-FILE AT END MOVE 1 TO FILE-STATUS.
           READ COURSES-FILE AT END MOVE 1 TO FILE-STATUS.
           PERFORM PROCESS-FILE UNTIL EOF.
           CLOSE COURSES-RELATIVE.
           CLOSE COURSES-FILE.
           STOP RUN.

       PROCESS-FILE.
           ADD 421 TO CRN OF STUDENT-RECORD GIVING SEQ-NUMBER.
           WRITE RELATIVE-STUDENT-RECORD FROM STUDENT-RECORD.
           READ COURSES-FILE AT END MOVE 1 TO FILE-STATUS.

       END PROGRAM EXERCISE-6.
