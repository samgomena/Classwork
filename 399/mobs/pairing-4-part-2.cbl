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
           SELECT COURSES-RELATIVE ASSIGN TO "bin/RelativeCourses"
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
       77 ACCEPTED-CRN PIC 9(5).
       77 ACCEPTED-TERM PIC 9(4).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT COURSES-RELATIVE.
           DISPLAY "Enter CRN: ".
           ACCEPT ACCEPTED-CRN.
           DISPLAY "Enter term code: ".
           ACCEPT ACCEPTED-TERM.

           ADD ACCEPTED-TERM TO ACCEPTED-CRN GIVING SEQ-NUMBER.
           READ COURSES-RELATIVE INVALID KEY 
               MOVE "NOT FOUND" TO INSTRUCTOR-FIRST 
               MOVE SPACES TO INSTRUCTOR-LAST.
           DISPLAY INSTRUCTOR-FIRST " " INSTRUCTOR-LAST.
           CLOSE COURSES-RELATIVE.
           STOP RUN.

       END PROGRAM EXERCISE-6.