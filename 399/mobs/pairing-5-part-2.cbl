      ******************************************************************
      * Author: Charles Fox and Sam Gomena
      * Date: 5/19/2021
      * Purpose: Pairing 7
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAIRING-7.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COURSES-INDEXED ASSIGN TO "IndexedCourses"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CRN.
       DATA DIVISION.
       FILE SECTION.
       FD COURSES-INDEXED.
       COPY CoursesFormat REPLACING STUDENT-RECORD
               BY STUDENT-RECORD-INDEXED.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT COURSES-INDEXED.
           DISPLAY "Enter CRN: ".
           ACCEPT CRN.
           
           READ COURSES-INDEXED KEY IS CRN
               INVALID KEY 
                   DISPLAY "INVALID KEY: " CRN
               NOT INVALID KEY 
                   DISPLAY INSTRUCTOR-FIRST " " INSTRUCTOR-LAST.
           CLOSE COURSES-INDEXED.
           STOP RUN.

       END PROGRAM PAIRING-7.
