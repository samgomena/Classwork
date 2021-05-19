      ******************************************************************
      * Author: Charles Fox & Sam Gomena
      * Date: 05/19/2021
      * Purpose: Pairing 7
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAIRING-7.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COURSES ASSIGN TO "COURSES.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT COURSES-INDEXED ASSIGN TO "IndexedCourses"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CRN OF STUDENT-RECORD-INDEXED.
       DATA DIVISION.
       FILE SECTION.
       FD COURSES.
       COPY CoursesFormat.
       FD COURSES-INDEXED.
       COPY CoursesFormat REPLACING STUDENT-RECORD
           BY STUDENT-RECORD-INDEXED.
       WORKING-STORAGE SECTION.
       77 FILE-STATUS PIC 9 VALUE IS 0.
           88 EOF VALUE IS 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT COURSES.
           OPEN OUTPUT COURSES-INDEXED.

           READ COURSES AT END MOVE 1 TO FILE-STATUS.
           READ COURSES AT END MOVE 1 TO FILE-STATUS.
           PERFORM PROCESS-FILE UNTIL EOF.
           CLOSE COURSES-INDEXED.
           CLOSE COURSES.
           STOP RUN.

       PROCESS-FILE.
           WRITE STUDENT-RECORD-INDEXED FROM STUDENT-RECORD.
           READ COURSES AT END MOVE 1 TO FILE-STATUS.

       END PROGRAM PAIRING-7.

