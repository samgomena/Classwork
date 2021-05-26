      ******************************************************************
      * Author: Sam Gomena
      * Date: 05/26/2021
      * Purpose: Assignment 4 part 1
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGNMENT-4.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COURSES ASSIGN TO "COURSES.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT COURSES-INDEXED ASSIGN TO "IndexedCourses"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CRN OF STUDENT-RECORD-INDEXED
               ALTERNATE KEY IS DEPARTMENT-CODE
                   OF STUDENT-RECORD-INDEXED WITH DUPLICATES
               ALTERNATE KEY IS INSTRUCTOR-LAST
                   OF STUDENT-RECORD-INDEXED WITH DUPLICATES.
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

       *>  Read the header line in the file
           READ COURSES AT END MOVE 1 TO FILE-STATUS.
       *>  Read the first record before we start processing it
       *>  in PROCESS-FILE
           READ COURSES AT END MOVE 1 TO FILE-STATUS.
           PERFORM PROCESS-FILE UNTIL EOF.
           CLOSE COURSES-INDEXED.
           CLOSE COURSES.
           STOP RUN.

       PROCESS-FILE.
           WRITE STUDENT-RECORD-INDEXED FROM STUDENT-RECORD.
           READ COURSES AT END MOVE 1 TO FILE-STATUS.

       END PROGRAM ASSIGNMENT-4.

