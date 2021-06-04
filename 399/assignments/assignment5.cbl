      ******************************************************************
      * Author: Sam Gomena
      * Date: 05/26/2021
      * Purpose: Assignment 5
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGNMENT-5.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COURSES ASSIGN TO "COURSES.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD COURSES.
       01 COURSE-RECORD.
           03 FILLER PIC X(165).
           03 DEY PIC X(3).
           03 TYME PIC X(9).
           03 FILLER PIC X(30).
       WORKING-STORAGE SECTION.
       77 FILE-STATUS PIC 9 VALUE IS 0.
           88 EOF VALUE IS 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT COURSES.
        *>    CALL "SUB-PROGRAM".
        *>    CLOSE COURSES.
        *>    STOP RUN.

       *>  Read the header line in the file
           READ COURSES AT END MOVE 1 TO FILE-STATUS.
       *>  Read the first record before we start processing it
       *>  in PROCESS-FILE
           READ COURSES AT END MOVE 1 TO FILE-STATUS.
           PERFORM PROCESS-FILE UNTIL EOF.
           CLOSE COURSES.
           STOP RUN.

       PROCESS-FILE.
           DISPLAY COURSE-RECORD.
           READ COURSES AT END MOVE 1 TO FILE-STATUS.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUB-PROGRAM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           EXIT PROGRAM.
       END PROGRAM SUB-PROGRAM.

       END PROGRAM ASSIGNMENT-5.

