      ******************************************************************
      * Author: Brian Langford & Lawrence Scroggs & Sam Gomena
      * Date: 5/26/2021
      * Purpose: Pairing 8
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAIRING-8.
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
               NOT INVALID KEY CALL "FIX-NAME" 
                   USING BY REFERENCE INSTRUCTOR-FIRST INSTRUCTOR-LAST.
           
           CLOSE COURSES-INDEXED.
           STOP RUN.
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIX-NAME.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INSTRUCTOR-FULLNAME PIC X(33).
       01 STRING-POINTER PIC 99.
       LINKAGE SECTION.
       01 INSTRUCTOR-LAST PIC X(16).
       01 INSTRUCTOR-FIRST PIC X(16).
       PROCEDURE DIVISION USING INSTRUCTOR-FIRST INSTRUCTOR-LAST.

           MOVE 1 TO STRING-POINTER.
           STRING INSTRUCTOR-FIRST DELIMITED BY SPACE " "
               INSTRUCTOR-LAST DELIMITED BY SPACE 
               INTO INSTRUCTOR-FULLNAME WITH POINTER STRING-POINTER.

           DISPLAY INSTRUCTOR-FULLNAME.
           EXIT PROGRAM.

       END PROGRAM FIX-NAME.
       END PROGRAM PAIRING-8.
