      ******************************************************************
      * Author: Sam Gomena
      * Date: 5/26/2021
      * Purpose: Assignment 4 part 2
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGNMENT-4.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT COURSES-INDEXED ASSIGN TO "IndexedCourses"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CRN OF STUDENT-RECORD-INDEXED
           ALTERNATE KEY IS DEPARTMENT-CODE
               OF STUDENT-RECORD-INDEXED WITH DUPLICATES
           ALTERNATE KEY IS INSTRUCTOR-LAST
               OF STUDENT-RECORD-INDEXED WITH DUPLICATES
           FILE STATUS IS FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD COURSES-INDEXED.
       COPY CoursesFormat REPLACING STUDENT-RECORD
               BY STUDENT-RECORD-INDEXED.
       WORKING-STORAGE SECTION.
       77 INPUT-STRING PIC X(20).
       77 INPUT-INSTRUCTOR-LAST PIC X(16).
       77 INPUT-DEPARTMENT-CODE PIC X(6).
       77 NEW-INSTRUCTOR-FIRST PIC X(16).
       77 NEW-INSTRUCTOR-LAST PIC X(16).
       77 CONFIRM-DELETE PIC X.
       77 ACTION PIC X.
       77 LIST-ACTION PIC X.
       77 FILE-STATUS PIC X(2).
           88 NO-MORE-MATCHES VALUE "46".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN I-O COURSES-INDEXED.
           DISPLAY "Enter an action (V, U, D, L [I/D]) and a CRN "
               "or search term:".
           ACCEPT INPUT-STRING.

           MOVE INPUT-STRING(1:2) TO ACTION.
           MOVE INPUT-STRING(3:7) TO CRN.

           IF ACTION EQUALS "V" OR "v" THEN
               PERFORM VIEW-CONTENTS.

           IF ACTION EQUALS "U" OR "u" THEN
               PERFORM UPDATE-CONTENTS.

           IF ACTION EQUALS "D" OR "d" THEN
               PERFORM DELETE-CONTENTS.

           IF ACTION EQUALS "L" OR "l"
               MOVE INPUT-STRING(3:4) TO LIST-ACTION

               IF LIST-ACTION EQUALS "I" OR "i"
                   MOVE INPUT-STRING(5:) TO INSTRUCTOR-LAST
                   PERFORM LIST-INSTRUCTOR
               END-IF

               IF LIST-ACTION EQUALS "D" OR "d"
                   MOVE INPUT-STRING(5:10) TO DEPARTMENT-CODE
                   PERFORM LIST-DEPARTMENT
               END-IF
           END-IF.

           CLOSE COURSES-INDEXED.
           STOP RUN.

       VIEW-CONTENTS.
           READ COURSES-INDEXED KEY IS CRN
               INVALID KEY 
                   DISPLAY "INVALID KEY: " CRN
               NOT INVALID KEY 
                   DISPLAY STUDENT-RECORD-INDEXED.
       
       UPDATE-CONTENTS.
           READ COURSES-INDEXED KEY IS CRN
               INVALID KEY 
                   DISPLAY "INVALID KEY: " CRN
               NOT INVALID KEY
                   DISPLAY STUDENT-RECORD-INDEXED.
               DISPLAY "NEW NAME INSTRUCTOR FIRST NAME: ".
               ACCEPT NEW-INSTRUCTOR-FIRST.
               DISPLAY "NEW NAME INSTRUCTOR LAST NAME: ".
               ACCEPT NEW-INSTRUCTOR-LAST.

               MOVE NEW-INSTRUCTOR-FIRST TO INSTRUCTOR-FIRST.
               MOVE NEW-INSTRUCTOR-LAST TO INSTRUCTOR-LAST.

               REWRITE STUDENT-RECORD-INDEXED.
               READ COURSES-INDEXED KEY IS CRN.
               DISPLAY STUDENT-RECORD-INDEXED.

       DELETE-CONTENTS.
           READ COURSES-INDEXED KEY IS CRN
               INVALID KEY 
                   DISPLAY "INVALID KEY: " CRN
               NOT INVALID KEY
                   DISPLAY STUDENT-RECORD-INDEXED.
               DISPLAY "ARE YOU SURE YOU WANT TO DELETE? (Y/N)".
               ACCEPT CONFIRM-DELETE.
               
               IF CONFIRM-DELETE EQUALS "Y" OR "y" THEN
                   DELETE COURSES-INDEXED 
                       INVALID KEY 
                           DISPLAY "INVALID KEY: " CRN
                       NOT INVALID KEY 
                           DISPLAY "SUCCESSFULLY DELETED ENTRY".

       LIST-INSTRUCTOR.
           READ COURSES-INDEXED KEY IS INSTRUCTOR-LAST
               INVALID KEY 
                   DISPLAY "INVALID INSTRUCTOR LAST NAME: " 
                       INSTRUCTOR-LAST
               NOT INVALID KEY 
                   DISPLAY STUDENT-RECORD-INDEXED.

           MOVE INSTRUCTOR-LAST TO INPUT-INSTRUCTOR-LAST.
           READ COURSES-INDEXED KEY IS INSTRUCTOR-LAST
               INVALID KEY 
                   DISPLAY "INVALID INSTRUCTOR LAST NAME: " 
                       INSTRUCTOR-LAST
               NOT INVALID KEY 
                   PERFORM UNTIL NO-MORE-MATCHES
                       DISPLAY STUDENT-RECORD-INDEXED
                       READ COURSES-INDEXED 
                           AT END MOVE "46" TO FILE-STATUS
                       END-READ
                       IF INPUT-INSTRUCTOR-LAST <> INSTRUCTOR-LAST
                           MOVE "46" TO FILE-STATUS
                       END-IF
                   END-PERFORM
           END-READ.

       LIST-DEPARTMENT.
           READ COURSES-INDEXED KEY IS DEPARTMENT-CODE
               INVALID KEY 
                   DISPLAY "INVALID DEPARTMENT CODE: " DEPARTMENT-CODE
               NOT INVALID KEY 
                   DISPLAY STUDENT-RECORD-INDEXED.

           MOVE DEPARTMENT-CODE TO INPUT-DEPARTMENT-CODE.
           READ COURSES-INDEXED KEY IS DEPARTMENT-CODE
               INVALID KEY 
                   DISPLAY "INVALID DEPARTMENT CODE: " DEPARTMENT-CODE
               NOT INVALID KEY 
                   PERFORM UNTIL NO-MORE-MATCHES
                       DISPLAY STUDENT-RECORD-INDEXED
                       READ COURSES-INDEXED 
                           AT END MOVE "46" TO FILE-STATUS
                       END-READ
                       IF INPUT-DEPARTMENT-CODE <> DEPARTMENT-CODE
                           MOVE "46" TO FILE-STATUS
                       END-IF
                   END-PERFORM
           END-READ.
           
       END PROGRAM ASSIGNMENT-4.
