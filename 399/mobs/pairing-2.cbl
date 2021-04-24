      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       INPUT-OUTPUT SEcTION.
       FILE-CONTROL.
           SELECT COURSES ASSIGN TO "COURSES.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
           FD COURSES.
           01 COURSE-RECORD.
               03 SUBJEc PIC X(6).
               03 COURS.
                   05 GRAD PIC 9.
                   05 REST PIC X(4).
               03 FILLER PIC X(125).
               03 SCH PIC X(4).
               03 FILLER X(65).
       WORKING-STORAGE SECTION.
       77 FILE-STATUS PIC 99 VALUE 0.
           88 EOF VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT COURSES.
           READ COURSES AT END MOVE 1 TO FILE-STATUS.
           DISPLAY SUBJEC " " COURS
           STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
