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
           03 FILLER PIC X(164).
           03 COURSE-DAY PIC X(3).
           03 COURSE-TIME PIC X(9).
           03 FILLER PIC X(30).
       WORKING-STORAGE SECTION.
       01 MON-SCHED.
           03 MB1 PIC 99 VALUE IS 0.
           03 MB2 PIC 99 VALUE IS 0.
           03 MB3 PIC 99 VALUE IS 0.
           03 MB4 PIC 99 VALUE IS 0.
           03 MB5 PIC 99 VALUE IS 0.
           03 MB6 PIC 99 VALUE IS 0.
           03 MB7 PIC 99 VALUE IS 0.
       01 TUE-SCHED.
           03 TB1 PIC 99 VALUE IS 0.
           03 TB2 PIC 99 VALUE IS 0.
           03 TB3 PIC 99 VALUE IS 0.
           03 TB4 PIC 99 VALUE IS 0.
       01 WED-SCHED.
           03 WB1 PIC 99 VALUE IS 0.
           03 WB2 PIC 99 VALUE IS 0.
           03 WB3 PIC 99 VALUE IS 0.
           03 WB4 PIC 99 VALUE IS 0.
           03 WB5 PIC 99 VALUE IS 0.
           03 WB6 PIC 99 VALUE IS 0.
           03 WB7 PIC 99 VALUE IS 0.
       01 THU-SCHED.
           03 RB1 PIC 99 VALUE IS 0.
           03 RB2 PIC 99 VALUE IS 0.
           03 RB3 PIC 99 VALUE IS 0.
           03 RB4 PIC 99 VALUE IS 0.
       01 FRI-SCHED.
           03 FB1 PIC 99 VALUE IS 0.
           03 FB2 PIC 99 VALUE IS 0.
           03 FB3 PIC 99 VALUE IS 0.
           03 FB4 PIC 99 VALUE IS 0.
           03 FB5 PIC 99 VALUE IS 0.
           03 FB6 PIC 99 VALUE IS 0.
           03 FB7 PIC 99 VALUE IS 0.
       
       77 DAY-ONE PIC X.
       77 DAY-TWO PIC X.
       77 TIME-SLOT PIC X(7).
       77 SND-TIME-SLOT PIC X(7).
       77 GRID-TIME PIC XXX.
       77 FILE-STATUS PIC 9 VALUE IS 0.
           88 EOF VALUE IS 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT COURSES.
       *>  Read the header line in the file
           READ COURSES AT END MOVE 1 TO FILE-STATUS.
       *>  Read the first record before we start processing it
       *>  in PROCESS-FILE
           READ COURSES AT END MOVE 1 TO FILE-STATUS.
           PERFORM PROCESS-FILE UNTIL EOF.
           PERFORM DISPLAY-RESULTS.
           CLOSE COURSES.
           STOP RUN.

       PROCESS-FILE.
      *>    Ignore courses that don't have a day assigned to them 
      *>    or start after 4:00 PM (1600)
           IF COURSE-DAY IS NOT EQUAL TO " " AND COURSE-TIME(1:4) 
               IS LESS THAN 1600 THEN

               MOVE COURSE-DAY(1:1) TO DAY-ONE
               MOVE COURSE-DAY(2:2) TO DAY-TWO
            *>    DISPLAY DAY-ONE " " DAY-TWO " " COURSE-TIME
               CALL "PROCESS-DAY-AND-TIME" 
                   USING BY REFERENCE DAY-ONE COURSE-TIME TIME-SLOT 
                       SND-TIME-SLOT

            *>    IF SND-TIME-SLOT IS NOT EQUAL TO " "
            *>        DISPLAY TIME-SLOT " " SND-TIME-SLOT
            *>    ELSE
            *>        DISPLAY TIME-SLOT
            *>    END-IF

               IF DAY-TWO IS NOT EQUAL TO " " THEN
                   CALL "PROCESS-DAY-AND-TIME" 
                   USING BY REFERENCE DAY-TWO COURSE-TIME TIME-SLOT
                       SND-TIME-SLOT
               END-IF

            *>    IF SND-TIME-SLOT IS NOT EQUAL TO " "
            *>        DISPLAY TIME-SLOT " " SND-TIME-SLOT
            *>    ELSE
            *>        DISPLAY TIME-SLOT
            *>    END-IF
               
            *>    Increment the bucket count conditionally if we have
            *>     "cross bucket" classes
               PERFORM INCREMENT-BUCKETS
               IF SND-TIME-SLOT IS NOT EQUAL TO " "
                   MOVE SND-TIME-SLOT TO TIME-SLOT
                   PERFORM INCREMENT-BUCKETS
               END-IF

               MOVE " " TO TIME-SLOT
               MOVE " " TO SND-TIME-SLOT
           END-IF.
           READ COURSES AT END MOVE 1 TO FILE-STATUS.

       INCREMENT-BUCKETS.
           EVALUATE TIME-SLOT
               WHEN "MB1" ADD 1 TO MB1
               WHEN "MB2" ADD 1 TO MB2
               WHEN "MB3" ADD 1 TO MB3
               WHEN "MB4" ADD 1 TO MB4
               WHEN "MB5" ADD 1 TO MB5
               WHEN "MB6" ADD 1 TO MB6
               WHEN "MB7" ADD 1 TO MB7
       
               WHEN "TB1" ADD 1 TO TB1
               WHEN "TB2" ADD 1 TO TB2
               WHEN "TB3" ADD 1 TO TB3
               WHEN "TB4" ADD 1 TO TB4

               WHEN "WB1" ADD 1 TO WB1
               WHEN "WB2" ADD 1 TO WB2
               WHEN "WB3" ADD 1 TO WB3
               WHEN "WB4" ADD 1 TO WB4
               WHEN "WB5" ADD 1 TO WB5
               WHEN "WB6" ADD 1 TO WB6
               WHEN "WB7" ADD 1 TO WB7

               WHEN "RB1" ADD 1 TO RB1
               WHEN "RB2" ADD 1 TO RB2
               WHEN "RB3" ADD 1 TO RB3
               WHEN "RB4" ADD 1 TO RB4

               WHEN "FB1" ADD 1 TO FB1
               WHEN "FB2" ADD 1 TO FB2
               WHEN "FB3" ADD 1 TO FB3
               WHEN "FB4" ADD 1 TO FB4
               WHEN "FB5" ADD 1 TO FB5
               WHEN "FB6" ADD 1 TO FB6
               WHEN "FB7" ADD 1 TO FB7
           END-EVALUATE.

       DISPLAY-RESULTS.
           DISPLAY "MON 0745-0850: " MB1.
           DISPLAY "MON 0900-1005: " MB2.
           DISPLAY "MON 1015-1120: " MB3.
           DISPLAY "MON 1130-1235: " MB4.
           DISPLAY "MON 1245-1350: " MB5.
           DISPLAY "MON 1400-1505: " MB6.
           DISPLAY "MON 1515-1620: " MB7.

           DISPLAY " "

           DISPLAY "TUE 0800-0950: " TB1.
           DISPLAY "TUE 1000-1150: " TB2.
           DISPLAY "TUE 1200-1350: " TB3.
           DISPLAY "TUE 1400-1550: " TB4.

           DISPLAY " "

           DISPLAY "WED 0745-0850: " WB1.
           DISPLAY "WED 0900-1005: " WB2.
           DISPLAY "WED 1015-1120: " WB3.
           DISPLAY "WED 1130-1235: " WB4.
           DISPLAY "WED 1245-1350: " WB5.
           DISPLAY "WED 1400-1505: " WB6.
           DISPLAY "WED 1515-1620: " WB7.

           DISPLAY " "

           DISPLAY "THU 0800-0950: " RB1.
           DISPLAY "THU 1000-1150: " RB2.
           DISPLAY "THU 1200-1350: " RB3.
           DISPLAY "THU 1400-1550: " RB4.
           
           DISPLAY " "

           DISPLAY "FRI 0745-0850: " FB1.
           DISPLAY "FRI 0900-1005: " FB2.
           DISPLAY "FRI 1015-1120: " FB3.
           DISPLAY "FRI 1130-1235: " FB4.
           DISPLAY "FRI 1245-1350: " FB5.
           DISPLAY "FRI 1400-1505: " FB6.
           DISPLAY "FRI 1515-1620: " FB7.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-DAY-AND-TIME IS INITIAL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 START-TIME PIC 9(4).
       77 END-TIME PIC 9(4).
       LINKAGE SECTION.
       01 COURSE-DAY PIC X.
       01 COURSE-TIME PIC X(9).
       01 TIME-SLOT PIC X(7).
       01 SND-TIME-SLOT PIC X(7).
       PROCEDURE DIVISION USING COURSE-DAY COURSE-TIME 
               TIME-SLOT SND-TIME-SLOT.
           MOVE COURSE-TIME(1:4) TO START-TIME.
           MOVE COURSE-TIME(5:) TO END-TIME.
        *>    DISPLAY COURSE-DAY " " START-TIME " " END-TIME.

           EVALUATE COURSE-DAY
               WHEN 'M' PERFORM CALC-MWF-SLOT
               WHEN 'T' PERFORM CALC-TR-SLOT
               WHEN 'W' PERFORM CALC-MWF-SLOT
               WHEN 'R' PERFORM CALC-TR-SLOT
               WHEN 'F' PERFORM CALC-MWF-SLOT
           END-EVALUATE.

           EXIT PROGRAM.

       CALC-MWF-SLOT.
           IF START-TIME >= 0745 AND END-TIME <= 0850
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B1" INTO TIME-SLOT
           END-IF.
           IF START-TIME >= 0900 AND END-TIME <= 1005
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B2" INTO TIME-SLOT
           END-IF.
           IF START-TIME >= 1015 AND END-TIME <= 1120
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B3" INTO TIME-SLOT
           END-IF.
           IF START-TIME >= 1130 AND END-TIME <= 1235
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B4" INTO TIME-SLOT
           END-IF.
           IF START-TIME >= 1245 AND END-TIME <= 1350
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B5" INTO TIME-SLOT
           END-IF.
           IF START-TIME >= 1400 AND END-TIME <= 1505
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B6" INTO TIME-SLOT
           END-IF.
           IF START-TIME >= 1515 AND END-TIME <= 1620
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B7" INTO TIME-SLOT
           END-IF.

        *>    Edge cases: We assume that no classes span more than
        *>     two time slots
           IF START-TIME >= 0745 AND START-TIME < 0850 
                   AND END-TIME > 0850 AND END-TIME <= 1005
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B1" INTO TIME-SLOT
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B2" INTO SND-TIME-SLOT
           END-IF.
           IF START-TIME >= 0900 AND START-TIME < 1005 
                   AND END-TIME > 1005 AND END-TIME <= 1120
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B2" INTO TIME-SLOT
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B3" INTO SND-TIME-SLOT
           END-IF.
           IF START-TIME >= 1015 AND START-TIME < 1120 
                   AND END-TIME > 1120 AND END-TIME <= 1235
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B3" INTO TIME-SLOT
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B4" INTO SND-TIME-SLOT
           END-IF.
           IF START-TIME >= 1130 AND START-TIME < 1235 
                   AND END-TIME > 1235 AND END-TIME <= 1350
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B4" INTO TIME-SLOT
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B5" INTO SND-TIME-SLOT
           END-IF.
           IF START-TIME >= 1245 AND START-TIME < 1350 
                   AND END-TIME > 1350 AND END-TIME <= 1505
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B5" INTO TIME-SLOT
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B6" INTO SND-TIME-SLOT
           END-IF.
           IF START-TIME >= 1400 AND START-TIME < 1505 
                   AND END-TIME > 1505 AND END-TIME <= 1620
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B6" INTO TIME-SLOT
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B7" INTO SND-TIME-SLOT
           END-IF.

       CALC-TR-SLOT.
           IF START-TIME >= 0800 AND END-TIME <= 0950
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B1" INTO TIME-SLOT
           END-IF.
           IF START-TIME >= 1000 AND END-TIME <= 1150
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B2" INTO TIME-SLOT
           END-IF.
           IF START-TIME >= 1200 AND END-TIME <= 1350
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B3" INTO TIME-SLOT
           END-IF.
           IF START-TIME >= 1400 AND END-TIME <= 1550
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B4" INTO TIME-SLOT
           END-IF.
           
        *>    Edge cases: Agani, we assume that no classes span more than
        *>     two time slots
           IF START-TIME >= 0800 AND START-TIME < 1000 
                   AND END-TIME > 0950 AND END-TIME <= 1150
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B1" INTO TIME-SLOT
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B2" INTO SND-TIME-SLOT
           END-IF.
           IF START-TIME >= 1000 AND START-TIME < 1150 
                   AND END-TIME > 1150 AND END-TIME <= 1350
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B2" INTO TIME-SLOT
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B3" INTO SND-TIME-SLOT
           END-IF.
           IF START-TIME >= 1200 AND START-TIME < 1350 
                   AND END-TIME > 1350 AND END-TIME <= 1550
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B3" INTO TIME-SLOT
               STRING COURSE-DAY DELIMITED BY SPACE
                   "B4" INTO SND-TIME-SLOT
           END-IF.
       END PROGRAM PROCESS-DAY-AND-TIME.

       END PROGRAM ASSIGNMENT-5.

