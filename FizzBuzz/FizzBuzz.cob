       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIZZBUZZ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 I               PIC 9(3) VALUE 1.
       01 REMAINDER-3     PIC 9(3).
       01 REMAINDER-5     PIC 9(3).
       01 OUTPUT-LINE     PIC X(20).
       01 OUTPUT-LINE-LEN PIC 9(3).

       PROCEDURE DIVISION.
           PERFORM UNTIL I > 100
      *        Clear the output line
               MOVE SPACES TO OUTPUT-LINE
               MOVE 1 TO OUTPUT-LINE-LEN
               STRING I DELIMITED BY SIZE
                  " - " DELIMITED BY SIZE
                  INTO OUTPUT-LINE
                  WITH POINTER OUTPUT-LINE-LEN
      *        Find The the remainder of division by 3 and 5
               DIVIDE I BY 3 GIVING REMAINDER-3 REMAINDER REMAINDER-3
               DIVIDE I BY 5 GIVING REMAINDER-5 REMAINDER REMAINDER-5
      *        If the number is divisible by 3 print Fizz
               IF REMAINDER-3 = 0
                   STRING "Fizz" DELIMITED BY SIZE
                     INTO OUTPUT-LINE
                     WITH POINTER OUTPUT-LINE-LEN
               END-IF
      *        If the number is divisible by 5 print Buzz
               IF REMAINDER-5 = 0
                   STRING "Buzz" DELIMITED BY SIZE
                     INTO OUTPUT-LINE
                     WITH POINTER OUTPUT-LINE-LEN
               END-IF
      *        Print the output line to the console
               DISPLAY OUTPUT-LINE
               ADD 1 TO I
           END-PERFORM.

           STOP RUN.