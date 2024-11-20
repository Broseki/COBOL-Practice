       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROT13.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
      *  How much we shift the characters to the right
      *  Ex: A + 13 = N
         01 ROTATION-AMOUNT PIC 9(2) VALUE 13.
      *  We accept A-Za-z and 0-9
      *  You could of course just use ACSII math but this
      *  is more readable, and I don't really care about
      *  performance for this little toy program
         01 ACCEPTED-CHARS PIC X(63) VALUE 
              "abcdefghijklmnopqrstuvwxyz" &
              "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
              "1234567890 ".
      *  The length of the accepted characters string
         01 ACCEPTED-CHARS-LEN PIC 9(2) VALUE 63.
      *  Plaintext user input
         01 USER-INPUT PIC X(20).
      *  The length of the user input
         01 USER-INPUT-LEN PIC 9(2).
      *  Plaintext loop index
         01 USER-INPUT-IDX PIC 9(2).
      *  Ciphertext output
         01 CIPHERTEXT PIC X(20).
      *  General Loop index
         01 IDX PIC 9(2).
      *  Character found flag
         01 CHAR-FOUND PIC X VALUE 'N'.
      *  Current character being processed
         01 CHAR PIC X.
       PROCEDURE DIVISION.
      *    Get the user's input
           DISPLAY "Enter a string to encrypt: ".
           ACCEPT USER-INPUT.
      *    Find the actual length of the user input by removing trailing
      *    spaces
           PERFORM VARYING USER-INPUT-IDX FROM 20 BY -1 UNTIL
               USER-INPUT-IDX = 0
               IF USER-INPUT(USER-INPUT-IDX:1) NOT = SPACE
                   MOVE USER-INPUT-IDX TO USER-INPUT-LEN
                   EXIT PERFORM
               END-IF
           END-PERFORM.
      *    Check if each character is in ACCEPTED-CHARS
           PERFORM VARYING USER-INPUT-IDX FROM 1 BY 1 UNTIL 
              USER-INPUT-IDX > USER-INPUT-LEN
              MOVE USER-INPUT(USER-INPUT-IDX:1) TO CHAR
              PERFORM CHECK-CHAR
              PERFORM ROTATE-CHAR
              MOVE CHAR TO CIPHERTEXT(USER-INPUT-IDX:1)
           END-PERFORM.
      *    Display the encrypted string
           DISPLAY "Encypted string: " CIPHERTEXT
           STOP RUN.
      
      * CHECK-CHAR will check if the character is in ACCEPTED-CHARS
       CHECK-CHAR SECTION.
           MOVE 'N' TO CHAR-FOUND
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 
              ACCEPTED-CHARS-LEN
               IF CHAR = ACCEPTED-CHARS(IDX:1)
                   MOVE 'Y' TO CHAR-FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM
           IF CHAR-FOUND = 'N'
               DISPLAY "Error: Invalid character " CHAR
               STOP RUN
           END-IF
           EXIT.
      * ROTATE-CHAR will shift the character to the right by
      * ROTATION-AMOUNT using modular arithmetic
       ROTATE-CHAR SECTION.
      *    Find the index of the character in ACCEPTED-CHARS
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX >
               ACCEPTED-CHARS-LEN OR CHAR = ACCEPTED-CHARS(IDX:1)
               CONTINUE
           END-PERFORM.
      *    Calculate the new index using modular arithmetic
           COMPUTE IDX = FUNCTION MOD(IDX + ROTATION-AMOUNT,
               ACCEPTED-CHARS-LEN).
      *    Get the new character and return it
           MOVE ACCEPTED-CHARS (IDX:1) TO CHAR.
           EXIT.

           