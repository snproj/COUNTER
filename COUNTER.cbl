      ******************************************************************
      * Author: SUNRISE MAINFRAMES
      * Date: 14 APRIL 1967
      * Purpose: EASY PROGRAM FOR COUNTING STUFF
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNTER.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 DICT.
           05 DICT-ENTRY OCCURS 1000 TIMES INDEXED BY I.
               10 WORD-ENTRY   PIC X(100).
               10 WORD-COUNTER PIC S999.
       77 TOTAL-WORDS      PIC 999.
       77 NEW-PTR          PIC 999.
       77 TEMP-WORD        PIC X(100).
       77 WORD-TO-DELETE   PIC X(100).
       77 WORDS-DELETED    PIC 9999.
       77 WORD-TO-AMEND    PIC X(100).
       77 AMEND-AMT        PIC S9999999.
       77 ALLOW-UNDERFLOW  PIC X(1).
       77 WORD-TO-DICTATE  PIC X(100).
       77 DICTATE-AMT      PIC S9999999.
       77 WORD-TO-REPLACE  PIC X(100).
       77 REPLACED-WITH    PIC X(100).
       77 WORD-TO-MERGE1   PIC X(100).
       77 WORD-TO-MERGE2   PIC X(100).
       77 MERGE-AMT        PIC S9999999.
       77 MERGE-FLAG1      PIC 9.
       77 MERGE-FLAG2      PIC 9.
       77 MERGE-PTR1       PIC S9999999.
       77 MERGE-PTR2       PIC S9999999.
       77 WORD-TO-SPLIT1   PIC X(100).
       77 WORD-TO-SPLIT2   PIC X(100).
       77 SPLIT-AMT        PIC S9999999.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "***************************************************"
           DISPLAY "*                                                 *"
           DISPLAY "*              SUNRISE POTATOFR/ES                *"
           DISPLAY "*             ~  TALLY ASSISTANT  ~               *"
           DISPLAY "*                 14 APRIL 1967                   *"
           DISPLAY "*                                                 *"
           DISPLAY "***************************************************"
           DISPLAY " "
           DISPLAY "THIS PROGRAM IS DESIGNED TO ASSIST MANUAL TALLYING"
           DISPLAY "OF THE NUMBER OF TIMES A WORD, PHRASE OR OTHER DATA"
           DISPLAY "UNIT OCCURS."
           DISPLAY " "
           DISPLAY "TYPE IN A NEW ENTRY IN ORDER TO HAVE IT STORED."
           DISPLAY "ENTER IT AGAIN IN ORDER TO LOG A NEW OCCURRENCE."
           DISPLAY "THE PROGRAM WILL THEN INCREASE THAT ENTRY'S TALLY"
           DISPLAY "BY ONE."
           DISPLAY " "
           DISPLAY "(CASE SENSITIVE!!!) SPECIAL WORDS FOR FUNCTIONS: "
           DISPLAY "__________________________________________________"
           DISPLAY """DISPLAY"" -- TO DISPLAY ALL ENTRIES AND THEIR"
           DISPLAY "             RESPECTIVE NUMBERS OF OCCURENCES"
           DISPLAY """AMEND"" ---- TO AMEND ENTRY TALLIES MANUALLY, BY "
           DISPLAY "             ADDING EITHER A POSITIVE OR NEGATIVE"
           DISPLAY "             NUMBER"
           DISPLAY """DICTATE"" -- TO MANUALLY DICTATE AN ENTRY TALLY"
           DISPLAY """REPLACE"" -- TO REPLACE AN ENTRY NAME WITH"
           DISPLAY "             ANOTHER"
           DISPLAY """MERGE"" ---- TO MERGE TWO ENTRIES"
           DISPLAY """SPLIT"" ---- TO SPLIT OFF A NEW ENTRY FROM"
           DISPLAY "             ANOTHER, TAKING A CHOSEN NUMBER FROM"
           DISPLAY "             THE ORIGINAL"
           DISPLAY """DELETE"" --- TO DELETE ENTRIES"
           DISPLAY """EXIT"" ----- TO EXIT PROGRAM"
           DISPLAY " "
           DISPLAY "NOTE: DELETED OR MERGED ENTRIES WILL NOT SHIFT THE"
           DISPLAY "      OTHERS' POSITIONS."
           DISPLAY "__________________________________________________"
           DISPLAY " "
           DISPLAY " "
           SET WORDS-DELETED TO 0.
           SET NEW-PTR TO 1.
           SET I TO 1.
           PERFORM FOREVER
               PERFORM ENTERING-PROCEDURE
               PERFORM DIRECTORY
           END-PERFORM.
           DISPLAY "MAIN-PROCEDURE: PRESS ANY KEY TO END PROGRAM."
           DISPLAY "IF YOU SEE THIS, THIS IS AN ERROR."
           ACCEPT OMITTED.
           STOP RUN.
       ENTERING-PROCEDURE.
           DISPLAY "__________________________________________________"
           DISPLAY "PLEASE SUBMIT AN ENTRY.".
           ACCEPT TEMP-WORD.
       DIRECTORY.
           IF TEMP-WORD EQUALS "DISPLAY" THEN
               PERFORM DISPLAY-ALL
           ELSE IF TEMP-WORD EQUALS "AMEND" THEN
               PERFORM AMEND-PROCEDURE
           ELSE IF TEMP-WORD EQUALS "DICTATE" THEN
               PERFORM DICTATE-PROCEDURE
           ELSE IF TEMP-WORD EQUALS "REPLACE" THEN
               PERFORM REPLACE-PROCEDURE
           ELSE IF TEMP-WORD EQUALS "MERGE" THEN
               PERFORM MERGE-PROCEDURE
           ELSE IF TEMP-WORD EQUALS "SPLIT" THEN
               PERFORM SPLIT-PROCEDURE
           ELSE IF TEMP-WORD EQUALS "DELETE" THEN
               PERFORM DELETE-PROCEDURE
           ELSE IF TEMP-WORD EQUALS "EXIT" THEN
               PERFORM EXIT-PROCEDURE
           ELSE PERFORM SEARCH-PROCEDURE
           END-IF.
       SEARCH-PROCEDURE.
           DISPLAY "__________________________________________________"
           SET I TO 1.
           SEARCH DICT-ENTRY
               AT END
                   PERFORM ADD-NEW-WORD
               WHEN WORD-ENTRY(I) EQUAL TO TEMP-WORD
                   DISPLAY "AT INDEX: "I", ENTRY FOUND: "WORD-ENTRY(I)
                   PERFORM INCREASE-WORD-COUNTER
           END-SEARCH.
           INITIALIZE TEMP-WORD.
           DISPLAY "__________________________________________________".
           DISPLAY " ".
           DISPLAY " ".
       ADD-NEW-WORD.
           MOVE TEMP-WORD TO WORD-ENTRY(NEW-PTR).
           MOVE 1 TO WORD-COUNTER(NEW-PTR).
           DISPLAY "NEW ENTRY!".
           DISPLAY WORD-COUNTER(NEW-PTR) " OCCURENCES OF: " WORD-ENTRY(N
      -            EW-PTR).
           ADD 1 TO NEW-PTR.
       INCREASE-WORD-COUNTER.
           ADD 1 TO WORD-COUNTER(I).
           DISPLAY WORD-COUNTER(I) " OCCURENCES OF: " WORD-ENTRY(I).
       DISPLAY-ALL.
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY "/////________________________________________\\\\\"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I EQUALS NEW-PTR
               DISPLAY "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               DISPLAY "INDEX: " I
               DISPLAY WORD-COUNTER(I) " OCCURENCES OF: " WORD-ENTRY(I)
               DISPLAY "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
           END-PERFORM.
           SUBTRACT 1 FROM NEW-PTR GIVING TOTAL-WORDS.
           SUBTRACT WORDS-DELETED FROM TOTAL-WORDS.
           DISPLAY TOTAL-WORDS " ENTRIES IN TOTAL.".
           DISPLAY "\\\\\________________________________________/////".
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY " ".
       AMEND-PROCEDURE.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "__________________________________________________"
           DISPLAY "PLEASE SELECT ENTRY TO AMEND.".
           ACCEPT WORD-TO-AMEND.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "__________________________________________________"
           DISPLAY "PLEASE SELECT AMOUNT TO AMEND BY.".
           ACCEPT AMEND-AMT.
           PERFORM AMEND-SEARCH.
       AMEND-SEARCH.
           DISPLAY "__________________________________________________"
           SET I TO 1.
           SEARCH DICT-ENTRY
               AT END
                   DISPLAY "NOT IN EXISTING DICTIONARY: " WORD-TO-AMEND
                   DISPLAY "AMEND UNSUCCESSFUL."
               WHEN WORD-ENTRY(I) EQUAL TO WORD-TO-AMEND
                   DISPLAY "AT INDEX: "I", ENTRY FOUND: "WORD-ENTRY(I)
                   PERFORM AMEND-WORD
           END-SEARCH.
           INITIALIZE TEMP-WORD.
           INITIALIZE ALLOW-UNDERFLOW.
           DISPLAY "__________________________________________________".
           DISPLAY " "
           DISPLAY " ".
       AMEND-WORD.
           ADD AMEND-AMT TO WORD-COUNTER(I).
           IF WORD-COUNTER(I) IS LESS THAN ZERO THEN
               DISPLAY "NEGATIVE TALLY NUMBER DETECTED. ALLOW"
      -        " UNDERFLOW? Y/N"
               ACCEPT ALLOW-UNDERFLOW
               IF ALLOW-UNDERFLOW EQUALS "Y" THEN
                   DISPLAY "SUCCESSFULLY AMENDED ENTRY: " WORD-TO-AMEND
                   DISPLAY "TALLY UPDATED: " WORD-COUNTER(I)
      -                    " OCCURRENCES OF: "WORD-ENTRY(I)
               ELSE IF ALLOW-UNDERFLOW EQUALS "N" THEN
                   MOVE ZERO TO WORD-COUNTER(I)
                   DISPLAY "SUCCESSFULLY AMENDED ENTRY: " WORD-TO-AMEND
                   DISPLAY "TALLY UPDATED: " WORD-COUNTER(I)
      -                    " OCCURRENCES OF: "WORD-ENTRY(I)
               ELSE
                   MOVE ZERO TO WORD-COUNTER(I)
                   DISPLAY "INPUT NOT RECOGNIZED. UNDERFLOW ASSUMED TO"
      -            " BE DISALLOWED."
                   DISPLAY "SUCCESSFULLY AMENDED ENTRY: " WORD-TO-AMEND
                   DISPLAY "TALLY UPDATED: " WORD-COUNTER(I)
      -                    " OCCURRENCES OF: "WORD-ENTRY(I)
           ELSE
               DISPLAY "SUCCESSFULLY AMENDED ENTRY: " WORD-TO-AMEND
               DISPLAY "TALLY UPDATED: " WORD-COUNTER(I)
      -                " OCCURRENCES OF: "WORD-ENTRY(I)
           INITIALIZE AMEND-AMT.
           INITIALIZE WORD-TO-AMEND.
       DICTATE-PROCEDURE.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "__________________________________________________"
           DISPLAY "PLEASE SELECT ENTRY TO DICTATE.".
           ACCEPT WORD-TO-DICTATE.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "__________________________________________________"
           DISPLAY "PLEASE SELECT AMOUNT TO DICTATE BY.".
           ACCEPT DICTATE-AMT.
           PERFORM DICTATE-SEARCH.
       DICTATE-SEARCH.
           DISPLAY "__________________________________________________"
           SET I TO 1.
           SEARCH DICT-ENTRY
               AT END
                   DISPLAY "NOT IN EXISTING DICTIONARY: "
      -                    WORD-TO-DICTATE
                   DISPLAY "DICTATE UNSUCCESSFUL."
               WHEN WORD-ENTRY(I) EQUAL TO WORD-TO-DICTATE
                   DISPLAY "AT INDEX: "I", ENTRY FOUND: "WORD-ENTRY(I)
                   PERFORM DICTATE-WORD
           END-SEARCH.
           INITIALIZE TEMP-WORD.
           INITIALIZE ALLOW-UNDERFLOW.
           DISPLAY "__________________________________________________".
           DISPLAY " "
           DISPLAY " ".
       DICTATE-WORD.
           MOVE DICTATE-AMT TO WORD-COUNTER(I).
           IF WORD-COUNTER(I) IS LESS THAN ZERO THEN
               DISPLAY "NEGATIVE TALLY NUMBER DETECTED. ALLOW"
      -        " UNDERFLOW? Y/N"
               ACCEPT ALLOW-UNDERFLOW
               IF ALLOW-UNDERFLOW EQUALS "Y" THEN
                   DISPLAY "SUCCESSFULLY AMENDED ENTRY: "
      -            WORD-TO-DICTATE
                   DISPLAY "TALLY UPDATED: " WORD-COUNTER(I)
      -                    " OCCURRENCES OF: "WORD-ENTRY(I)
               ELSE IF ALLOW-UNDERFLOW EQUALS "N" THEN
                   MOVE ZERO TO WORD-COUNTER(I)
                   DISPLAY "SUCCESSFULLY AMENDED ENTRY: "
      -                    WORD-TO-DICTATE
                   DISPLAY "TALLY UPDATED: " WORD-COUNTER(I)
      -                    " OCCURRENCES OF: "WORD-ENTRY(I)
               ELSE
                   MOVE ZERO TO WORD-COUNTER(I)
                   DISPLAY "INPUT NOT RECOGNIZED. UNDERFLOW ASSUMED TO"
      -            " BE DISALLOWED."
                   DISPLAY "SUCCESSFULLY AMENDED ENTRY: "
      -                    WORD-TO-DICTATE
                   DISPLAY "TALLY UPDATED: " WORD-COUNTER(I)
      -                    " OCCURRENCES OF: "WORD-ENTRY(I)
           INITIALIZE DICTATE-AMT.
           INITIALIZE WORD-TO-DICTATE.
       REPLACE-PROCEDURE.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "__________________________________________________"
           DISPLAY "PLEASE SELECT ENTRY TO REPLACE.".
           ACCEPT WORD-TO-REPLACE.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "__________________________________________________"
           DISPLAY "PLEASE SELECT WORD TO REPLACE WITH.".
           ACCEPT REPLACED-WITH.
           PERFORM REPLACE-SEARCH.
       REPLACE-SEARCH.
           DISPLAY "__________________________________________________"
           SET I TO 1.
           SEARCH DICT-ENTRY
               AT END
                   DISPLAY "NOT IN EXISTING DICTIONARY: "
      -                    WORD-TO-REPLACE
                   DISPLAY "REPLACE UNSUCCESSFUL."
               WHEN WORD-ENTRY(I) EQUAL TO WORD-TO-REPLACE
                   DISPLAY "AT INDEX: "I", ENTRY FOUND: "WORD-ENTRY(I)
                   PERFORM REPLACE-WORD
           END-SEARCH.
           INITIALIZE TEMP-WORD.
           DISPLAY "__________________________________________________".
           DISPLAY " "
           DISPLAY " ".
       REPLACE-WORD.
           MOVE REPLACED-WITH TO WORD-ENTRY(I).
           DISPLAY "SUCCESSFULLY REPLACED ENTRY TO: " WORD-ENTRY(I).
           INITIALIZE WORD-TO-REPLACE.
           INITIALIZE REPLACED-WITH.
       DELETE-PROCEDURE.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "__________________________________________________"
           DISPLAY "PLEASE SELECT ENTRY TO DELETE.".
           ACCEPT WORD-TO-DELETE.
           PERFORM DELETE-SEARCH.
       DELETE-SEARCH.
           DISPLAY "__________________________________________________"
           SET I TO 1.
           SEARCH DICT-ENTRY
               AT END
                   DISPLAY "NOT IN EXISTING DICTIONARY: " WORD-TO-DELETE
                   DISPLAY "DELETE UNSUCCESSFUL."
               WHEN WORD-ENTRY(I) EQUAL TO WORD-TO-DELETE
                   DISPLAY "AT INDEX: "I", ENTRY FOUND: "WORD-ENTRY(I)
                   PERFORM DELETE-WORD
           END-SEARCH.
           INITIALIZE TEMP-WORD.
           DISPLAY "__________________________________________________".
           DISPLAY " "
           DISPLAY " ".
       DELETE-WORD.
           MOVE "//////////////" TO WORD-ENTRY(I).
           INITIALIZE WORD-COUNTER(I).
           ADD 1 TO WORDS-DELETED.
           DISPLAY "SUCCESSFULLY DELETED ENTRY: " WORD-TO-DELETE.
           INITIALIZE WORD-TO-DELETE.
       MERGE-PROCEDURE.
           SET MERGE-FLAG1 TO 0.
           SET MERGE-FLAG2 TO 0.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "__________________________________________________"
           DISPLAY "PLEASE SELECT ENTRY TO MERGE FROM.".
           ACCEPT WORD-TO-MERGE1.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "__________________________________________________"
           DISPLAY "PLEASE SELECT ENTRY TO MERGE INTO.".
           ACCEPT WORD-TO-MERGE2.
           DISPLAY " ".
           DISPLAY " ".
           CALL "LOC-PROG" USING DICT, WORD-TO-MERGE1, MERGE-FLAG1,
      -                            MERGE-PTR1.
           CALL "LOC-PROG" USING DICT, WORD-TO-MERGE2, MERGE-FLAG2,
      -                            MERGE-PTR2.
           IF MERGE-FLAG1 EQUALS 1 THEN
               IF MERGE-FLAG2 EQUALS 1 THEN
                   PERFORM MERGE-WORDS
           ELSE
               DISPLAY "WORD(S) NOT FOUND. MERGE UNSUCCESSFUL."
           END-IF.
       MERGE-WORDS.
           MOVE WORD-ENTRY(MERGE-PTR1) TO WORD-ENTRY(MERGE-PTR2).
           ADD WORD-COUNTER(MERGE-PTR1) TO WORD-COUNTER(MERGE-PTR2).
           DISPLAY "SUCCESSFULLY MERGED ENTRY: " WORD-ENTRY(MERGE-PTR1)
      -            " WITH ENTRY: " WORD-ENTRY(MERGE-PTR2).
           DISPLAY "TALLY UPDATED: " WORD-COUNTER(MERGE-PTR2)
      -            " OCCURRENCES OF: " WORD-ENTRY(MERGE-PTR2)
           MOVE "//////////////" TO WORD-ENTRY(MERGE-PTR1).
           INITIALIZE WORD-COUNTER(MERGE-PTR1).
       SPLIT-PROCEDURE.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "__________________________________________________"
           DISPLAY "PLEASE SELECT ENTRY TO SPLIT FROM.".
           ACCEPT WORD-TO-SPLIT1.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "__________________________________________________"
           DISPLAY "PLEASE SELECT AMOUNT TO SPLIT OFF.".
           ACCEPT SPLIT-AMT.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "__________________________________________________"
           DISPLAY "PLEASE SELECT ENTRY TO SPLIT INTO.".
           ACCEPT WORD-TO-SPLIT2.
           PERFORM SPLIT-SEARCH.
       SPLIT-SEARCH.
           DISPLAY "__________________________________________________"
           SET I TO 1.
           SEARCH DICT-ENTRY
               AT END
                   DISPLAY "NOT IN EXISTING DICTIONARY: " WORD-TO-SPLIT1
                   DISPLAY "SPLIT UNSUCCESSFUL."
               WHEN WORD-ENTRY(I) EQUAL TO WORD-TO-SPLIT1
                   DISPLAY "AT INDEX: "I", ENTRY FOUND: "WORD-ENTRY(I)
                   PERFORM SPLIT-WORD
           END-SEARCH.
           INITIALIZE TEMP-WORD.
           DISPLAY "__________________________________________________".
           DISPLAY " "
           DISPLAY " ".
       SPLIT-WORD.
           MOVE WORD-TO-SPLIT2 TO WORD-ENTRY(NEW-PTR).
           MOVE SPLIT-AMT TO WORD-COUNTER(NEW-PTR).
           SUBTRACT SPLIT-AMT FROM WORD-COUNTER(I).
           DISPLAY "NEW ENTRY FROM SPLIT!".
           DISPLAY WORD-COUNTER(NEW-PTR) " OCCURENCES OF: " WORD-ENTRY(N
      -            EW-PTR).
           ADD 1 TO NEW-PTR.
       EXIT-PROCEDURE.
           DISPLAY " "
           DISPLAY " "
           DISPLAY "__________________________________________________"
           DISPLAY "EXIT DETECTED. PRESS ENTER TO END PROGRAM."
           ACCEPT OMITTED.
           STOP RUN.
       END PROGRAM COUNTER.
