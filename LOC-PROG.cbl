      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOC-PROG.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01 DICT.
           05 DICT-ENTRY OCCURS 1000 TIMES INDEXED BY I.
               10 WORD-ENTRY PIC X(100).
               10 WORD-COUNTER PIC S999.

       77 WORD-TO-ACTIONX PIC X(100).
       77 ACTION-FLAGX PIC 9.
       77 ACTION-PTRX PIC S9999999.
       PROCEDURE DIVISION USING DICT, WORD-TO-ACTIONX, ACTION-FLAGX,
      -                         ACTION-PTRX.
       MAIN-PROCEDURE.
           DISPLAY "__________________________________________________"
           SET I TO 1.
           SEARCH DICT-ENTRY
               AT END
                   DISPLAY "NOT IN EXISTING DICTIONARY: "
      -                    WORD-TO-ACTIONX
                   DISPLAY "MERGE UNSUCCESSFUL."
               WHEN WORD-ENTRY(I) EQUAL TO WORD-TO-ACTIONX
                   DISPLAY "AT INDEX: "I", ENTRY FOUND: "WORD-ENTRY(I)
                   SET ACTION-FLAGX TO 1
                   SET ACTION-PTRX TO I
           END-SEARCH.
           DISPLAY "__________________________________________________".
           DISPLAY " "
           DISPLAY " ".
           GOBACK.
       END PROGRAM LOC-PROG.
