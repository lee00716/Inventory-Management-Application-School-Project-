       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY-REPORT.
       AUTHOR. DAVID LEE, RUTH SEBASTIAN, DEJAN ZORKIC, MADELEINE
               PELUSO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENT-FILE-IN
              ASSIGN TO "C:\Users\Ruth\workspace\program3\INVENT6.TXT"  
                 ORGANIZATION IS INDEXED
                 ACCESS MODE IS SEQUENTIAL
                 RECORD KEY IS PART-NUMBER.

           SELECT SUPPLIER-FILE
              ASSIGN TO "C:\Users\Ruth\workspace\program3\SUPPLIER1.TXT"
                 ORGANIZATION IS INDEXED
                 ACCESS IS RANDOM
                 RECORD KEY IS SUPPLY-CODE.

           SELECT  INVENT-FILE-OUT
              ASSIGN TO "C:\Users\Ruth\workspace\program3\INVENT8.TXT"  
                 ORGANIZATION IS LINE SEQUENTIAL.

           SELECT REORDER-REPORT
              ASSIGN TO "C:\Users\Ruth\workspace\program3\REORDER.TXT"  
                  ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INVENT-FILE-IN.
       COPY "C:\Users\Ruth\workspace\program3\INVENTORY-RECORD.txt".
       
       FD  SUPPLIER-FILE
           RECORD CONTAINS 25 CHARACTERS.
       01  SUPPLIER-IN-RECORD.
           05 SUPPLY-CODE     PIC X(5).
           05 SUPPLY-NAME     PIC X(20).

       FD  INVENT-FILE-OUT.
       01  INVENT-RECORD-OUT   PIC X(43).

       FD  REORDER-REPORT.
       01  REORDER-RECORD-OUT  PIC X(54).

       WORKING-STORAGE SECTION.
       01  INVENT-OUT-HEADERS.
           05  PART-NUMBER-HEADER  PIC X(11) VALUE "PART NUMBER".
           05  PART-NAME-HEADER    PIC X(9) VALUE "PART NAME".
           05  FILLER              PIC X(15) VALUE SPACES.
           05  VALUE-HEADER        PIC X(11) VALUE "VALUE".

       01  INVENT-DETAIL-OUT.
               10  PART-NUMBER-OUT   PIC 9(7) VALUE ZERO.
               10  FILLER            PIC X(4) VALUE SPACES.
               10  PART-NAME-OUT     PIC X(20).
               10  FILLER            PIC X(4) VALUE SPACES.
               10  STOCK-VALUE-OUT   PIC ZZZZZZZ9 VALUE ZERO.

       01  CALCULATION-VALUES.
           05  STOCK-VALUE-CALC    PIC 9(8) VALUE ZERO.
           05  TOTAL-VALUE-CALC    PIC 9(10) VALUE ZERO.

       01  AUDIT-TRAIL-OUT.
           05  FILLER              PIC X(5)   VALUE "VALUE ".
           05  FILLER              PIC X(2)   VALUE SPACES.
           05  INVENT-TOTAL-VALUE  PIC $$$$$$$$$9 VALUE ZERO.
           05  FILLER              PIC X(2)   VALUE SPACES.
           05  FILLER              PIC X(4)   VALUE "READ ".
           05  FILLER              PIC X(2)   VALUE SPACES.
           05  RECORDS-READ        PIC ZZZ9   .
           05  FILLER              PIC X(2)   VALUE SPACES.
           05  FILLER              PIC X(7)   VALUE "WRITTEN".
           05  RECORDS-WRITTEN     PIC ZZZ9   .

       01  REORDER-RECORD.
           05  PART-NUMBER-REORDER PIC 9(7).
           05  FILLER              PIC X(1) VALUE SPACES.
           05  PART-NAME-REORDER   PIC X(20).
           05  FILLER              PIC X(1) VALUE SPACES.
           05  QTY-ON-HAND-REORDER PIC 9(4).
           05  FILLER              PIC X(1) VALUE SPACES.
           05  SUPPLIER-NAME-REORDER   PIC X(20).

       01  FLAGS-AND-CONUNTERS.
           05  EOF-FLAG-INV   PIC X(3) VALUE "NO".
           05  EOF-FLAG-SUP   PIC X(3) VALUE "NO".
           
       01 READ-DATA            PIC 9(4) VALUE ZERO.
       01 WRITTEN-DATA         PIC 9(4) VALUE ZERO.

       PROCEDURE DIVISION.
      *the top of the tree which controls all modules
       100-PRODUCE-INVETORY-REPORT.
           PERFORM 201-INIT-INVENT-REPORT.
           PERFORM 202-PRODUCE-INVENT-RECORD
               UNTIL EOF-FLAG-INV = "YES".
           PERFORM 203-TERNINATE-INVENT-REPORT.
           STOP RUN.
           
      *initialization
       201-INIT-INVENT-REPORT.
           PERFORM 301-OPEN-INVENT-FILES.
           PERFORM 302-READ-INVENT-RECORD.
           PERFORM 399-WRITE-HEADERS.

      *mainline which produces inventory record
       202-PRODUCE-INVENT-RECORD.
           PERFORM 303-CALL-CALCULATION.
           PERFORM 305-WRITE-INVENT-DETAIL.
           IF QUANTITY < REORDER-POINT
               PERFORM 309-WRITE-INVENTORY-REORDER.
           PERFORM 302-READ-INVENT-RECORD.

      *termination
       203-TERNINATE-INVENT-REPORT.
           PERFORM 306-WRITE-AUDIT.
           PERFORM 307-CLOSE-INVENT-FILES.

      *open files
       301-OPEN-INVENT-FILES.
           OPEN INPUT  INVENT-FILE-IN.
           OPEN INPUT  SUPPLIER-FILE.
           OPEN OUTPUT INVENT-FILE-OUT.
           OPEN OUTPUT REORDER-REPORT.

      *read files
       302-READ-INVENT-RECORD.
           READ INVENT-FILE-IN 
           AT END MOVE "YES" TO EOF-FLAG-INV
            NOT AT END ADD 1 TO READ-DATA.

      *writes headers
       399-WRITE-HEADERS.
           WRITE INVENT-RECORD-OUT FROM INVENT-OUT-HEADERS.

      *calls calculation
       303-CALL-CALCULATION.
          CALL "C:\CALCULATION" USING QUANTITY UNIT-PRICE
          STOCK-VALUE-CALC TOTAL-VALUE-CALC.
           
      *writes inventory record
       305-WRITE-INVENT-DETAIL.
           MOVE PART-NUMBER TO PART-NUMBER-OUT.
           MOVE PART-NAME TO PART-NAME-OUT.
           MOVE STOCK-VALUE-CALC TO STOCK-VALUE-OUT.
           MOVE INVENT-DETAIL-OUT TO INVENT-RECORD-OUT.
           WRITE INVENT-RECORD-OUT.
           ADD 1 TO WRITTEN-DATA.

      *writes audit data
       306-WRITE-AUDIT.
           MOVE READ-DATA TO RECORDS-READ
           MOVE WRITTEN-DATA TO RECORDS-WRITTEN
           MOVE TOTAL-VALUE-CALC TO INVENT-TOTAL-VALUE.
           WRITE INVENT-RECORD-OUT FROM AUDIT-TRAIL-OUT.

      *closes files
       307-CLOSE-INVENT-FILES.
           CLOSE INVENT-FILE-IN  INVENT-FILE-OUT SUPPLIER-FILE 
           REORDER-REPORT.

      *controls moving and writing of reorder data
       309-WRITE-INVENTORY-REORDER.
       PERFORM 401-MOVE-SUPPLIER-DATA
       PERFORM 402-WRITE-REORDER-DATA.
       
      *moves reorder record
       401-MOVE-SUPPLIER-DATA.
       MOVE SUPPLIER-CODE TO SUPPLY-CODE.
       READ SUPPLIER-FILE 
           INVALID KEY
               DISPLAY "INVALID KEY"
           NOT INVALID KEY
               MOVE SUPPLY-NAME TO SUPPLIER-NAME-REORDER.
               MOVE QUANTITY TO QTY-ON-HAND-REORDER.
               MOVE PART-NAME-OUT TO PART-NAME-REORDER.
               MOVE PART-NUMBER-OUT TO PART-NUMBER-REORDER.
       
      *writes reorder record
       402-WRITE-REORDER-DATA.
       WRITE REORDER-RECORD-OUT FROM REORDER-RECORD.
