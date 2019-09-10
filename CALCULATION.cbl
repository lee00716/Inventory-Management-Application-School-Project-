       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATION.
       AUTHOR.     DAVID LEE.
       
       DATA DIVISION.
       LINKAGE SECTION.
           01  QUANTITY        PIC 9(4).
           01  UNIT-PRICE      PIC 9(4).
           01  STOCK-VALUE-CALC    PIC 9(8).
           01  TOTAL-VALUE-CALC    PIC 9(10).
       
       PROCEDURE DIVISION USING QUANTITY UNIT-PRICE
       STOCK-VALUE-CALC TOTAL-VALUE-CALC.
      
      *Calculates stock value and total value
       CALCULATE-VALUES.
           MULTIPLY QUANTITY BY UNIT-PRICE
               GIVING  STOCK-VALUE-CALC.
           ADD STOCK-VALUE-CALC TO TOTAL-VALUE-CALC.
           
       EXIT PROGRAM.
           
