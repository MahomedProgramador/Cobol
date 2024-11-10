      $set sourceformat(free)
IDENTIFICATION DIVISION.
PROGRAM-ID. main.
*> criar o environment division.
ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.
    FILE-CONTROL.
     COPY "slvendor.cbl".

DATA DIVISION.

FILE SECTION.
  COPY "fdvendor.cbl".

WORKING-STORAGE SECTION.
       01 Num1    PIC 9 VALUE 5.
       01 Num2    PIC 9 VALUE 4.
       01 Sum1     PIC 99.
PROCEDURE DIVISION.
*> Call the subroutine in the other file and display the result
CALL 'teste_linkage' USING Num1, Num2, Sum1.
DISPLAY Num1 " + " Num2 " = " Sum1.

STOP RUN.
