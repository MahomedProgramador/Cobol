*>****************************************************************
*> Author:
*> Date:
*> Purpose:
*> Tectonics: cobc
*>****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. get_vendor_by_id.
ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.
    FILE-CONTROL.
     COPY "slvendor.cbl".

DATA DIVISION.
FILE SECTION.
  COPY "fdvendor.cbl".


WORKING-STORAGE SECTION.
    77 GET-VENDOR-NUMBER         PIC 9(5).
    77 FIM-FICHEIRO              PIC X VALUE 'N'.

PROCEDURE DIVISION.


    PERFORM GET_VENDOR_BY_ID.
STOP RUN.


GET_VENDOR_BY_ID.
    DISPLAY "Introduza o id do fornecedor que deseja pesquisar: " NO ADVANCING
    ACCEPT GET-VENDOR-NUMBER

    OPEN INPUT VENDOR-FILE.
        PERFORM UNTIL FIM-FICHEIRO = 'Y'
           READ VENDOR-FILE
            AT END MOVE 'Y' TO FIM-FICHEIRO
            NOT AT END
                IF VENDOR-NUMBER = GET-VENDOR-NUMBER
                    DISPLAY "-------------------------------"
                    DISPLAY "ID do fornecedor: " VENDOR-NUMBER
                    DISPLAY "Nome do fornecedor: " VENDOR-NAME
                    DISPLAY "-------------------------------"
                    MOVE 'Y' TO FIM-FICHEIRO
           END-READ
        END-PERFORM
    CLOSE VENDOR-FILE
.
