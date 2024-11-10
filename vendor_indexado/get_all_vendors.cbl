

IDENTIFICATION DIVISION.
PROGRAM-ID. get_all_vendors.
 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
   FILE-CONTROL.
   COPY "slvendor.cbl".

DATA DIVISION.
FILE SECTION.
   COPY "fdvendor.cbl".

WORKING-STORAGE SECTION.
  *> LINKAGE SECTION.

    77 INSERT-VENDOR-NUMBER      PIC 9(5).
    77 INSERT-VENDOR-NAME        PIC X(30).
    77 FIM-FICHEIRO              PIC X VALUE 'N'.


PROCEDURE DIVISION.

    PERFORM LER-TODOS-DADOS UNTIL FIM-FICHEIRO = 'Y'.

LER-TODOS-DADOS.
    OPEN INPUT VENDOR-FILE.
        PERFORM UNTIL FIM-FICHEIRO = 'Y'
            READ VENDOR-FILE
                AT END MOVE 'Y' TO FIM-FICHEIRO
                NOT AT END
                    DISPLAY "ID do fornecedor: " VENDOR-NUMBER
                    DISPLAY "Nome do fornecedor: " VENDOR-NAME
                    DISPLAY "-------------------------------"
            END-READ
         END-PERFORM
    CLOSE VENDOR-FILE
.
