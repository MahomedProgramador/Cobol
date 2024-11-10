IDENTIFICATION DIVISION.
PROGRAM-ID. atualizar_vendor.
ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.
    FILE-CONTROL.
     COPY "slvendor.cbl".

DATA DIVISION.
FILE SECTION.
  COPY "fdvendor.cbl".


WORKING-STORAGE SECTION.
    77 UPDATE-VENDOR-NUMBER             PIC 9(5).
    77 UPDATE-VENDOR-NAME               PIC X(30).
    77 UPDATE-FIM-FICHEIRO              PIC X VALUE 'N'.

PROCEDURE DIVISION.


    PERFORM UPDATE_VENDOR.
STOP RUN.


UPDATE_VENDOR.
    DISPLAY "Introduza o id do fornecedor que deseja alterar: " NO ADVANCING
    ACCEPT UPDATE-VENDOR-NUMBER



    OPEN I-O VENDOR-FILE.
        PERFORM UNTIL UPDATE-FIM-FICHEIRO = 'Y'
           READ VENDOR-FILE
            AT END MOVE 'Y' TO UPDATE-FIM-FICHEIRO
            NOT AT END
                IF VENDOR-NUMBER = UPDATE-VENDOR-NUMBER
                    DISPLAY "-------------------------------"
                    DISPLAY "Vai alterar o nome de " VENDOR-NAME " indique o novo nome: " NO ADVANCING
                    ACCEPT UPDATE-VENDOR-NAME
                    MOVE UPDATE-VENDOR-NAME TO VENDOR-NAME
                    REWRITE VENDOR-RECORD
                    DISPLAY "O fornecedor " UPDATE-VENDOR-NAME " foi alterado com sucesso"
                    MOVE 'Y' TO UPDATE-FIM-FICHEIRO
           END-READ
        END-PERFORM
    CLOSE VENDOR-FILE
.
