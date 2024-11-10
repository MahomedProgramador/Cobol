*>****************************************************************
*> Author:
*> Date:
*> Purpose:
*> Tectonics: cobc
*>****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. delete_vendor.
ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            COPY "slvendor.cbl".

DATA DIVISION.
    FILE SECTION.
        COPY "fdvendor.cbl".
WORKING-STORAGE SECTION.

77 DELETE-VENDOR-NUMBER         PIC 9(5).
77 DELETE-VENDOR-NAME        PIC X(30).
77 DELETE-FIM-FICHEIRO          PIC X VALUE 'N'.

PROCEDURE DIVISION.


  PERFORM RECEBER-VENDOR
  PERFORM APAGAR-VENDOR
    STOP RUN.



RECEBER-VENDOR.
       DISPLAY " ".
       DISPLAY "Introduza o ID do fornecedor que deseja apagar: " WITH NO ADVANCING.
       ACCEPT DELETE-VENDOR-NUMBER.



APAGAR-VENDOR.
        OPEN I-O VENDOR-FILE.
        PERFORM UNTIL DELETE-FIM-FICHEIRO = 'Y'
           READ VENDOR-FILE
            AT END MOVE 'Y' TO DELETE-FIM-FICHEIRO
            NOT AT END
                IF VENDOR-NUMBER = DELETE-VENDOR-NUMBER
                    MOVE  VENDOR-NAME TO DELETE-VENDOR-NAME
                    DELETE VENDOR-FILE
                        INVALID KEY DISPLAY "Id nao existe."
                    END-DELETE

                    MOVE 'Y' TO DELETE-FIM-FICHEIRO
           END-READ
        END-PERFORM
    CLOSE VENDOR-FILE
    DISPLAY "O fornecedor " DELETE-VENDOR-NAME " foi apagado com sucesso."
.
