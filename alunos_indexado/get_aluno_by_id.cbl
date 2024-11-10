
IDENTIFICATION DIVISION.
PROGRAM-ID. get_aluno_by_id.
ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.
    FILE-CONTROL.
     COPY "slaluno.cbl".

DATA DIVISION.
FILE SECTION.
  COPY "fdaluno.cbl".


WORKING-STORAGE SECTION.
  77 GET-ID-ALUNO           PIC 9(5).
  77 FILLER             PIC X VALUE SPACE.
  77 GET-NOME-ALUNO         PIC X(30).
  77 FILLER PIC X VALUE SPACE .
  77 GET-NOTA1             PIC 99V99.
  77 FILLER PIC X VALUE SPACES.
  77 GET-NOTA2              PIC 99V99.
  77 FILLER PIC X VALUE SPACES.
  77 GET-NOTA3             PIC 99V99.
  77 FILLER PIC X VALUE SPACES.
  77 GET-MEDIAALUNO        PIC 99V99.
  77 GET-FIM-FICHEIRO     PIC X VALUES 'N'.


PROCEDURE DIVISION.


    PERFORM GET_ALUNO_BY_ID.
STOP RUN.


GET_ALUNO_BY_ID.
    DISPLAY "Introduza o id do aluno que deseja pesquisar: " NO ADVANCING
    ACCEPT GET-ID-ALUNO

    OPEN INPUT FICHEIRO-ALUNOS.
        PERFORM UNTIL GET-FIM-FICHEIRO = 'Y'
           READ FICHEIRO-ALUNOS
            AT END MOVE 'Y' TO GET-FIM-FICHEIRO
            NOT AT END
                IF ID-ALUNO = GET-ID-ALUNO
                    DISPLAY "-------------------------------"
                    DISPLAY "ID do fornecedor: " GET-ID-ALUNO
                    DISPLAY "Nome do aluno: " NOME-ALUNO
                    DISPLAY "Nota1: " NOTA1
                    DISPLAY "Nota2: " NOTA2
                    DISPLAY "Nota3: " NOTA3 "."
                    DISPLAY "-------------------------------"
                    MOVE 'Y' TO GET-FIM-FICHEIRO
           END-READ
        END-PERFORM
    CLOSE FICHEIRO-ALUNOS
.
