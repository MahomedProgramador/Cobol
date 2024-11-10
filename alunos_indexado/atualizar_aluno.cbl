IDENTIFICATION DIVISION.
PROGRAM-ID. atualizar_aluno.
ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.
    FILE-CONTROL.
     COPY "slaluno.cbl".

DATA DIVISION.
FILE SECTION.
  COPY "fdaluno.cbl".


WORKING-STORAGE SECTION.
    77 ACT-ID-ALUNO           PIC 9(5).
    77 FILLER             PIC X VALUE SPACE.
    77 ACT-NOME-ALUNO         PIC X(30).
    77 FILLER PIC X VALUE SPACE .
    77 ACT-NOTA1             PIC 99V99.
    77 FILLER PIC X VALUE SPACES.
    77 ACT-NOTA2              PIC 99V99.
    77 FILLER PIC X VALUE SPACES.
    77 ACT-NOTA3             PIC 99V99.
    77 FILLER PIC X VALUE SPACES.

    77 ACT-MEDIAALUNO        PIC 99V99.
    77 ACT-FIM-FICHEIRO     PIC X VALUES 'N'.

PROCEDURE DIVISION.

    PERFORM UPDATE-ALUNO.
STOP RUN.


UPDATE-ALUNO.
    DISPLAY "Introduza o id do aluno que deseja alterar: " NO ADVANCING
    ACCEPT ACT-ID-ALUNO

    OPEN I-O FICHEIRO-ALUNOS.
        PERFORM UNTIL ACT-FIM-FICHEIRO = 'Y'
           READ FICHEIRO-ALUNOS
            AT END MOVE 'Y' TO ACT-FIM-FICHEIRO
            NOT AT END
                IF ID-ALUNO = ACT-ID-ALUNO
                    DISPLAY "-------------------------------"
                    DISPLAY "Vai alterar o nome de " NOME-ALUNO " indique o novo nome: " NO ADVANCING
                    ACCEPT ACT-NOME-ALUNO

                    DISPLAY "Indique a nova nota 1 de " ACT-NOME-ALUNO NO ADVANCING
                    ACCEPT ACT-NOTA1

                    DISPLAY "Indique a nova nota 2 de " ACT-NOME-ALUNO NO ADVANCING
                    ACCEPT ACT-NOTA2

                    DISPLAY "Indique a nova nota 3 de " ACT-NOME-ALUNO NO ADVANCING
                    ACCEPT ACT-NOTA3


                    MOVE ACT-NOME-ALUNO TO NOME-ALUNO
                    MOVE ACT-NOTA1 TO NOTA1
                    MOVE ACT-NOTA2 TO NOTA2
                    MOVE ACT-NOTA3 TO NOTA3

                    REWRITE REGISTO-ALUNO
               MOVE 'Y' TO ACT-FIM-FICHEIRO
                    DISPLAY "Os dados do aluno " ACT-NOME-ALUNO " foram alterados com sucesso"
                    DISPLAY "-------------------------------"
                    DISPLAY "Notas de " NOME-ALUNO
                    DISPLAY "ID: " ID-ALUNO
                    DISPLAY "Nota1: " NOTA1
                    DISPLAY "Nota2: " NOTA2
                    DISPLAY "Nota3: " NOTA3 "."
                    DISPLAY "-------------------------------"
           END-READ
        END-PERFORM
    CLOSE FICHEIRO-ALUNOS
.
