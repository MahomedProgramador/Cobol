

IDENTIFICATION DIVISION.
PROGRAM-ID. get_all_alunos.
 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
   FILE-CONTROL.
   COPY "slaluno.cbl".

DATA DIVISION.
FILE SECTION.
   COPY "fdaluno.cbl".

WORKING-STORAGE SECTION.
  *> LINKAGE SECTION.

77 INSERT-ID-ALUNO           PIC 9(5).
    77 FILLER PIC X VALUE SPACES.
    77 GET-ALL-NOME-ALUNO         PIC X(30).
    77 FILLER PIC X VALUE SPACES.
    77 GET-ALL-NOTA1             PIC 99V99.
    77 FILLER PIC X VALUE SPACES.
    77 GET-ALL-INSERIR-NOTA2              PIC 99V99.
    77 FILLER PIC X VALUE SPACES.
    77 GET-ALL-NOTA3             PIC 99V99.
    77 FILLER PIC X VALUE SPACES.
    77 GET-ALL-MEDIA-ALUNO        PIC 99V99.

  77 GET-ALL-FIM-FICHEIRO       PIC X VALUES 'N'.


PROCEDURE DIVISION.

    PERFORM LER-TODOS-DADOS UNTIL GET-ALL-FIM-FICHEIRO = 'Y'.

LER-TODOS-DADOS.
    OPEN INPUT FICHEIRO-ALUNOS.
        PERFORM UNTIL GET-ALL-FIM-FICHEIRO = 'Y'
            READ FICHEIRO-ALUNOS
                AT END MOVE 'Y' TO GET-ALL-FIM-FICHEIRO
                NOT AT END
                    DISPLAY "-------------------------------"
                    DISPLAY "Notas de " NOME-ALUNO *> nao esta a aparecer o nome
                    DISPLAY "ID: " ID-ALUNO
                    DISPLAY "Nota1: " NOTA1
                    DISPLAY "Nota2: " NOTA2
                    DISPLAY "Nota3: " NOTA3 "."
                    DISPLAY "-------------------------------"
            END-READ
         END-PERFORM
    CLOSE FICHEIRO-ALUNOS.
