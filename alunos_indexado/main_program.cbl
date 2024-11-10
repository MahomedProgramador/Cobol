IDENTIFICATION DIVISION.
PROGRAM-ID. main_program.
 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
   FILE-CONTROL.
   COPY "slaluno.cbl".

DATA DIVISION.
FILE SECTION.
   COPY "fdaluno.cbl".

WORKING-STORAGE SECTION.
    77 OPCAO-MENU              PIC 9.
    77 FIM-PROGRAMA            PIC 9.
    77 SAIDA-INSERIR           PIC 9.
    77 SAIDA-ATUALIZAR         PIC 9.
    77 CONFIRMACAO-ATUALIZAR   PIC 9.
    77 ID-VALIDO               PIC X VALUE 'N'.
    77 ID-MEDIA                PIC 9.
    77 MEDIA-ALUNO             PIC 99V99.



    77 MAIN-ID-ALUNO           PIC 9(5).
    77 FILLER PIC X VALUE SPACES.
    77 MAIN-NOME-ALUNO         PIC X(30).
    77 FILLER PIC X VALUE SPACES.
    77 MAIN-NOTA1             PIC 99V99.
    77 FILLER PIC X VALUE SPACES.
    77 MAIN-NOTA2              PIC 99V99.
    77 FILLER PIC X VALUE SPACES.
    77 MAIN-NOTA3             PIC 99V99.
    77 FILLER PIC X VALUE SPACES.

  77 MAIN-FIM-FICHEIRO       PIC X VALUES 'N'.

PROCEDURE DIVISION.
    PERFORM WITH TEST AFTER UNTIL FIM-PROGRAMA EQUAL 1

        PERFORM APRESENTACAO
            EVALUATE OPCAO-MENU
               WHEN 0
                    PERFORM DESPEDIDA
               WHEN 1
                    PERFORM LISTAR-ALUNOS
               WHEN 2
                    PERFORM GET_ALUNO_BY_ID
               WHEN 3
                    PERFORM GET_ALUNO_BY_NOME
               WHEN 4
                    PERFORM INSERIR-ALUNO
               WHEN 5
                    PERFORM ACTUALIZAR-ALUNO
               WHEN 6
                    PERFORM APAGAR-ALUNO
               WHEN 7
                    PERFORM CALCULAR-MEDIA
               WHEN OTHER
                    DISPLAY "Opcao invalida, tente novamente."
            END-EVALUATE
        END-PERFORM
STOP RUN.


DESPEDIDA.
    DISPLAY "Obrigado, volte sempre."
    MOVE 1 TO FIM-PROGRAMA.

APRESENTACAO.
    DISPLAY "Bem-vindo, por favor escolha uma das seguintes opcoes: "
    DISPLAY "1 - Listar todos os alunos"
    DISPLAY "2 - Pesquisar aluno por id"
    DISPLAY "3 - Pesquisar aluno por nome (em construcao)"
    DISPLAY "4 - Inserir novo aluno"
    DISPLAY "5 - Modificar aluno"
    DISPLAY "6 - Apagar aluno"
    DISPLAY "7 - Calcular media"

    DISPLAY "0 ou tecla ENTER - Sair"
    ACCEPT OPCAO-MENU.

CRIAR-FICHEIRO.
    OPEN OUTPUT FICHEIRO-ALUNOS
        DISPLAY "Ficheiro criado com sucesso."
    CLOSE FICHEIRO-ALUNOS.

INSERIR-ALUNO.
    PERFORM WITH TEST AFTER UNTIL SAIDA-INSERIR = 0
        PERFORM RECEBER-DADOS
        PERFORM ESCREVER-FICHEIRO
        PERFORM MOSTRAR-INSERCAO
        DISPLAY "Prima 0 para voltar ao menu anterior ou 1 para inserir novo aluno)"
        ACCEPT SAIDA-INSERIR
    END-PERFORM.

RECEBER-DADOS.
    MOVE 'N' TO ID-VALIDO
    PERFORM UNTIL ID-VALIDO = 'Y'
        OPEN INPUT FICHEIRO-ALUNOS

        DISPLAY "Introduza o ID do aluno"
        ACCEPT MAIN-ID-ALUNO

        MOVE MAIN-ID-ALUNO TO ID-ALUNO
        READ FICHEIRO-ALUNOS KEY IS ID-ALUNO
            INVALID KEY
                MOVE 'Y' TO ID-VALIDO
            NOT INVALID KEY
                DISPLAY "ID ja existe. Por favor, insira um ID diferente."
        END-READ


        CLOSE FICHEIRO-ALUNOS
    END-PERFORM


    DISPLAY "Introduza o nome do aluno"
    ACCEPT MAIN-NOME-ALUNO
    DISPLAY "Introduza a primeira nota"
    ACCEPT MAIN-NOTA1
    DISPLAY "Introduza a segunda nota"
    ACCEPT MAIN-NOTA2
    DISPLAY "Introduza a terceira nota"
    ACCEPT MAIN-NOTA3.

ESCREVER-FICHEIRO.
    OPEN I-O FICHEIRO-ALUNOS
         MOVE MAIN-ID-ALUNO TO ID-ALUNO
         MOVE MAIN-NOME-ALUNO TO NOME-ALUNO
         MOVE MAIN-NOTA1 TO NOTA1
         MOVE MAIN-NOTA2 TO NOTA2
         MOVE MAIN-NOTA3 TO NOTA3
         WRITE REGISTO-ALUNO
    CLOSE FICHEIRO-ALUNOS.

MOSTRAR-INSERCAO.
    DISPLAY MAIN-NOME-ALUNO " introduzido com o ID: " MAIN-ID-ALUNO
    DISPLAY "Nota1: " MAIN-NOTA1
    DISPLAY "Nota2: " MAIN-NOTA2
    DISPLAY "Nota3: " MAIN-NOTA3 ".".

LISTAR-ALUNOS.
    MOVE 'N' TO MAIN-FIM-FICHEIRO.
    OPEN INPUT FICHEIRO-ALUNOS.
        PERFORM UNTIL MAIN-FIM-FICHEIRO = 'Y'

            PERFORM UNTIL MAIN-FIM-FICHEIRO = 'Y'
                READ FICHEIRO-ALUNOS
                    AT END MOVE 'Y' TO MAIN-FIM-FICHEIRO
                    NOT AT END
                        DISPLAY "-------------------------------"
                        DISPLAY "Notas de " NOME-ALUNO
                        DISPLAY "ID: "      ID-ALUNO
                        DISPLAY "Nota1: "   NOTA1
                        DISPLAY "Nota2: "   NOTA2
                        DISPLAY "Nota3: "   NOTA3
                        DISPLAY "-------------------------------"
                END-READ
            END-PERFORM
         END-PERFORM
    CLOSE FICHEIRO-ALUNOS.
    DISPLAY "Prima qualquer tecla para continuar"
    ACCEPT OMITTED.

GET_ALUNO_BY_ID.
    MOVE 'N' TO MAIN-FIM-FICHEIRO.
    DISPLAY "Introduza o id do aluno que deseja pesquisar: " NO ADVANCING
    ACCEPT MAIN-ID-ALUNO

    OPEN INPUT FICHEIRO-ALUNOS.
        PERFORM UNTIL MAIN-FIM-FICHEIRO = 'Y'
           READ FICHEIRO-ALUNOS KEY IS MAIN-ID-ALUNO


            AT END MOVE 'Y' TO MAIN-FIM-FICHEIRO
            NOT AT END
                IF ID-ALUNO = MAIN-ID-ALUNO
                    DISPLAY "-------------------------------"
                    DISPLAY "ID do aluno: " MAIN-ID-ALUNO
                    DISPLAY "Nome do aluno: " NOME-ALUNO
                    DISPLAY "Nota1: " NOTA1
                    DISPLAY "Nota2: " NOTA2
                    DISPLAY "Nota3: " NOTA3 "."
                    DISPLAY "-------------------------------"
                    MOVE 'Y' TO MAIN-FIM-FICHEIRO
           END-READ
        END-PERFORM
    CLOSE FICHEIRO-ALUNOS.
    DISPLAY "Prima qualquer tecla para continuar..."
    ACCEPT OMITTED.

APAGAR-ALUNO.
    MOVE 'N' TO MAIN-FIM-FICHEIRO
    DISPLAY "Introduza o ID do aluno que deseja apagar: " WITH NO ADVANCING.
    ACCEPT MAIN-ID-ALUNO.

        OPEN I-O FICHEIRO-ALUNOS.
        PERFORM UNTIL MAIN-FIM-FICHEIRO = 'Y'
           READ FICHEIRO-ALUNOS
            AT END MOVE 'Y' TO MAIN-FIM-FICHEIRO
            NOT AT END
                IF ID-ALUNO = MAIN-ID-ALUNO
                    MOVE  NOME-ALUNO TO MAIN-NOME-ALUNO
                    DELETE FICHEIRO-ALUNOS
                        INVALID KEY DISPLAY "Id nao existe."
                    END-DELETE
                    MOVE 'Y' TO MAIN-FIM-FICHEIRO

           END-READ
        END-PERFORM
    CLOSE FICHEIRO-ALUNOS
    DISPLAY "O aluno " MAIN-NOME-ALUNO " foi apagado com sucesso".
    DISPLAY "Prima qualquer tecla para continuar...".
    ACCEPT OMITTED.

ACTUALIZAR-ALUNO.
    MOVE 'N' TO MAIN-FIM-FICHEIRO.
    DISPLAY "Introduza o id do aluno que deseja alterar: " NO ADVANCING
    ACCEPT MAIN-ID-ALUNO

    OPEN I-O FICHEIRO-ALUNOS.
        PERFORM UNTIL MAIN-FIM-FICHEIRO = 'Y'
           READ FICHEIRO-ALUNOS
            AT END MOVE 'Y' TO MAIN-FIM-FICHEIRO
            NOT AT END
                IF ID-ALUNO = MAIN-ID-ALUNO
                    DISPLAY "-------------------------------"
                    DISPLAY "Vai alterar o nome de " NOME-ALUNO
                    DISPLAY "Tem a certeza que pretende alterar ? prima 0 para continuar ou 1 para sair"
                    ACCEPT CONFIRMACAO-ATUALIZAR
                    IF CONFIRMACAO-ATUALIZAR = 0

                        DISPLAY " indique o novo nome: " NO ADVANCING
                        ACCEPT MAIN-NOME-ALUNO
                        DISPLAY "Indique a nova nota 1 de " MAIN-NOME-ALUNO ":" NO ADVANCING
                        ACCEPT MAIN-NOTA1
                        DISPLAY "Indique a nova nota 2 de " MAIN-NOME-ALUNO NO ADVANCING
                        ACCEPT MAIN-NOTA2
                        DISPLAY "Indique a nova nota 3 de " MAIN-NOME-ALUNO NO ADVANCING
                        ACCEPT MAIN-NOTA3

                        MOVE MAIN-NOME-ALUNO TO NOME-ALUNO
                        MOVE MAIN-NOTA1 TO NOTA1
                        MOVE MAIN-NOTA2 TO NOTA2
                        MOVE MAIN-NOTA3 TO NOTA3
                        REWRITE REGISTO-ALUNO

                        MOVE 'Y' TO MAIN-FIM-FICHEIRO
                        DISPLAY "Os dados do aluno " MAIN-NOME-ALUNO " foram alterados com sucesso"
                        DISPLAY "-------------------------------"
                        DISPLAY "Notas de " NOME-ALUNO
                        DISPLAY "ID: " ID-ALUNO
                        DISPLAY "Nota1: " NOTA1
                        DISPLAY "Nota2: " NOTA2
                        DISPLAY "Nota3: " NOTA3 "."
                        DISPLAY "-------------------------------"
                    ELSE
                        MOVE 'Y' TO MAIN-FIM-FICHEIRO
               END-READ
            END-PERFORM
        CLOSE FICHEIRO-ALUNOS.
        DISPLAY "Prima qualquer tecla para continuar"

    ACCEPT OMITTED.

GET_ALUNO_BY_NOME.
    MOVE 'N' TO MAIN-FIM-FICHEIRO.
    DISPLAY "Introduza o nome do aluno que deseja pesquisar: " NO ADVANCING
    ACCEPT MAIN-NOME-ALUNO

    OPEN INPUT FICHEIRO-ALUNOS.
        PERFORM UNTIL MAIN-FIM-FICHEIRO = 'Y'
           READ FICHEIRO-ALUNOS
            AT END MOVE 'Y' TO MAIN-FIM-FICHEIRO
            NOT AT END
                IF NOME-ALUNO = MAIN-NOME-ALUNO
                    DISPLAY "-------------------------------"
                    DISPLAY "ID do aluno: " ID-ALUNO
                    DISPLAY "Nome do aluno: " NOME-ALUNO
                    DISPLAY "Nota1: " NOTA1
                    DISPLAY "Nota2: " NOTA2
                    DISPLAY "Nota3: " NOTA3 "."
                    DISPLAY "-------------------------------"
                    MOVE 'Y' TO MAIN-FIM-FICHEIRO
           END-READ
        END-PERFORM
    CLOSE FICHEIRO-ALUNOS.
    DISPLAY "Prima qualquer tecla para continuar..."
    ACCEPT OMITTED.

CALCULAR-MEDIA.
    MOVE 'N' TO MAIN-FIM-FICHEIRO.
    DISPLAY "Introduza o id do aluno que pretende calcular a media: "
    ACCEPT ID-MEDIA

     OPEN INPUT FICHEIRO-ALUNOS.
        PERFORM UNTIL MAIN-FIM-FICHEIRO = 'Y'
           READ FICHEIRO-ALUNOS
            AT END MOVE 'Y' TO MAIN-FIM-FICHEIRO
            NOT AT END
                IF ID-ALUNO = ID-MEDIA
                    MOVE NOTA1 TO MAIN-NOTA1
                    MOVE NOTA2 TO MAIN-NOTA2
                    MOVE NOTA3 TO MAIN-NOTA3
                    COMPUTE MEDIA-ALUNO = (NOTA1 + NOTA2 + NOTA3) / 3
                    DISPLAY "-------------------------------"
                    DISPLAY "ID do aluno: " ID-ALUNO
                    DISPLAY "Nome do aluno: " NOME-ALUNO
                    DISPLAY "Nota1: " NOTA1 "."
                    DISPLAY "Nota2: " NOTA2 "."
                    DISPLAY "Nota3: " NOTA3 "."
                    DISPLAY "A media: " MEDIA-ALUNO "."


                    DISPLAY "-------------------------------"
                    MOVE 'Y' TO MAIN-FIM-FICHEIRO
           END-READ
        END-PERFORM
    CLOSE FICHEIRO-ALUNOS.
    DISPLAY "Prima qualquer tecla para continuar..."
    ACCEPT OMITTED.
