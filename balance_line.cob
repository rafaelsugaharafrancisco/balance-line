      ******************************************************************
      * Author: Rafael
      * Date: 2021-10-29
      * Purpose: treinar balance line
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. BALANCELINE.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
           SELECT ENTRADA ASSIGN TO 'C:\Users\rafap\cobol\ENTRADA.DAT'
           FILE STATUS IS WS-FS-ENTRADA
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DADOS ASSIGN TO 'C:\Users\rafap\cobol\DADOS.DAT'
           FILE STATUS IS WS-FS-DADOS
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SAIDA ASSIGN TO 'C:\Users\rafap\cobol\SAIDA.DAT'
           FILE STATUS IS WS-FS-SAIDA
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD ENTRADA.
      *    BLOCK CONTAINS 0 RECORDS
      *    RECORDING MODE IS F.
      *    RECORD CONTAINS 07 CHARACTERS.
       01 REG-ENTRADA.
           05 ENTRADA-COD-PRODUTO      PIC 9(03).
           05 ENTRADA-COD-SUBPRODUTO   PIC 9(04).

       FD DADOS.
      *    BLOCK CONTAINS 0 RECORDS
      *    RECORDING MODE IS F.
      *    RECORD CONTAINS 024 CHARACTERS.
       01 REG-DADOS.
           05 DADOS-COD-SUBPRODUTO     PIC 9(04).
           05 DADOS-DESC-SUBPRODUTO    PIC X(20).

       FD SAIDA.
      *    BLOCK CONTAINS 0 RECORDS
      *    RECORDING MODE IS F.
      *    RECORD CONTAINS 027 CHARACTERS.
       01 REG-SAIDA.
           05 SAIDA-COD-PRODUTO        PIC 9(03).
           05 SAIDA-COD-SUBPRODUTO     PIC 9(04).
           05 SAIDA-DESC-SUBPRODUTO    PIC X(20).

      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
       01 WS-FS-ENTRADA                PIC X(02) VALUE SPACES.
       01 WS-FS-DADOS                  PIC X(02) VALUE SPACES.
       01 WS-FS-SAIDA                  PIC X(02) VALUE SPACES.
       01 WS-FS-DESPREZADO             PIC X(02) VALUE SPACES.
       01 WS-ERRO                      PIC X(40) VALUE SPACES.
       01 WS-FILE-STATUS               PIC X(02) VALUE SPACES.
       01 WS-QTDE-LIDOS-ENT            PIC 9(04) VALUE ZEROES.
       01 WS-QTDE-LIDOS-DADOS          PIC 9(04) VALUE ZEROES.
       01 WS-QTDE-GRAVADOS             PIC 9(04) VALUE ZEROES.
       01 WS-QTDE-DESPREZADOS          PIC 9(04) VALUE ZEROES.

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PERFORM 1000-INICIAR.
       PERFORM 2000-PROCESSAR.
       PERFORM 3000-FINALIZAR.
      ******************************************************************
      * 1000-INICIAR
      ******************************************************************
       1000-INICIAR.
           OPEN INPUT ENTRADA.
           IF WS-FS-ENTRADA NOT EQUAL '00'
              MOVE 'ABERTURA DO ARQUIVO ENTRADA' TO WS-ERRO
              MOVE WS-FS-ENTRADA TO WS-FILE-STATUS
              PERFORM 4000-ERRO
           END-IF.

           OPEN INPUT DADOS.
           IF WS-FS-DADOS NOT EQUAL '00'
              MOVE 'ABERTURA DO ARQUIVO DADOS' TO WS-ERRO
              MOVE WS-FS-DADOS   TO WS-FILE-STATUS
              PERFORM 4000-ERRO
           END-IF.

           OPEN OUTPUT SAIDA.
           IF WS-FS-SAIDA NOT EQUAL '00'
              MOVE 'ABERTURA DO ARQUIVO SAIDA' TO WS-ERRO
              MOVE WS-FS-SAIDA TO WS-FILE-STATUS
              PERFORM 4000-ERRO
           END-IF.

       1000-END-PERFORM.
      ******************************************************************
      * 2000-PROCESSAR
      ******************************************************************
       2000-PROCESSAR.
           PERFORM 2100-LER-ARQUIVO-ENTRADA.
           PERFORM 2200-LER-ARQUIVO-DADOS.
           PERFORM UNTIL WS-FS-ENTRADA EQUAL '10' OR
                         WS-FS-DADOS EQUAL '10'
                   IF ENTRADA-COD-SUBPRODUTO EQUAL
                      DADOS-COD-SUBPRODUTO
                      PERFORM 2300-GRAVAR
                      PERFORM 2100-LER-ARQUIVO-ENTRADA
                      PERFORM 2200-LER-ARQUIVO-DADOS
                   ELSE
                       IF ENTRADA-COD-SUBPRODUTO LESS
                          DADOS-COD-SUBPRODUTO
                          PERFORM 2100-LER-ARQUIVO-ENTRADA
                       ELSE
                           IF ENTRADA-COD-SUBPRODUTO GREATER
                              DADOS-COD-SUBPRODUTO
                              PERFORM 2200-LER-ARQUIVO-DADOS
                           END-IF
                       END-IF
                   END-IF
           END-PERFORM.
       2000-END-PERFORM.
      ******************************************************************
      * 2100-LER-ARQUIVO-ENTRADA
      ******************************************************************
       2100-LER-ARQUIVO-ENTRADA.
           INITIALIZE REG-ENTRADA REPLACING NUMERIC BY ZEROES.
           READ ENTRADA INTO REG-ENTRADA.
           IF WS-FS-ENTRADA NOT EQUAL '00' AND
              WS-FS-ENTRADA NOT EQUAL '10'
              MOVE 'LEITURA DO ARQUIVO ENTRADA' TO WS-ERRO
              MOVE WS-FS-ENTRADA TO WS-FILE-STATUS
              PERFORM 4000-ERRO
           ELSE
              IF WS-FS-ENTRADA EQUAL '00'
                 ADD 1 TO WS-QTDE-LIDOS-ENT
              END-IF
           END-IF.
       2100-END-PERFORM.
      ******************************************************************
      * 2100-LER-ARQUIVO-DADOS
      ******************************************************************
       2200-LER-ARQUIVO-DADOS.
           INITIALIZE REG-DADOS REPLACING NUMERIC BY ZEROES
                                          ALPHANUMERIC BY SPACES.
           READ DADOS INTO REG-DADOS.
           IF WS-FS-DADOS NOT EQUAL '00' AND
              WS-FS-DADOS NOT EQUAL '10'
              MOVE 'LEITURA DO ARQUIVO DADOS' TO WS-ERRO
              MOVE WS-FS-DADOS TO WS-FILE-STATUS
              PERFORM 4000-ERRO
           ELSE
               IF WS-FS-DADOS EQUAL '00'
                  ADD 1 TO WS-QTDE-LIDOS-DADOS
               END-IF
           END-IF.
       2200-END-PERFORM.
      ******************************************************************
      * 2300-GRAVAR
      ******************************************************************
       2300-GRAVAR.
           INITIALIZE REG-SAIDA REPLACING ALPHANUMERIC BY SPACES
                                          NUMERIC BY ZEROES.
           MOVE ENTRADA-COD-PRODUTO    TO SAIDA-COD-PRODUTO.
           MOVE ENTRADA-COD-SUBPRODUTO TO SAIDA-COD-SUBPRODUTO.
           MOVE DADOS-DESC-SUBPRODUTO  TO SAIDA-DESC-SUBPRODUTO.
           WRITE REG-SAIDA.
           IF WS-FS-SAIDA NOT EQUAL '00' AND
              WS-FS-SAIDA NOT EQUAL '10'
              MOVE 'GRAVACAO DO ARQUIVO SAIDA' TO WS-ERRO
              MOVE WS-FS-SAIDA TO WS-FILE-STATUS
              PERFORM 4000-ERRO
           ELSE
               IF WS-FS-SAIDA EQUAL '00'
                  ADD 1 TO WS-QTDE-GRAVADOS
               END-IF
           END-IF.
       2300-END-PERFORM.
      ******************************************************************
      * 3000-FINALIZAR
      ******************************************************************
       3000-FINALIZAR.
           CLOSE ENTRADA DADOS SAIDA.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*            PROGRAMA BALANCELINE                *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '* QTDE DE REGS LIDOS ENTRADA ' WS-QTDE-LIDOS-ENT
           DISPLAY '* QTDE DE REGS LIDOS DADOS ' WS-QTDE-LIDOS-DADOS
           DISPLAY '* QTDE DE REGS GRAVADOS SAIDA ' WS-QTDE-GRAVADOS
           STOP RUN.
       3000-END-PERFORM.
      ******************************************************************
      * 4000-ERRO
      *****************************************************************
       4000-ERRO.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*            PROGRAMA BALANCELINE                *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '* ERRO: ' WS-ERRO.
           DISPLAY '* FILE STATUS: ' WS-FILE-STATUS.
           STOP RUN.
       4000-END-PERFORM.
