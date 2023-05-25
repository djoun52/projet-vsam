       IDENTIFICATION DIVISION.
       PROGRAM-ID. FEEDT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
             SELECT ASSUR  ASSIGN ASSUR
             FILE STATUS  WS-ASR.
             SELECT MVT  ASSIGN MVT
             FILE STATUS  WS-MVT.
       DATA DIVISION.
       FILE SECTION.
       FD  ASSUR RECORDING F.
       01  EASSUR.
           05 ASR-MATRICUL   PIC X(6).
           05 ASR-NM-PRNM    PIC X(20).
           05 ASR-ADSS       PIC X(18).
           05 ASR-CP         PIC 9(5).
           05 ASR-VLL        PIC X(12).
           05 ASR-TPV        PIC X.
           05 ASR-PRMBS      PIC 9(4)V99.
           05 ASR-PRMT       PIC X.
           05 ASR-TAUX       PIC X(2).
           05                PIC X(9).
       FD  MVT RECORDING F.
       01  EMVT.
           05 MVT-MATRICUL   PIC X(6).
           05 MVT-NM-PRNM    PIC X(20).
           05 MVT-ADSS       PIC X(18).
           05 MVT-CP         PIC 9(5).
           05 MVT-VLL        PIC X(12).
           05 MVT-TPV        PIC X.
           05 MVT-PRMBS      PIC 9(4)V99.
           05 MVT-PRMT       PIC X.
           05 MVT-TAUX       PIC X(2).
           05                PIC X.
           05 CDE-MVT        PIC X.
           05 FILLER         PIC X(7).
       WORKING-STORAGE SECTION.
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
           EXEC SQL
              INCLUDE TASR
           END-EXEC.
           EXEC SQL
              INCLUDE TMVT
           END-EXEC.
      *    PARAMETRES DSNTIAR
       01  ERR-MSG.
           05  ERR-LONG   PIC S9(4) COMP VALUE +720.
           05  ERR-TXT    PIC X(72) OCCURS 10 TIMES.
       01  ERR-TXT-LONG   PIC S9(9) COMP VALUE 72.
       01  I              PIC 99.
       01  SQLCD1         PIC S9(4) COMP.
       01  SQLCDA         PIC S9(4) COMP.
       77  WS-ASR         PIC XX.
       77  WS-MVT         PIC XX.

           EXEC SQL DECLARE CURSA CURSOR FOR 
               SELECT *
                 FROM TASSURES 
           END-EXEC
           EXEC SQL DECLARE CURSM CURSOR FOR 
               SELECT *
                 FROM TMVTPROJ 
           END-EXEC
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       PARAG1.
           OPEN INPUT ASSUR
           OPEN INPUT MVT
           PERFORM TEST-STAT-ASSUR
           PERFORM TEST-STAT-MVT
           PERFORM FEEDASSURT
           PERFORM FEEDMVT
           CLOSE MVT    
           CLOSE ASSUR    
           PERFORM TEST-STAT-MVT
           PERFORM TEST-STAT-ASSUR
           .
       PARAG-FIN.
           STOP RUN.

       TEST-SQLCODE.
           IF SQLCODE NOT = 0
             PERFORM PARAG-ERR
           END-IF
           .
       FEEDASSURT.
           READ ASSUR
           PERFORM UNTIL WS-ASR = '10'
             MOVE ASR-MATRICUL TO  A-MAT
             MOVE ASR-NM-PRNM  TO  A-NOM-PRN
             MOVE ASR-ADSS     TO  A-ADSS    
             MOVE ASR-CP       TO  A-CP 
             MOVE ASR-VLL      TO  A-VILLE 
             MOVE ASR-TPV      TO  A-TV
             MOVE ASR-PRMBS    TO  A-PB
             MOVE ASR-PRMT     TO  A-BON-MAL
             MOVE ASR-TAUX     TO  A-TAUX             
             EXEC SQL
               INSERT INTO TASSURES 
               VALUES (:A-MAT, :A-NOM-PRN, :A-ADSS, :A-CP, :A-VILLE,
                 :A-TV, :A-PB, :A-BON-MAL, :A-TAUX)
             END-EXEC
             IF SQLCODE = -803
                 DISPLAY ASR-MATRICUL ' EXISTE DEJA'
             ELSE
               PERFORM TEST-SQLCODE
             END-IF
             READ ASSUR
           END-PERFORM
           .
       FEEDMVT.
           READ MVT
           PERFORM UNTIL WS-MVT = '10'
             MOVE MVT-MATRICUL TO  M-MAT
             MOVE MVT-NM-PRNM  TO  M-NOM-PRN
             MOVE MVT-ADSS     TO  M-ADSS    
             MOVE MVT-CP       TO  M-CP 
             MOVE MVT-VLL      TO  M-VILLE 
             MOVE MVT-TPV      TO  M-TV
             MOVE MVT-PRMBS    TO  M-PB
             MOVE MVT-PRMT     TO  M-BON-MAL
             MOVE MVT-TAUX     TO  M-TAUX             
             MOVE CDE-MVT      TO  M-CDE-MVT
             EXEC SQL
               INSERT INTO TMVTPROJ 
               VALUES (:M-MAT, :M-NOM-PRN, :M-ADSS, :M-CP, :M-VILLE,
                 :M-TV, :M-PB, :M-BON-MAL, :M-TAUX, :M-CDE-MVT)
             END-EXEC
             IF SQLCODE = -803
                 DISPLAY MVT-MATRICUL ' EXISTE DEJA'
             ELSE
               PERFORM TEST-SQLCODE
             END-IF
             READ MVT
           END-PERFORM
           .
       PARAG-ERR.
           DISPLAY 'ERREUR DB2 INSERT'
           DISPLAY 'APPEL DSNTIAR (MISE EN FORME SQLCA)'
           CALL 'DSNTIAR' USING SQLCA, ERR-MSG, ERR-TXT-LONG
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
              DISPLAY ERR-TXT (I)
           END-PERFORM
           EXEC SQL ROLLBACK END-EXEC
           STOP RUN.

       TEST-STAT-ASSUR.
           IF WS-MVT NOT = '00'
             DISPLAY 'ERREUR FICHIER ASSUR ' WS-ASR
             MOVE 16 TO RETURN-CODE
             STOP RUN
           END-IF
           .
       TEST-STAT-MVT.
           IF WS-MVT NOT = '00'
             DISPLAY 'ERREUR FICHIER MOUVEMENT ' WS-MVT
             MOVE 16 TO RETURN-CODE
             STOP RUN
           END-IF
           .
       