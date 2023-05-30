       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAJASFDB.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MESSG  ASSIGN MESS
             FILE STATUS  WS-MESS.
           SELECT ANO     ASSIGN ANO
             FILE STATUS  WS-ANO.
       DATA DIVISION.
       FILE SECTION.
       FD  MESSG RECORDING F.
       01  EMESSG             PIC X(80).  
       FD  ANO  RECORDING F.
       01  EANO               PIC X(80).
       WORKING-STORAGE SECTION.
       77  WS-MESS        PIC XX.
       77  WS-ANO        PIC XX.
       01  NB-MESS  PIC X(3).    
       01  MESS           PIC X(60).
       77  SOUSPROG3      PIC X(8) VALUE 'SMESSERR'.
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
           EXEC SQL
              INCLUDE TASR
           END-EXEC.
           EXEC SQL
              INCLUDE TMVT
           END-EXEC.
       01  ERR-MSG.
           05  ERR-LONG   PIC S9(4) COMP VALUE +720.
           05  ERR-TXT    PIC X(72) OCCURS 10 TIMES.
       01  ERR-TXT-LONG   PIC S9(9) COMP VALUE 72.
       01  I              PIC 99.
       LINKAGE SECTION.
       01  MVT.
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
       PROCEDURE DIVISION USING MVT.
           OPEN OUTPUT MESSG
           OPEN OUTPUT ANO
           PERFORM TEST-STAT-MESS
           MOVE MVT TO EMESSG
           WRITE EMESSG
      * AFFICHAGE DE LA DATE ET DE L'HEURE      
           PERFORM TEST-SQLCODE
           PERFORM CHECK-MOV
           CLOSE MESSG
           CLOSE ANO
           PERFORM TEST-STAT-MESS
           GOBACK
           .

       CHECK-MOV.
           EVALUATE CDE-MVT
           WHEN 'C'
             PERFORM CREA-LIGNE
           WHEN 'M'
             PERFORM MODIF
           WHEN 'S'
             PERFORM SUPP
           WHEN OTHER
             MOVE '001' TO NB-MESS
             PERFORM WRITE-ERROR
           END-EVALUATE
           .

       MODIF.
           PERFORM MVTODCLGEN
           EXEC SQL
               UPDATE TASSURES
                SET MAT = :M-MAT, NOM_PRN = :M-NOM-PRN, ADSS = :M-ADSS, 
                  CP = :M-CP, VILLE = :M-VILLE, TV = :M-TV, PB = :M-PB,
                  BON_MAL = :M-BON-MAL, TAUX = :M-TAUX
                WHERE :M-MAT = MAT
           END-EXEC
           IF SQLCODE = 100
             MOVE '003' TO NB-MESS
             PERFORM WRITE-ERROR
           ELSE 
             PERFORM TEST-SQLCODE
           END-IF 
           .
       SUPP.
           
           EXEC SQL
                 DELETE FROM TASSURES WHERE :MVT-MATRICUL = MAT
           END-EXEC
           IF SQLCODE = 100
             MOVE '004' TO NB-MESS
             PERFORM WRITE-ERROR
           ELSE 
             PERFORM TEST-SQLCODE
           END-IF 
         
           .
       CREA-LIGNE.
           PERFORM MVTODCLGEN
           EXEC SQL
               INSERT INTO TASSURES 
               VALUES (:M-MAT, :M-NOM-PRN, :M-ADSS, :M-CP, :M-VILLE,
                 :M-TV, :M-PB, :M-BON-MAL, :M-TAUX)
             END-EXEC
             IF SQLCODE = -803
               MOVE '004' TO NB-MESS
               PERFORM WRITE-ERROR
             ELSE
               PERFORM TEST-SQLCODE
             END-IF
           .
       MVTODCLGEN.
           MOVE MVT-MATRICUL  TO M-MAT
           MOVE MVT-NM-PRNM   TO M-NOM-PRN
           MOVE MVT-ADSS      TO M-ADSS
           MOVE MVT-CP        TO M-CP
           MOVE MVT-VLL       TO M-VILLE
           MOVE MVT-TPV       TO M-TV
           MOVE MVT-PRMBS     TO M-PB
           MOVE MVT-PRMT      TO M-BON-MAL
           MOVE MVT-TAUX      TO M-TAUX
           .

         
       TEST-STAT-MESS.
           IF WS-MESS NOT = '00'
             DISPLAY 'ERREUR FICHIER MESS ' WS-MESS
             MOVE 16 TO RETURN-CODE
             STOP RUN
           END-IF
           .   

       TEST-SQLCODE.
           IF SQLCODE NOT = 0
             PERFORM PARAG-ERR
           END-IF
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

       WRITE-MESS.
           WRITE EMESSG
           MOVE ALL ' ' TO EMESSG
           .
       WRITE-ERROR.
           CALL SOUSPROG3 USING NB-MESS MESS M-MAT M-CDE-MVT
           MOVE ALL ' ' TO EANO
           STRING M-MAT ' ERREUR : ' MESS 'IN DB PROC'
               DELIMITED BY SIZE INTO EANO
           WRITE EANO
           .

            