       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAJASDB.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STAT  ASSIGN STAT
             FILE STATUS  WS-STAT.
           SELECT ANO     ASSIGN ANO
             FILE STATUS  WS-ANO.
       DATA DIVISION.
       FILE SECTION.
       FD  STAT RECORDING F.
       01  ESTAT             PIC X(80).  
       FD  ANO  RECORDING F.
       01  EANO               PIC X(80).
       WORKING-STORAGE SECTION.
       77  WS-ANO         PIC XX.
       77  WS-STAT        PIC XX.
       77  DISP-PRM       PIC 9(4)V,99.
       77  LGN-DEB2       PIC X(80) VALUE ALL SPACE.
       77  LGN-DEB3       PIC X(80) VALUE ALL SPACE.
       77  LGN-DEB4       PIC X(80) VALUE ALL SPACE.
       77  LGN-DISP       PIC X(80) VALUE ALL SPACE. 
       77  SOUSPROG1      PIC X(8) VALUE 'SPDATE'.
       77  SOUSPROG2      PIC X(8) VALUE 'SHOURS'.
       77  SOUSPROG3      PIC X(8) VALUE 'SMESSERR'.
       77  DAT            PIC X(30).
       77  CODEDG         PIC X VALUE 'D'.
       77  CODECL         PIC X VALUE 'L'.
       01  TIM            PIC X(8).
       77  CPT-MT             PIC 99.
       77  CPT-GA             PIC 99.
       77  CPT-MW             PIC 99.
       77  CPT-MA             PIC 99.
       77  CPT-ST             PIC 99.
       77  CPT-SW             PIC 99.
       77  CPT-SA             PIC 99.
       77  CPT-CT             PIC 99.
       77  CPT-CW             PIC 99.
       77  CPT-CA             PIC 99.
       77  NB-LIGNE-DEB       PIC S9(8) COMP.
       77  NB-LIGNE-FIN       PIC S9(8) COMP.
       01  DIS-RESP           PIC Z99.
       01  DIS-RESP9          PIC 999.
       01  TAB-PRM.
           05 PRM-OK  OCCURS 10 INDEXED BY IND-1 PIC X.
       01  NB-MESS  PIC X(3).    
       01  MESS           PIC X(60).
       77  LIGNE-RESV1        PIC X(80).
       77  LIGNE-RESV2        PIC X(80).
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
           EXEC SQL
              INCLUDE TASR
           END-EXEC.
           EXEC SQL
              INCLUDE TMVT
           END-EXEC.
           EXEC SQL DECLARE CURSA CURSOR FOR 
               SELECT *
                 FROM TASSURES 
                 ORDER BY MAT ASC
           END-EXEC
           EXEC SQL DECLARE CURSM CURSOR FOR 
               SELECT *
                 FROM TMVTPROJ 
                 ORDER BY MAT ASC
           END-EXEC
       01  ERR-MSG.
           05  ERR-LONG   PIC S9(4) COMP VALUE +720.
           05  ERR-TXT    PIC X(72) OCCURS 10 TIMES.
       01  ERR-TXT-LONG   PIC S9(9) COMP VALUE 72.
       01  I              PIC 99.

       PROCEDURE DIVISION.
           OPEN OUTPUT ANO
           OPEN OUTPUT STAT
           PERFORM TEST-STAT-STAT
           PERFORM TEST-STAT-ANO
      * AFFICHAGE DE LA DATE ET DE L'HEURE      
           PERFORM CREA-DAT-TIME
           EXEC SQL
             SELECT COUNT(MAT)             
               INTO :NB-LIGNE-DEB
               FROM TASSURES
           END-EXEC
           PERFORM TEST-SQLCODE

           EXEC SQL OPEN CURSM END-EXEC
           PERFORM TEST-SQLCODE
           EXEC SQL FETCH CURSM
               INTO :M-MAT, :M-NOM-PRN, :M-ADSS, :M-CP, :M-VILLE, :M-TV,
                    :M-PB, :M-BON-MAL, :M-TAUX, :M-CDE-MVT
           END-EXEC 
           PERFORM UNTIL SQLCODE = 100
             PERFORM CHECK-MOV
             
             EXEC SQL FETCH CURSM
               INTO :M-MAT, :M-NOM-PRN, :M-ADSS, :M-CP, :M-VILLE, :M-TV,
                    :M-PB, :M-BON-MAL, :M-TAUX, :M-CDE-MVT
             END-EXEC 
           END-PERFORM
           EXEC SQL CLOSE CURSM END-EXEC
           PERFORM TEST-SQLCODE
           EXEC SQL
             SELECT COUNT(MAT)             
               INTO :NB-LIGNE-FIN
               FROM TASSURES
           END-EXEC
           PERFORM TEST-SQLCODE
           PERFORM CREA-TABL-STAT
           CLOSE ANO
           CLOSE STAT
           PERFORM TEST-STAT-ANO
           PERFORM TEST-STAT-STAT
           GOBACK
           .

       CHECK-MOV.
           EVALUATE M-CDE-MVT
           WHEN 'C'
             ADD 1 to CPT-CT
             PERFORM CREA-LIGNE
           WHEN 'M'
             ADD 1 to CPT-MT
             PERFORM MODIF
           WHEN 'S'
             ADD 1 to CPT-ST
             PERFORM SUPP
           WHEN OTHER
             MOVE '001' TO NB-MESS
             ADD 1 to CPT-GA
             PERFORM WRITE-ERROR
           END-EVALUATE
           .

       MODIF.
           EXEC SQL
               UPDATE TASSURES
                SET MAT = :M-MAT, NOM_PRN = :M-NOM-PRN, ADSS = :M-ADSS, 
                  CP = :M-CP, VILLE = :M-VILLE, TV = :M-TV, PB = :M-PB,
                  BON_MAL = :M-BON-MAL, TAUX = :M-TAUX
                WHERE :M-MAT = MAT
           END-EXEC
           IF SQLCODE = 100
             ADD 1 to CPT-MA
             MOVE '003' TO NB-MESS
             PERFORM WRITE-ERROR
           ELSE 
             ADD 1 TO CPT-MW
             PERFORM TEST-SQLCODE
           END-IF 
           .
       SUPP.
           EXEC SQL
                 DELETE FROM TASSURES WHERE :M-MAT = MAT
           END-EXEC
           IF SQLCODE = 100
             ADD 1 to CPT-SA
             MOVE '004' TO NB-MESS
             PERFORM WRITE-ERROR
           ELSE 
             ADD 1 TO CPT-SW
             PERFORM TEST-SQLCODE
           END-IF 
         
           .
       CREA-LIGNE.
           EXEC SQL
               INSERT INTO TASSURES 
               VALUES (:M-MAT, :M-NOM-PRN, :M-ADSS, :M-CP, :M-VILLE,
                 :M-TV, :M-PB, :M-BON-MAL, :M-TAUX)
             END-EXEC
             IF SQLCODE = -803
               ADD 1 to CPT-CA
               MOVE '004' TO NB-MESS
               PERFORM WRITE-ERROR
             ELSE
               ADD 1 TO CPT-CW
               ADD 1 TO NB-LIGNE-FIN
               PERFORM TEST-SQLCODE
             END-IF
           .
      
       CREA-TABL-STAT.
           MOVE NB-LIGNE-DEB TO DIS-RESP.
           STRING '005 - NOMBRE D''ENREGISTREMENTS ASSURES LU     : ' 
                DIS-RESP  DELIMITED BY SIZE INTO ESTAT
           PERFORM WRITE-STAT
           MOVE 0 TO DIS-RESP9
           ADD CPT-MT CPT-ST CPT-CT CPT-GA TO DIS-RESP9.
           MOVE DIS-RESP9 TO DIS-RESP
           STRING '006 - NOMBRE D''ENREGISTREMENTS MOUVEMENTS LUS : '
               DIS-RESP DELIMITED BY SIZE INTO ESTAT
           PERFORM WRITE-STAT
           MOVE CPT-CW TO DIS-RESP.
           STRING '008 - NOMBRE D''ENREGISTREMENTS EN CREATION    : ' 
              DIS-RESP  DELIMITED BY SIZE INTO ESTAT
           PERFORM WRITE-STAT
           MOVE NB-LIGNE-FIN TO DIS-RESP.
           STRING '008 - NOMBRE D''ENREGISTREMENTS FIN PROG       : ' 
              DIS-RESP DELIMITED BY SIZE INTO ESTAT
           PERFORM WRITE-STAT
           MOVE CPT-MW TO DIS-RESP.
           STRING '009 - NOMBRE D''ENREGISTREMENTS MODIFIES       : ' 
              DIS-RESP  DELIMITED BY SIZE INTO ESTAT
           PERFORM WRITE-STAT
           MOVE CPT-SW TO DIS-RESP.
           STRING '010 - NOMBRE D''ENREGISTREMENTS SUPPRIMES      : ' 
             DIS-RESP  DELIMITED BY SIZE INTO ESTAT
           PERFORM WRITE-STAT
           MOVE SPACES TO ESTAT.
           PERFORM WRITE-STAT
           MOVE 0 TO DIS-RESP9
           ADD CPT-GA CPT-MA CPT-CA CPT-SA TO DIS-RESP9.
           MOVE DIS-RESP9 TO DIS-RESP
           STRING '007 - NOMBRE D''ENREGISTREMENTS EN ANOMALIE    : ' 
              DIS-RESP DELIMITED BY SIZE INTO ESTAT
           PERFORM WRITE-STAT
           MOVE CPT-GA TO DIS-RESP.
           STRING '           011 - ANOMALIE DE CODE MOUVEMENT   : ' 
              DIS-RESP  DELIMITED BY SIZE INTO ESTAT
           PERFORM WRITE-STAT
           MOVE CPT-CA TO DIS-RESP.
           STRING '           012 - ANOMALIE DE CREATION         : ' 
              DIS-RESP  DELIMITED BY SIZE INTO ESTAT
           PERFORM WRITE-STAT
           MOVE CPT-MA TO DIS-RESP.
           STRING '           013 - ANOMALIE DE MISE A JOUR      : ' 
              DIS-RESP  DELIMITED BY SIZE INTO ESTAT
           PERFORM WRITE-STAT
           MOVE CPT-SA TO DIS-RESP.
           STRING '           014 - ANOMALIE DE SUPPRESSION      : ' 
              DIS-RESP  DELIMITED BY SIZE INTO ESTAT
           PERFORM WRITE-STAT
           .
           
       CREA-DAT-TIME.
           CALL SOUSPROG1 USING DAT CODEDG CODECL
           CALL SOUSPROG2 USING TIM 
           MOVE 'API3' TO LGN-DEB3(1:4) LGN-DEB4(1:4)
           MOVE 'STATISTIQUE SUR CONTROLE DU FICHIER MOUVEMENT' TO 
                LGN-DEB3(8:45)
           MOVE 'LISTE DES ANOMALIES' TO LGN-DEB4(14:32)
           MOVE DAT TO LGN-DEB3(51:30) LGN-DEB4(51:30)
           MOVE TIM TO LGN-DEB2(73:8)
           STRING LGN-DEB3 DELIMITED BY SIZE INTO ESTAT
           PERFORM WRITE-STAT
           STRING LGN-DEB2 DELIMITED BY SIZE INTO ESTAT
           PERFORM WRITE-STAT
           STRING LGN-DEB4 DELIMITED BY SIZE INTO EANO
           WRITE EANO
           MOVE ALL SPACE TO EANO
           STRING LGN-DEB2 DELIMITED BY SIZE INTO EANO
           WRITE EANO
           MOVE ALL SPACE TO EANO
           .
         
       TEST-STAT-STAT.
           IF WS-STAT NOT = '00'
             DISPLAY 'ERREUR FICHIER STAT ' WS-STAT
             MOVE 16 TO RETURN-CODE
             STOP RUN
           END-IF
           .   
       TEST-STAT-ANO.
           IF WS-STAT NOT = '00'
             DISPLAY 'ERREUR FICHIER ANO ' WS-ANO
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

       WRITE-STAT.
           WRITE ESTAT
           MOVE ALL ' ' TO ESTAT
           .
       WRITE-ERROR.
           CALL SOUSPROG3 USING NB-MESS MESS M-MAT M-CDE-MVT
           MOVE ALL ' ' TO EANO
           STRING M-MAT ' ERREUR : ' MESS
               DELIMITED BY SIZE INTO EANO
           WRITE EANO
           .

            