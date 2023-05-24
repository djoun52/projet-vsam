       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAJAS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ASSUR     ASSIGN ASSUR
             ORGANIZATION          INDEXED
             ACCESS                SEQUENTIAL
             RECORD KEY            ASR-MATRICUL
             FILE STATUS  WS-ASR.
           SELECT MVT       ASSIGN AS-MVT
             ACCESS MODE           SEQUENTIAL
             FILE STATUS  WS-MVT.
           SELECT RESP ASSIGN RESP
             ORGANIZATION          INDEXED
             ACCESS                SEQUENTIAL
             RECORD KEY            RES-MATRICUL
             FILE STATUS  WS-RESP.
           SELECT STAT  ASSIGN STAT
             FILE STATUS  WS-STAT.
           SELECT ANO     ASSIGN ANO
             FILE STATUS  WS-ANO.
       DATA DIVISION.
       FILE SECTION.
       FD  ASSUR.
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
       FD  MVT.
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
       FD  RESP.
       01  ERESP.  
           05 RES-MATRICUL   PIC X(6).
           05 RES-NM-PRNM    PIC X(20).
           05 RES-ADSS       PIC X(18).
           05 RES-CP         PIC 9(5).
           05 RES-VLL        PIC X(12).
           05 RES-TPV        PIC X.
           05 RES-PRMBS      PIC 9(4)V99.
           05 RES-PRMT       PIC X.
           05 RES-TAUX       PIC X(2).
           05                PIC X(9).
       FD  STAT RECORDING F.
       01  ESTAT             PIC X(80).  
       FD  ANO  RECORDING F.
       01  EANO               PIC X(80).
       WORKING-STORAGE SECTION.
       77  WS-ASR         PIC XX.
       77  WS-MVT         PIC XX.
       77  WS-ANO         PIC XX.
       77  WS-RESP        PIC XX.
       77  WS-STAT        PIC XX.
       77  DISP-PRM       PIC 9(4)V,99.
       77  ERR            PIC 9.
       77  LGN-DEB1       PIC X(80) VALUE ALL SPACE.
       77  LGN-DEB2       PIC X(80) VALUE ALL SPACE.
       77  LGN-DEB3       PIC X(80) VALUE ALL SPACE.
       77  LGN-DEB4       PIC X(80) VALUE ALL SPACE.
       77  INTERLIGNE     PIC X(80) VALUE ALL '-'.
       77  LGN-DISP       PIC X(80) VALUE ALL SPACE. 
       77  SOUSPROG1      PIC X(8) VALUE 'SPDATE'.
       77  SOUSPROG2      PIC X(8) VALUE 'SHOURS'.
       77  SOUSPROG3      PIC X(8) VALUE 'SMESSERR'.
       77  DAT            PIC X(30).
       77  CODEDG         PIC X VALUE 'D'.
       77  CODECL         PIC X VALUE 'L'.
       01  TIM            PIC X(8).
       01  TABLEAU-ASSUR.
           05 LIGNE OCCURS 99.
             10 TB-MATRICUL   PIC X(6).
             10 TB-NM-PRNM    PIC X(20).
             10 TB-ADSS       PIC X(18).
             10 TB-CP         PIC 9(5).
             10 TB-VLL        PIC X(12).
             10 TB-TPV        PIC X.
             10 TB-PRMBS      PIC 9(4)V99.
             10 TB-PRMT       PIC X.
             10 TB-TAUX       PIC X(2).
             10               PIC X(9).
       77  CHECK              PIC 9.
       77  CPT-1              PIC 99.
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
       77  NB-LIGNE-DEB       PIC 99.
       77  NB-LIGNE-FIN       PIC 99.
       77  NB-LIGNE-TAB       PIC 99.
       01  DIS-RESP           PIC Z99.
       01  DIS-RESP9          PIC 999.
       01  TAB-PRM.
           05 PRM-OK  OCCURS 10 INDEXED BY IND-1 PIC X.
       01  NB-MESS  PIC X(3).    
       01  MESS           PIC X(60).
       77  LIGNE-RESV1        PIC X(80).
       77  LIGNE-RESV2        PIC X(80).
       PROCEDURE DIVISION.
           OPEN INPUT ASSUR
           OPEN INPUT MVT
           OPEN OUTPUT RESP
           OPEN OUTPUT ANO
           OPEN OUTPUT STAT
           PERFORM TEST-STAT-ASSUR
           PERFORM TEST-STAT-MVT
           PERFORM TEST-STAT-STAT
           PERFORM TEST-STAT-RESP
           PERFORM TEST-STAT-ANO
      * AFFICHAGE DE LA DATE ET DE L'HEURE      
           PERFORM CREA-DAT-TIME
           MOVE 0 TO CPT-1
           READ ASSUR
           PERFORM CREATAB
           READ MVT
           PERFORM UNTIL WS-MVT = '10'
             PERFORM  CHECK-MOV   
             READ MVT
           END-PERFORM
           PERFORM FEEDRESP
           PERFORM CREA-TABL-STAT
           CLOSE ANO
           CLOSE STAT
           CLOSE RESP
           CLOSE MVT
           CLOSE ASSUR
           PERFORM TEST-STAT-RESP
           PERFORM TEST-STAT-ANO
           PERFORM TEST-STAT-STAT
           PERFORM TEST-STAT-MVT
           PERFORM TEST-STAT-ASSUR
           GOBACK
           .
       CREATAB.
           PERFORM UNTIL WS-ASR = '10'
             ADD 1 TO CPT-1
             IF  CPT-1 > 30
               DISPLAY 'ERREUR TABLEAU-ASSUR TROP PETIT / LIMITE = 30 '
               MOVE 15 TO RETURN-CODE
               STOP RUN
             END-IF
             MOVE ASR-MATRICUL TO TB-MATRICUL(CPT-1)
             MOVE ASR-NM-PRNM  TO TB-NM-PRNM(CPT-1)
             MOVE ASR-ADSS     TO TB-ADSS(CPT-1)
             MOVE ASR-CP       TO TB-CP(CPT-1)
             MOVE ASR-VLL      TO TB-VLL(CPT-1)
             MOVE ASR-TPV      TO TB-TPV(CPT-1)
             MOVE ASR-PRMBS    TO TB-PRMBS(CPT-1)
             MOVE ASR-PRMT     TO TB-PRMT(CPT-1)
             MOVE ASR-TAUX     TO TB-TAUX(CPT-1)
             READ ASSUR
           END-PERFORM
           MOVE CPT-1 TO NB-LIGNE-TAB NB-LIGNE-DEB
           .
       PROMPT-TAB.
           PERFORM VARYING CPT-1 FROM 1 BY 1 UNTIL
                   CPT-1 > NB-LIGNE-TAB
             DISPLAY LIGNE(CPT-1)
           END-PERFORM
           DISPLAY NB-LIGNE-TAB
           .
       CHECK-MOV.
           EVALUATE CDE-MVT
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
           MOVE 0 TO CHECK
           PERFORM VARYING CPT-1 FROM 1 BY 1 UNTIL CPT-1 > NB-LIGNE-TAB
           IF TB-MATRICUL(CPT-1) = MVT-MATRICUL
              MOVE 1 TO CHECK
              ADD 1 TO CPT-MW
              MOVE MVT-NM-PRNM  TO TB-NM-PRNM(CPT-1)
              MOVE MVT-ADSS     TO TB-ADSS(CPT-1)
              MOVE MVT-CP       TO TB-CP(CPT-1)
              MOVE MVT-VLL      TO TB-VLL(CPT-1)
              MOVE MVT-TPV      TO TB-TPV(CPT-1)
              MOVE MVT-PRMBS    TO TB-PRMBS(CPT-1)
              MOVE MVT-PRMT     TO TB-PRMT(CPT-1)
              MOVE MVT-TAUX     TO TB-TAUX(CPT-1)
           END-IF
           END-PERFORM
           IF CHECK = 0
             ADD 1 to CPT-MA
             MOVE '003' TO NB-MESS
             PERFORM WRITE-ERROR
           END-IF
           .
       SUPP.
           MOVE 0 TO CHECK
           PERFORM VARYING CPT-1 FROM 1 BY 1 UNTIL CPT-1 > NB-LIGNE-TAB
             IF TB-MATRICUL(CPT-1) = MVT-MATRICUL
               MOVE 1 TO CHECK
             END-IF
             IF CHECK = 1  AND CPT-1 NOT = NB-LIGNE-TAB
               MOVE LIGNE(CPT-1 + 1) TO LIGNE(CPT-1)
             END-IF
             IF CHECK = 1 AND CPT-1 = NB-LIGNE-TAB
               MOVE ALL SPACE TO LIGNE(NB-LIGNE-TAB)
               MOVE '-' TO LIGNE(NB-LIGNE-TAB)(1:5)
             END-IF
           END-PERFORM
           IF CHECK = 0
             ADD 1 to CPT-SA
             MOVE '004' TO NB-MESS
             PERFORM WRITE-ERROR
           ELSE
             ADD 1 to CPT-SW
             SUBTRACT 1 FROM NB-LIGNE-TAB
           END-IF
           .
       CREA-LIGNE.
           MOVE 0 TO CHECK
      * CHECK IF NEW DATA AS TO BE ADD AT THE END    
           IF MVT-MATRICUL > TB-MATRICUL(NB-LIGNE-TAB)
             ADD 1 TO NB-LIGNE-TAB
             MOVE MVT-MATRICUL TO TB-MATRICUL(NB-LIGNE-TAB)
             MOVE MVT-NM-PRNM  TO TB-NM-PRNM(NB-LIGNE-TAB)
             MOVE MVT-ADSS     TO TB-ADSS(NB-LIGNE-TAB)
             MOVE MVT-CP       TO TB-CP(NB-LIGNE-TAB)
             MOVE MVT-VLL      TO TB-VLL(NB-LIGNE-TAB)
             MOVE MVT-TPV      TO TB-TPV(NB-LIGNE-TAB)
             MOVE MVT-PRMBS    TO TB-PRMBS(NB-LIGNE-TAB)
             MOVE MVT-PRMT     TO TB-PRMT(NB-LIGNE-TAB)
             MOVE MVT-TAUX     TO TB-TAUX(NB-LIGNE-TAB)
             ADD 1 to CPT-CW
           ELSE
             PERFORM VARYING CPT-1 FROM 1 BY 1
                     UNTIL CPT-1 > NB-LIGNE-TAB
      * CHECK ERREUR DATA ALREADY EXIST             
               IF TB-MATRICUL(CPT-1) = MVT-MATRICUL
                 MOVE 1 TO CHECK
                 ADD 1 to CPT-CA
                 MOVE '002' TO NB-MESS
                 PERFORM WRITE-ERROR
               END-IF
      * CHECK IF THE NEW DATA AS TO BE PUT IN THE TAB 
      * BUT NOT AT THE FORLAST POSITION
               IF MVT-MATRICUL < TB-MATRICUL(CPT-1) AND
                      CHECK = 0  AND CPT-1 NOT = NB-LIGNE-TAB
                 MOVE 2 TO CHECK
                 MOVE LIGNE(CPT-1) TO LIGNE-RESV1
                 PERFORM MAJTOTABLI
               END-IF
      * CHECK IF THE NEW DATA AS TO BE PUT AT THE FORLAST POSITION 
      * OF THE TAB 
               IF MVT-MATRICUL < TB-MATRICUL(CPT-1) AND
                      CHECK = 0  AND CPT-1 = NB-LIGNE-TAB
                 MOVE LIGNE(CPT-1) TO LIGNE(CPT-1 + 1)
                 PERFORM MAJTOTABLI
                 MOVE 3 TO CHECK
               END-IF
      * MOVE THE DATA WHO COME AFTER THE NEW DATA BY 1 LIGNE 
               IF CHECK = 2 AND CPT-1 NOT = NB-LIGNE-TAB AND
                   MVT-MATRICUL NOT = TB-MATRICUL(CPT-1)
                 MOVE LIGNE(CPT-1) TO LIGNE-RESV2
                 MOVE LIGNE-RESV1  TO LIGNE(CPT-1)
                 MOVE LIGNE-RESV2  TO LIGNE-RESV1
               END-IF
               IF CHECK = 2 AND CPT-1 = NB-LIGNE-TAB
                 MOVE LIGNE(CPT-1) TO LIGNE(CPT-1 + 1)
                 MOVE LIGNE-RESV1  TO LIGNE(CPT-1)
               END-IF
             END-PERFORM
             IF CHECK = 2 OR CHECK = 3
               ADD 1 to CPT-CW
               ADD 1 TO NB-LIGNE-TAB
             END-IF
           END-IF
           .
       MAJTOTABLI.
             MOVE MVT-MATRICUL TO TB-MATRICUL(CPT-1)
             MOVE MVT-NM-PRNM  TO TB-NM-PRNM(CPT-1)
             MOVE MVT-ADSS     TO TB-ADSS(CPT-1)
             MOVE MVT-CP       TO TB-CP(CPT-1)
             MOVE MVT-VLL      TO TB-VLL(CPT-1)
             MOVE MVT-TPV      TO TB-TPV(CPT-1)
             MOVE MVT-PRMBS    TO TB-PRMBS(CPT-1)
             MOVE MVT-PRMT     TO TB-PRMT(CPT-1)
             MOVE MVT-TAUX     TO TB-TAUX(CPT-1)
           .
           
       FEEDRESP.
           PERFORM VARYING CPT-1 FROM 1 BY 1 UNTIL CPT-1 > NB-LIGNE-TAB
             MOVE TB-MATRICUL(CPT-1) TO RES-MATRICUL
             MOVE TB-NM-PRNM(CPT-1)  TO RES-NM-PRNM
             MOVE TB-ADSS(CPT-1)     TO RES-ADSS
             MOVE TB-CP(CPT-1)       TO RES-CP
             MOVE TB-VLL(CPT-1)      TO RES-VLL
             MOVE TB-TPV(CPT-1)      TO RES-TPV
             MOVE TB-PRMBS(CPT-1)    TO RES-PRMBS
             MOVE TB-PRMT(CPT-1)     TO RES-PRMT
             MOVE TB-TAUX(CPT-1)     TO RES-TAUX
             PERFORM WRITE-RESP
           END-PERFORM
           MOVE NB-LIGNE-TAB TO NB-LIGNE-FIN
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
           STRING '008 - NOMBRE D''ENREGISTREMENTS CREES ASSURES4 : ' 
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
           MOVE 'API3' TO LGN-DEB1(1:4) LGN-DEB3(1:4) LGN-DEB4(1:4)
           MOVE 'LISTE DE CONTROLE DU FICHIER MVT' TO LGN-DEB1(14:32)
           MOVE 'STATISTIQUE SUR CONTROLE DU FICHIER MOUVEMENT' TO 
                LGN-DEB3(8:45)
           MOVE 'LISTE DES ANOMALIES' TO LGN-DEB4(14:32)
           MOVE DAT TO LGN-DEB1(51:30) LGN-DEB3(51:30) LGN-DEB4(51:30)
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
       TEST-STAT-RESP.
           IF WS-RESP NOT = '00'
             DISPLAY 'ERREUR FICHIER ASSURES4 ' WS-RESP
             MOVE 16 TO RETURN-CODE
             STOP RUN
           END-IF
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
       WRITE-RESP.
           WRITE ERESP
           MOVE ALL SPACE TO ERESP
           .
       WRITE-STAT.
           WRITE ESTAT
           MOVE ALL SPACE TO ESTAT
           .
       WRITE-ERROR.
           CALL SOUSPROG3 USING NB-MESS MESS
           STRING MVT-MATRICUL ' ERREUR : ' MESS
               DELIMITED BY SIZE INTO EANO
           WRITE EANO
           .
            