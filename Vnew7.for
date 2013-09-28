C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    ×÷Õß:       Àî  ÖÇ                                    *
C*    ³ÌÐòÃû:                                               *Á
C*               MAT                                       *
C*    ¹¦ÄÜ:      Ý  ¡¡                                      *
C*               ´¦Àí²ÄÁÏÊý¾Ý                                *
C*    ±»µ÷ÓÃ³ÌÐò:              ¡                             *
C*               MAT0                                      *
C*    µ÷ÓÃ³ÌÐò:              ¡¡                              *
C*                ÎÞ                                       *
C*    ¶ÁÈ¡ÎÄ¼þ:               ¡¡                             *
C*                ÎÞ                                       *
c*    Ð´ÈëÎÄ¼þ:                                       ¡¡     *
C*                ÎÞ                                       *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    ÐÞ¸Ä»òÔö¼Ó                                         ¡¡  *
c*    ÐÞÔöÕß:                                             ¡¡ *
c*    ÐÞÔöÈÕÆÚ:                                           ¡¡ *
c*    ÐÞÔöÄÚÈÝ:                                           ¡¡ *
c*                                                         *
c*                                                         *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       SUBROUTINE MAT(RMATS,RMATE,RMATA,IS,IE,IA,TJ,ST,ET,ALF)
	IMPLICIT DOUBLE PRECISION(
     +A,B,C,D,E,F,G,H,O,P,Q,R,S,T,U,V,W,X,Y,Z)
       DIMENSION RMATS(IS,2),RMATE(IE,2),RMATA(IA,2)
                NS=IS-1
                NE=IE-1
                NA=IA-1
                DO 100 IMAT=1,NS
                JMAT=IMAT+1
                T1=RMATS(IMAT,1)
                T2=RMATS(JMAT,1)
                IF(TJ.GT.T1.AND.TJ.LE.T2)THEN
                  SIGMI=RMATS(IMAT,2)
                  SIGMJ=RMATS(JMAT,2)
                  SIGMD=((SIGMJ-SIGMI)/(T2-T1))*(TJ-T1)
                  ST=SIGMI+SIGMD
                  GOTO 110
                ELSE IF(TJ.LE.T1)THEN
                  ST=RMATS(IMAT,2)
                  GOTO 110
                END IF
100             CONTINUE
110             CONTINUE

                DO 200 IMAT=1,NE
                JMAT=IMAT+1
                T1=RMATE(IMAT,1)
                T2=RMATE(JMAT,1)
                IF(TJ.GT.T1.AND.TJ.LE.T2)THEN
                  EI=RMATE(IMAT,2)
                  EJ=RMATE(JMAT,2)
                  ED=((EJ-EI)/(T2-T1))*(TJ-T1)
                  ET=EI+ED
                  GOTO 210
                ELSE IF(TJ.LE.T1)THEN
                  ET=RMATE(IMAT,2)
                  GOTO 210
                END IF
200             CONTINUE
210             CONTINUE
                DO 300 IMAT=1,NA
                JMAT=IMAT+1
                T1=RMATA(IMAT,1)
                T2=RMATA(JMAT,1)
                IF(TJ.GT.T1.AND.TJ.LE.T2)THEN
                  AI=RMATA(IMAT,2)
                  AJ=RMATA(JMAT,2)
                  AD=((AJ-AI)/(T2-T1))*(TJ-T1)
                  ALF=AI+AD
                  GOTO 310
                ELSE IF(TJ.LE.T1)THEN
                  ALF=RMATA(IMAT,2)
                  GOTO 310
                END IF
300             CONTINUE
310             CONTINUE

         RETURN
      END


C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    ×÷Õß:       Àî  ÖÇ                                    *
C*    ³ÌÐòÃû:                                               *Á
C*               MAT0                                       *
C*    ¹¦ÄÜ:      Ý  ¡¡                                      *
C*               ²éÈ¡ºÍ´¦Àí²ÄÁÏÊý¾Ý                           *
C*    ±»µ÷ÓÃ³ÌÐò:              ¡                             *
C*                DATAIN                                   *
C*    µ÷ÓÃ³ÌÐò:              ¡¡                              *
C*                MAT                                       *
C*    ¶ÁÈ¡ÎÄ¼þ:               ¡¡                             *
C*                ÎÞ                                       *
c*    Ð´ÈëÎÄ¼þ:                                       ¡¡     *
c*            Í¨µÀºÅ      ÄÚÈÝ                           ¡¡  *
c*            1          Êä³ö·ÖÎöÊý¾Ý                        *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    ÐÞ¸Ä»òÔö¼Ó                                         ¡¡  *
c*    ÐÞÔöÕß:                                             ¡¡ *
c*    ÐÞÔöÈÕÆÚ:                                           ¡¡ *
c*    ÐÞÔöÄÚÈÝ:                                           ¡¡ *
c*                                                         *
c*                                                         *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


        SUBROUTINE MAT0(DAT1,S,T0,TJ,XM0,XMT,ALF,E0,ET)
	IMPLICIT DOUBLE PRECISION(
     +A,B,C,D,E,F,G,H,O,P,Q,R,S,T,U,V,W,X,Y,Z)
        CHARACTER*17 DAT1

        COMMON/MATGA2/ RMAT21S1(14,2),RMAT21E(7,2),RMAT21A(6,2),
     +RMAT21S2(14,2),RMAT21S3(14,2),
     +RMAT22S1(24,2),RMAT22E(7,2),RMAT22A(6,2),
     +RMAT22S2(24,2),RMAT22S3(24,2),
     +RMAT31S(4,2),RMAT32S(10,2),RMAT33S(10,2),
     +RMAT31E(8,2),RMAT32E(11,2),RMAT31A(10,2),
     +RMAT11S(16,2),RMAT11E(18,2),RMAT11A(17,2),
     +RMAT12S(19,2),RMAT12E(21,2),RMAT12A(13,2),
     +RMAT13S(23,2),RMAT13E(31,2),RMAT13A(30,2),
     +RMAT14S(5,2),RMAT14E(7,2),RMAT14A(6,2),
     +RMAT15S(11,2),RMAT15E(13,2),RMAT15A(12,2)

      COMMON/MATGA3/I11S,I11E,I11A,I12S,I12E,I12A,I13S,I13E,I13A,
     +              I14S,I14E,I14A,I15S,I15E,I15A
      COMMON/MATGA1/I21S,I21E,I21A,I22S,I31S,I31E,I31A,I32S,I32E

      COMMON/MATFLE/RMATS(50,2),RMATE(50,2),RMATA(50,2),IS,IE,IA
                IF((INDEX(DAT1,'ST45.8')).EQ.1)then
                 IF(S.LE.16)THEN
                  CALL MAT(RMAT21S1,RMAT21E,RMAT21A,I21S,I21E,I21A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT21S1,RMAT21E,RMAT21A,I21S,I21E,I21A,
     +                     T0,XM0,E0,ALF0)
                 ELSE IF(S.GT.16.AND.S.LE.40)THEN
                  CALL MAT(RMAT21S2,RMAT21E,RMAT21A,I21S,I21E,I21A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT21S2,RMAT21E,RMAT21A,I21S,I21E,I21A,
     +                     T0,XM0,E0,ALF0)
                 ELSE IF(S.GT.40.AND.S.LE.60)THEN
                  CALL MAT(RMAT21S3,RMAT21E,RMAT21A,I21S,I21E,I21A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT21S3,RMAT21E,RMAT21A,I21S,I21E,I21A,
     +                     T0,XM0,E0,ALF0)
                 ELSE
                  CALL MAT(RMAT21S1,RMAT21E,RMAT21A,I21S,I21E,I21A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT21S1,RMAT21E,RMAT21A,I21S,I21E,I21A,
     +                     T0,XM0,E0,ALF0)
                        WRITE(1,*)'WARNING !!!!!! THICK GREATER THAN 80'
                 END IF
                ELSE IF((INDEX(DAT1,'10CRMO910')).EQ.1)then
                 IF(S.LE.40)THEN
                  CALL MAT(RMAT22S1,RMAT22E,RMAT22A,I22S,I21E,I21A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT22S1,RMAT22E,RMAT22A,I22S,I21E,I21A,
     +                     T0,XM0,E0,ALF0)
                 ELSE IF(S.GT.40.AND.S.LE.60)THEN
                  CALL MAT(RMAT22S2,RMAT22E,RMAT22A,I22S,I21E,I21A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT22S2,RMAT22E,RMAT22A,I22S,I21E,I21A,
     +                     T0,XM0,E0,ALF0)
                 ELSE IF(S.GT.60.AND.S.LE.80)THEN
                  CALL MAT(RMAT22S3,RMAT22E,RMAT22A,I22S,I21E,I21A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT22S3,RMAT22E,RMAT22A,I22S,I21E,I21A,
     +                     T0,XM0,E0,ALF0)
                 ELSE
                  CALL MAT(RMAT22S1,RMAT22E,RMAT22A,I22S,I21E,I21A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT22S1,RMAT22E,RMAT22A,I22S,I21E,I21A,
     +                       T0,XM0,E0,ALF0)
                 WRITE(1,*)'WARNING !!!!!! WALL THICK GREATER THAN 60'
                 END IF
                ELSE IF((INDEX(DAT1,'10')).EQ.1)THEN
                  CALL MAT(RMAT11S,RMAT11E,RMAT11A,I11S,I11E,I11A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT11S,RMAT11E,RMAT11A,I11S,I11E,I11A,
     +                     T0,XM0,E0,ALF0)
                ELSE IF((INDEX(DAT1,'20G')).EQ.1)THEN
                  CALL MAT(RMAT12S,RMAT12E,RMAT12A,I12S,I12E,I12A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT12S,RMAT12E,RMAT12A,I12S,I12E,I12A,
     +                     T0,XM0,E0,ALF0)
                ELSE IF((INDEX(DAT1,'20')).EQ.1)THEN
                  CALL MAT(RMAT12S,RMAT12E,RMAT12A,I12S,I12E,I12A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT12S,RMAT12E,RMAT12A,I12S,I12E,I12A,
     +                     T0,XM0,E0,ALF0)
                ELSE IF((INDEX(DAT1,'12CR1MOV')).EQ.1)THEN
                  CALL MAT(RMAT13S,RMAT13E,RMAT13A,I13S,I13E,I13A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT13S,RMAT13E,RMAT13A,I13S,I13E,I13A,
     +                     T0,XM0,E0,ALF0)
                ELSE IF((INDEX(DAT1,'16MNG')).EQ.1)THEN
                  CALL MAT(RMAT15S,RMAT15E,RMAT15A,I15S,I15E,I15A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT15S,RMAT15E,RMAT15A,I15S,I15E,I15A,
     +                     T0,XM0,E0,ALF0)
                ELSE IF((INDEX(DAT1,'A106B')).EQ.1)THEN
                  CALL MAT(RMAT31S,RMAT31E,RMAT31A,I31S,I31E,I31A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT31S,RMAT31E,RMAT31A,I31S,I31E,I31A,
     +                     T0,XM0,E0,ALF0)
                ELSE IF((INDEX(DAT1,'A335P11')).EQ.1)THEN
                  CALL MAT(RMAT32S,RMAT32E,RMAT31A,I32S,I32E,I31A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT32S,RMAT32E,RMAT31A,I32S,I32E,I31A,
     +                     T0,XM0,E0,ALF0)
                ELSE IF((INDEX(DAT1,'A335P22')).EQ.1)THEN
                  CALL MAT(RMAT33S,RMAT32E,RMAT31A,I32S,I32E,I31A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT33S,RMAT32E,RMAT31A,I32S,I32E,I31A,
     +                     T0,XM0,E0,ALF0)
                ELSE IF((INDEX(DAT1,'A3F')).EQ.1)THEN
                  CALL MAT(RMAT14S,RMAT14E,RMAT14A,I14S,I14E,I14A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT14S,RMAT14E,RMAT14A,I14S,I14E,I14A,
     +                     T0,XM0,E0,ALF0)
                ELSE IF((INDEX(DAT1,'A3')).EQ.1)THEN
                  CALL MAT(RMAT14S,RMAT14E,RMAT14A,I14S,I14E,I14A,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMAT14S,RMAT14E,RMAT14A,I14S,I14E,I14A,
     +                     T0,XM0,E0,ALF0)
                ELSE
              WRITE(1,*)'SORRY!!! THERE IS NO THIS MATERIAL',DAT1
              WRITE(*,*)'SORRY!!! THERE IS NO THIS MATERIAL',DAT1
              write(*,*)'Note: Only character and number is legal.'
              write(*,*)'For example:'
              WRITE(*,*)' 10CrMo910 ','------','Error'
              write(*,*)'"10CRMO910"','------','Error'
              WRITE(*,*)' 10CRMO910 ','------','Correct'
                  CALL MAT(RMATS,RMATE,RMATA,IS,IE,IA,
     +                     TJ,XMT,ET,ALF)
                  CALL MAT(RMATS,RMATE,RMATA,IS,IE,IA,
     +                     T0,XM0,E0,ALF0)

                END IF
C ONLY INTEGE CAN BE USE
                 IF((INDEX(DAT1,'10')).EQ.1.OR.
     +              (INDEX(DAT1,'20')).EQ.1.OR.
     +              (INDEX(DAT1,'12CR1MOV')).EQ.1.OR.
     +              (INDEX(DAT1,'A3')).EQ.1.OR.
     +              (INDEX(DAT1,'16MNG')).EQ.1)THEN
                 XMT=AINT(XMT)
                 XM0=AINT(XM0)
                 END IF
               RETURN
               END

