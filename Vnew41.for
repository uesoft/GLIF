c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    ×÷Õß:             Àî  ÖÇ                             *
c*    ³ÌÐòÃû:                                              *
c*                            VECTOR                       *
c*    ¹¦ÄÜ:                                                *
c*                            ¾ØÕóÏòÁ¿Êý¾Ý´¦Àíó            *
c*    ±»µ÷ÓÃ³ÌÐò:                                          *
c*                            STRUCTK                      *
c*    ³ÌÐòÈë¿Ú:                                            *
C*                             ENTRY XLCC---ÏòÁ¿×ª´¢       *
C*                             ENTRY JZAD---¾ØÕóÏà¼Ó       *
C*                             ENTRY JZCH---¾ØÕóÏà³Ë       *
C*                             ENTRY JZZF---¾ØÕóÖÃ¸º       *
C*                             ENTRY JZXL---¾ØÕó³ËÒÔÏòÁ¿   *
C*                             ENTRY JZZZ---¾ØÕó×ªÖÃ       *
C*                             ENTRY XLAD---ÏòÁ¿Ïà¼Ó       *
C*                             ENTRY XLZF---ÏòÁ¿ÖÃ¸º       *
C*                             ENTRY JZCC---¾ØÕóÏà¼Ó       *
C*                             ENTRY SCXL---Êý³ËÏòÁ¿       *
C*                             ENTRY JZQN---¾ØÕóÇóÄæ       *
c*    µ÷ÓÃ³ÌÐò:                                            *
c*                            ÎÞ                           *
c*    ¶ÁÈ¡ÎÄ¼þ:                                            *
c*                            ÎÞ                           *
c*    Ð´ÈëÎÄ¼þ:                                            *
c*                            ÎÞ                           *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	    SUBROUTINE VECTOR(A,B)
	    IMPLICIT DOUBLE PRECISION(A-H,O-Z)
	    DIMENSION A(6),B(6),C(6),D(6)
	    DIMENSION AA(6,6),BB(6,6),CC(6,6)

	    DO 30 I=1,6
30            B(I)=A(I)
	    RETURN
	    
	    ENTRY XLCC(A,B)
	    DO 31 I=1,6
31            B(I)=A(I)
	    RETURN
	    
	    ENTRY JZAD(AA,BB,CC)
	    DO 40 I=1,6
	    DO 40 J=1,6
40            CC(I,J)=AA(I,J)+BB(I,J)
	    RETURN

	    ENTRY JZCH(AA,BB,CC)
	    DO 60 I=1,6
	    DO 60 J=1,6
	       X=0.
	       DO 50 K=1,6
50                          X=X+AA(I,K)*BB(K,J)
60            CC(I,J)=X
	    RETURN


	    ENTRY JZZF(AA,BB)
	    DO 70 I=1,6
	    DO 70 J=1,6
70            BB(I,J)=-AA(I,J)
	    RETURN


	    ENTRY JZXL (AA,B,C)
	    DO 90 I=1,6
	       X=0.
	       DO 80 K=1,6
80                          X=X+AA(I,K)*B(K)
90            C(I)=X
	    RETURN


	    ENTRY JZZZ(AA,BB)
	    DO 100 I=1,6
	    DO 100 J=1,6
100         BB(I,J)=AA(J,I)
	    RETURN


	    ENTRY XLAD(A,B,C)
	    DO 110 I=1,6
110         C(I)=A(I)+B(I)
	    RETURN


	    ENTRY XLZF(A,B)
	    DO 120 I=1,6
120     B(I)=-A(I)
	    RETURN


	    ENTRY JZCC(AA,BB)
	    DO 130 I=1,6
	    DO 130 J=1,6
130         BB(I,J)=AA(I,J)
	    RETURN


	    ENTRY SCXL(S,C,D)
	    DO 140 I=1,6
140         D(I)=S*C(I)
	    RETURN


	    ENTRY JZQN(AA,BB)
	    DO 1400 I=1,6
		IF(AA(I,I).EQ.0.0)GOTO 1900
	    DO 1400 J=1,6
1400        BB(I,J)=0.
	    BB(1,1)=1./AA(1,1)
	    DO 1800 M=2,6
	     K=M-1
	     EK=AA(M,M)
	     DO 1500 I=1,K
	     DO 1500 J=1,K
1500            EK=EK-AA(M,I)*BB(I,J)*AA(J,M)
	     WRITE(1,*)EK
	     BB(M,M)=1./EK
	     DO 1700 I=1,K
	     DO 1600 J=1,K
1600           BB(I,M)=BB(I,M)-BB(I,J)*AA(J,M)/EK
1700           BB(M,I)=BB(I,M)
	    DO 1800 I=1,K
	    DO 1800 J=1,K
1800           BB(I,J)=BB(I,J)+BB(I,M)*BB(M,J)*EK
	    

	    RETURN
1900        WRITE(11,200)
200         FORMAT('ERROR,DELETED WHILE JZQN')
	    RETURN



999     RETURN
	    END
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    ×÷Õß:             Àî  ÖÇ                             *
c*    ³ÌÐòÃû:                                              *
c*                             CPRINT                      *
c*    ¹¦ÄÜ:                                                *
c*                           í ´òÓ¡Êä³öÎÄ¼þÌâÍ·            *
c*    ±»µ÷ÓÃ³ÌÐò:                                          *
c*                                                         *
c*    µ÷ÓÃ³ÌÐò:                                            *
c*                            ÎÞ                           *
c*    ¶ÁÈ¡ÎÄ¼þ:                                            *
c*                            ÎÞ                           *
c*    Ð´ÈëÎÄ¼þ:                                            *
c*                            1                            *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C------------------------------P R I N T---------------------------
	    SUBROUTINE CPRINT(N)
	    COMMON/TIM/LTIM1,LTIM2,ABC
	    CHARACTER LTIM1*8,LTIM2*9,ABC*9

	    IF(N.EQ.0)NPAGE=0
	    NPAGE=NPAGE+1
	    WRITE(1,1111)
	    WRITE(1,2222)NPAGE,LTIM1,LTIM2
	    WRITE(1,3333)
1111    FORMAT('
     +')
2222    FORMAT(2X,'GLIF--V/3.1',16X,
     +  'PIPING STRESS ANALYSIS',21X,
     +  'PAGE NO:',I4,/,2X,'SDGJ 6-90',
     +  48X,'TIME: ',A8,1X,A9)
3333    FORMAT
     1('---------------------------------',
     + '---------------------------------',
     + '------------------')

	    RETURN
	    END



C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    ×÷Õß:             Àî  ÖÇ                             *
C*    ³ÌÐòÃû:                                              *Á

C*                           NUMP                          *
C*    ¹¦ÄÜ:												 *
C*                           ÖØÅÅ½ÚµãºÅ  ¡¡                *
C*                                                         *
C*    ±»µ÷ÓÃ³ÌÐò:                          ¡               *
C*                           SANS                          *
C*    µ÷ÓÃ³ÌÐò:                          ¡¡                *
c*                            ÎÞ                           *
C*    ¶ÁÈ¡ÎÄ¼þ:                           ¡¡               *
c*                            ÎÞ                           *
c*    Ð´ÈëÎÄ¼þ:                                            *
c*                        Í¨µÀºÅ            ÄÚÈÝ           *
c*                        1                Êä³ö·ÖÎöÊý¾Ý    *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	    SUBROUTINE NUMP(JCTAB,KQR,KQL,ISH,IMH,
     +                       IDJX,JSIX,NF0,NCDH,JYS,NFZ)
	    IMPLICIT INTEGER*4 (K)
	    INTEGER*4 MAX,I
	    COMMON/SCDS/INFZ,NDD,JD(50)
	    SAVE/SCDS/

	    DIMENSION JCTAB(1000),KQR(1000),KQL(1000)
	    DIMENSION ISH(NFZ),IMH(NFZ),IDH(2000)
	    DIMENSION IDJX(6000),JYS(6000)
	    LOGICAL NEW

	    NJH=0
	    NBR=0
	    NCDH=0
	    NDH=0
	    
	    NEW=.TRUE.
	    
	    J=0
	    DO 5 I=1,2*NFZ,2
	    J=J+1
	    IDH(I)=ISH(J)
5             IDH(I+1)=IMH(J)
	    DO 10 I=1,1000
10            KQR(I)=I


	    DO 60 I=1,2*NFZ
		
		NDH=IDH(I)
	    DO 15 J=1,NDD
	     IF(NDH.EQ.JD(J))THEN
	      LDD=0
	      GOTO 40
	     END IF  
15          CONTINUE
	     LDD=1
	     IF(NCDH.EQ.0)GOTO 30
	    DO 20 NJ=1,NCDH
	     IF(JCTAB(NJ).EQ.NDH)GOTO 40
20          CONTINUE
30          NCDH=NCDH+1
	      JCTAB(NCDH)=NDH
	      NJ=NCDH

40           IF(.NOT.NEW)GOTO 50
	      NBR=NBR+1
	      IF(LDD.EQ.0)JS=0
	      IF(LDD.EQ.1)JS=NJ             
	      NEW=.FALSE.
	      GOTO 60

50           CONTINUE
		
	     IF(LDD.EQ.0)JE=0

	     IF(LDD.EQ.1)JE=NJ
	     IF(JS.GT.0.AND.JE.GT.0.AND.JS.GT.JE.AND.KQR(JS).GT.JE)
     1       KQR(JS)=JE
	     IF(JS.GT.0.AND.JE.GT.0.AND.JE.GT.JS.AND.KQR(JE).GT.JS)
     1       KQR(JE)=JS
	     ISH(NBR)=JS
	     IMH(NBR)=JE
	     NEW=.TRUE.
60           CONTINUE


	    DO 75 I=1,NCDH
75             KQR(I)=KQR(I)-1
	    KQL(1)=0
	    DO 80 I=2,NCDH
	       KQL(I)=KQL(I-1)+KQR(I)
80            continue
	    JSIX=6*NCDH
	    KSIX=JSIX

	    MAX=(KSIX*(KSIX+1))/2
	    NF0=MAX-36*KQL(NCDH)

	    IDJX(1)=1
	    JYS(1)=0
	    DO 90 I=2,KSIX
	      ISUP=(I-1)/6+1
	      IDJX(I)=I*(I+1)/2
	      IF(ISUP.EQ.1)GOTO 90
	      IDJX(I)=I*(I+1)/2-36*KQL(ISUP-1)-
     +                 6*KQR(ISUP)*(I-6*(ISUP-1))
	    
90          JYS(I)=IDJX(I)-IDJX(I-1)-1
	    IF(NF0.GT.150000)WRITE(1,*)'PLEASE REPAIR  MMMM IN GPIPE'
	    IF(NF0.GT.150000)WRITE(1,*)'UNITS IS TOO MANY '
	    IF(NF0.GT.150000)stop
	    RETURN
	    END

C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    ×÷Õß:             Àî  ÖÇ                             *
C*    ³ÌÐòÃû:                                              *Á

C*                            SANA                         *
C*    ¹¦ÄÜ:                         Ý  ¡¡                  *
C*                            °´¹¤¿ö×éÖ¯³ÌÐò½øÐÐÓ¦Á¦·ÖÎö   *
C*    ±»µ÷ÓÃ³ÌÐò:                          ¡               *
C*                            VNEW11                       *
C*    µ÷ÓÃ³ÌÐò:                          ¡¡                *
C*                            NUMP
C*                            STRUCTK
C*                            RIGHT
C                             BOUND0
C*                            BOUND4
C*                            BOUND1
C*                            SPRING
C*                            BOUND2
C*                            BOUND3
C*                            DIV
C*                            SOLVE
C*                            ANSELE
C*    ¶ÁÈ¡ÎÄ¼þ:                           ¡¡              *
c*                        Í¨µÀºÅ            ÄÚÈÝ  ¡¡                                                   *
c*                        11               Ô­Ê¼µ¯»ÉÊý¾Ý                 í  ¡¡¡¡             *
c*                        12               ×Ü¸Õ¶È¾ØÕó                                                *
c*    Ð´ÈëÎÄ¼þ:                                                                           ¡¡     *
c*                            ÎÞ                                                                            *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    ÐÞ¸Ä»òÔö¼Ó                                                                             ¡¡  *
c*    ÐÞÔöÕß:                                                                                       ¡¡ *
c*    ÐÞÔöÈÕÆÚ:                                                                                     ¡¡ *
c*    ÐÞÔöÄÚÈÝ:                                                                                     ¡¡ *
c*                                                                                                               *
c*                                                                                                               *
c*                                                                                                               *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*****************THIS IS STATC***************
	    SUBROUTINE SANS(NFZ1,ISH,IMH,LX,ITLX)
	    IMPLICIT DOUBLE PRECISION(A-H,O-Z)
	    INTEGER*4 KQR,KQL
	    COMMON/NXHXS/FXHXS,IZBXZ
	    COMMON/GK/IGK,IFJ5,IFJ6,IFJ7,IFJ8,IFJ9,IFJ10,IFJ11,IFJ12
	    COMMON/PRIN/IPRIN,Iprin1,Isz,ILTD0,izh
	    COMMON/IZDJ/IZD(10),zdfc
	    COMMON/KZKG/kzdw,MTH,NTH,THXS
	    COMMON/SPRI/IDW,IRD,TKZ,KTH,TK(5,50),TMINSZ(5,50)
	    common/szsz/rsz(6000)
	    common/disk/lth,paz(6000)
         DIMENSION KQR(1000),KQL(1000),JRD(5),jdw(5),
     +                ISH(NFZ1),IMH(NFZ1),JCTAB(1000)
	    DIMENSION ZK(2000000),DJX(6000),JYS(6000),psj(6000),dsj(6000)
	    DIMENSION IDJX(6000),LX(NFZ1),ITLX(NFZ1),ITH(1000)
	    DIMENSION THL(1000),FORCE(6000),PSL(6000),JKTH(5)
	    DIMENSION D(6000),JSH(1000),JMH(1000),PSZ(6000)
	    CHARACTER*20 TBZ
C------------SPRING DATA IN---------------------
	    tmin=0.3
	    tmax=0.8
	lth=0
	if(mth.eq.9)then
	mth=3
	lth=9
	end if
            IF(MTH.EQ.3)THEN
       idw=1
       ird=0
		tkz=75
		kth=20
		JKTH(3)=KTH
       JRD(3)=IRD
       jdw(3)=idw
       tk(3,1)=1.5
       tk(3,2)=0.974
       tk(3,3)=0.636
       tk(3,4)=0.412
       tk(3,5)=0.268
       tk(3,6)=0.2
       tk(3,7)=0.15
       tk(3,8)=0.113
       tk(3,9)=0.0845
       tk(3,10)=0.0636
       tk(3,11)=0.0476
       tk(3,12)=0.0357
       tk(3,13)=0.0268
       tk(3,14)=0.02
       tk(3,15)=0.015
       tk(3,16)=0.0113
       tk(3,17)=0.00845
       tk(3,18)=0.00636
       tk(3,19)=0.00476
       tk(3,20)=0.00357
       TMINSZ(3,1)=30
       TMINSZ(3,2)=30
       TMINSZ(3,3)=30
       TMINSZ(3,4)=30
       TMINSZ(3,5)=30
       TMINSZ(3,6)=35
       TMINSZ(3,7)=35
       TMINSZ(3,8)=35
       TMINSZ(3,9)=35
       TMINSZ(3,10)=35
       TMINSZ(3,11)=35
       TMINSZ(3,12)=35
       TMINSZ(3,13)=35
       TMINSZ(3,14)=35
       TMINSZ(3,15)=35
       TMINSZ(3,16)=35
       TMINSZ(3,17)=35
       TMINSZ(3,18)=35
       TMINSZ(3,19)=35
       TMINSZ(3,20)=35
            ELSE IF(MTH.EQ.2)THEN
		idw=1
		ird=0
		tkz=70
		kth=16
		JRD(2)=IRD
       jdw(2)=idw
       JKTH(2)=KTH
       TK(2,1)=0.4895
       TK(2,2)=0.2778
       TK(2,3)=0.1759
       TK(2,4)=0.1237
       TK(2,5)=0.0914
       TK(2,6)=0.0702
       TK(2,7)=0.054
       TK(2,8)=0.0428
       TK(2,9)=0.0318
       TK(2,10)=0.0247
       TK(2,11)=0.0208
       TK(2,12)=0.0178
       TK(2,13)=0.0147
       TK(2,14)=0.0126
       TK(2,15)=0.0108
       TK(2,16)=0.0088
       TMINSZ(2,1)=30
       TMINSZ(2,2)=30
       TMINSZ(2,3)=30
       TMINSZ(2,4)=30
       TMINSZ(2,5)=30
       TMINSZ(2,6)=30
       TMINSZ(2,7)=30
       TMINSZ(2,8)=30
       TMINSZ(2,9)=30
       TMINSZ(2,10)=30
       TMINSZ(2,11)=30
       TMINSZ(2,12)=30
       TMINSZ(2,13)=30
       TMINSZ(2,14)=30
       TMINSZ(2,15)=30
       TMINSZ(2,16)=30
            ELSE IF(MTH.EQ.1)THEN
		idw=1
		ird=0
		tkz=70
		kth=10
		JRD(1)=IRD
       jdw(1)=idw
		JKTH(1)=KTH
		TK(1,1)=0.722
		TK(1,2)=0.355
		TK(1,3)=0.24
		TK(1,4)=0.136
		TK(1,5)=0.0859
		TK(1,6)=0.0606
		TK(1,7)=0.0448
		TK(1,8)=0.0342
		TK(1,9)=0.0289
		TK(1,10)=0.0208
		TMINSZ(1,1)=30
		TMINSZ(1,2)=30
		TMINSZ(1,3)=30
		TMINSZ(1,4)=30
		TMINSZ(1,5)=30
		TMINSZ(1,6)=30
		TMINSZ(1,7)=30
		TMINSZ(1,8)=30
		TMINSZ(1,9)=30
		TMINSZ(1,10)=30
            else IF(MTH.EQ.4)THEN
		idw=0
		ird=1
		tkz=80
		kth=25
		JKTH(4)=KTH
		JRD(4)=IRD
       jdw(4)=idw
		tk(4,1)=3.354
		tk(4,2)=4.472
		tk(4,3)=6.159
		tk(4,4)=7.796
		tk(4,5)=10.817
		tk(4,6)=14.690
		tk(4,7)=19.613
		tk(4,8)=26.900
		tk(4,9)=36.206
		tk(4,10)=48.994
		tk(4,11)=63.449
		tk(4,12)=87.152
		tk(4,13)=117.877
		tk(4,14)=149.552
		tk(4,15)=202.018
		tk(4,16)=251.150
		tk(4,17)=321.856
		tk(4,18)=451.304
		tk(4,19)=634.886
		tk(4,20)=831.118
		tk(4,21)=1108.157
		tk(4,22)=1442.566
		tk(4,23)=1838.756
		tk(4,24)=2278.096
		tk(4,25)=2860.320
		TMINSZ(4,1)=35
		TMINSZ(4,2)=35
       TMINSZ(4,3)=35
		TMINSZ(4,4)=35
		TMINSZ(4,5)=35
		TMINSZ(4,6)=35
		TMINSZ(4,7)=35
		TMINSZ(4,8)=35
		TMINSZ(4,9)=35
		TMINSZ(4,10)=35
		TMINSZ(4,11)=35
		TMINSZ(4,12)=35
		TMINSZ(4,13)=35
		TMINSZ(4,14)=35
		TMINSZ(4,15)=35
		TMINSZ(4,16)=35
		TMINSZ(4,17)=35
		TMINSZ(4,18)=35
		TMINSZ(4,19)=35
		TMINSZ(4,20)=35
		TMINSZ(4,21)=35
		TMINSZ(4,22)=35
		TMINSZ(4,23)=35
		TMINSZ(4,24)=35
		TMINSZ(4,25)=35
            else

	     IF(MTH.EQ.6)THEN
	       OPEN(UNIT=11,FILE='SP16.DB',STATUS='OLD')
	     else IF(MTH.EQ.7)THEN
	       OPEN(UNIT=11,FILE='SP10.DB',STATUS='OLD')
	     ELSE
	       OPEN(UNIT=11,FILE='SPRING.DB',STATUS='OLD')
	     END IF
	     
	     read(11,1001)tbz
1001                FORMAT(A20)
          READ(11,*)IDW
          read(11,*)IRD
          READ(11,*)TKZ
          READ(11,*)KTH
          jdw(5)=idw
          JRD(5)=IRD
          JKTH(5)=KTH
          READ(11,*,END=1002)(TK(5,I),TMINSZ(5,I),I=1,KTH)
1002                        CONTINUE
          CLOSE(11)
          end if
	    
	    TKZ=TKZ/10.
	    DO 1004 J=1,5
	    DO 1003 I=1,50
	    TMINSZ(J,I)=TMINSZ(J,I)/10
1003        CONTINUE
1004        CONTINUE
	     DO 1011 J=1,5
	     DO 1010 I=1,50
	       IF(JRD(J).NE.0.AND.TK(J,I).NE.0)TK(J,I)=1./TK(J,I)
	       TK(J,I)=TK(J,I)/10.
              IF(jDW(j).NE.1)TK(J,I)=TK(J,I)*9.807
1010                         CONTINUE
1011                         CONTINUE
C------------------------------------
	    NLDYS=0
		IFD=1
		NFDS=1

		IQQQQ=0
		IF(NFDS.EQ.1)NFZ=NFZ1
		DO 50 I=1,NFZ
		JSH(I)=ISH(I)
		JMH(I)=IMH(I)
50                          CONTINUE
	    CALL NUMP(JCTAB,KQR,KQL,JSH,JMH,IDJX,JSIX,NF0,NCDH,JYS,NFZ)
	    IS=0
	    DO 11 J=1,NCDH
	    ITH(J)=0
11            THL(J)=0.

c  water load  igk=100              
	    igk=100

	    CALL STRUCTK(NF0,NCDH,NFZ,KQR,KQL,ZK,JSH,JMH)
	    CALL RIGHT(JSIX,NFZ,NF0,FORCE,JSH,JMH,IDJX,ZK)
	    CALL BOUND0(JSIX,NF0,NFZ,LX,ZK,JMH,IDJX,ncdh,jctab)
	    CALL DIV(NF0,JSIX,DJX,JYS,IDJX,ZK)
	    CALL SOLVE(NF0,JSIX,DJX,JYS,IDJX,FORCE,D,ZK)
	    CALL ANSELE(IFD,JSH,JMH,NFZ,JSIX,D,
     +                        EP2,DJX,PSZ,NCDH,JCTAB,LX,ith,psl,thl)

	    do 9 i=1,jsix
	     rsz(i)=djx(i)
9     continue
	    JJ=0
	    if(iltd0.lt.1)then
	    jgk=1
	    else if(iltd0.ge.1)then
	    jgk=0
	    end if
	    
	    DO 10 IGK=jgk,10
c	    WRITE(*,*)'                               CASE:    ',IGK
	    IF(IGK.EQ.5.AND.IFJ5.EQ.0)GOTO 10
	    IF(IGK.EQ.6.AND.IFJ6.EQ.0)GOTO 10
	    IF(IGK.EQ.7.AND.IFJ7.EQ.0)GOTO 10
	    IF(IGK.EQ.8.AND.IFJ8.EQ.0)GOTO 10
	    IF(IGK.EQ.9.AND.IFJ9.EQ.0)GOTO 10
	    IF(IGK.EQ.10.AND.IFJ10.EQ.0)GOTO 10

		IXH=0
		IF(((IGK.EQ.1.OR.IGK.EQ.3).AND.ILTD0.LT.1).OR.
     +         ((IGK.EQ.0.OR.IGK.EQ.3).AND.ILTD0.GE.1).OR.
     +         IGK.EQ.5.OR.IGK.EQ.7.OR.IGK.EQ.9)CALL
     +         STRUCTK(NF0,NCDH,NFZ,KQR,KQL,ZK,JSH,JMH)
15                          continue
		REWIND 12
		
	    do 876 i=1,nf0
	     READ(12)ZK(I)
876                         continue
			 
C                           IF(IXH.EQ.0)
	     CALL RIGHT(JSIX,NFZ,NF0,FORCE,JSH,JMH,IDJX,ZK)
	     CALL BOUND0(JSIX,NF0,NFZ,LX,ZK,JMH,IDJX,ncdh,jctab)
	     IF(IGK.EQ.7)THEN
	      CALL BOUND4(JSIX,djx,force)
	     ELSE IF(IGK.EQ.9)THEN
	      CALL BOUND1(NF0,NFZ,JSIX,JMH,ZK,IDJX,LX)
	     ELSE IF(((IGK.EQ.4.OR.IGK.EQ.3).AND.ILTD0.LT.1).OR.
     +               ((IGK.EQ.2.OR.IGK.EQ.3).AND.ILTD0.GE.1))then
	      CALL SPRING(NF0,NFZ,
     +              NCDH,JSIX,LX,JMH,DJX,D,
     +              FORCE,THL,ITH,ZK,IDJX,IS,IXH,JCTAB,ITLX,JKTH)
	     ELSE IF(IGK.EQ.5.OR.IGK.EQ.6.OR.IGK.EQ.8)THEN
	      CALL BOUND2(NF0,NFZ,NCDH,JSIX,JMH,ITH,ZK,IDJX,TK,ITLX)
	     END IF
      if(izd(igk).gt.0)call BOUND3(jsix,nf0,nfz,lx,zk,jmh,idjx)
	    CALL DIV(NF0,JSIX,DJX,JYS,IDJX,ZK)
		CALL SOLVE(NF0,JSIX,DJX,JYS,IDJX,FORCE,D,ZK)
	      IF(ILTD0.LT.1)THEN
      IF(IGK.EQ.3.AND.(IS.GT.0.OR.IXH.GE.20).AND.IXH.NE.1)GOTO 18
		IF(IS.GT.0.AND.IXH.EQ.1)GOTO 18
		IF(IGK.EQ.3)GOTO 15
	      ELSE IF(ILTD0.GE.1)THEN
      IF(IGK.EQ.2.AND.(IS.GT.0.OR.IXH.GE.20).AND.IXH.NE.1)GOTO 18
		IF(IS.GT.0.AND.IXH.EQ.1)GOTO 18
		IF(IGK.EQ.2)GOTO 15
	      END IF
		
18            CONTINUE

	     IF(IGK.EQ.4.and.lth.eq.9)then
	       rewind(20)
	       call disk_vs(psj,dsj,jsix,Jmh,lx,nfz,izbxz,paz,ith)
c             rewind(31)
c             write(31)paz
            
	     END IF

	       CALL ANSELE(IFD,JSH,JMH,NFZ,JSIX,D,
     +   EP2,DJX,PSZ,NCDH,JCTAB,LX,ith,psl,thl)
	       IF((IGK.EQ.2.AND.ILTD0.LT.1).OR.
     +                         (IGk.EQ.1.AND.ILTD0.GE.1))THEN
	       DO 12 J=1,JSIX
12                   D(J)=0.0
	     END IF
10            CONTINUE
1918    CONTINUE
	    RETURN
	    END

C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    ×÷Õß:             Àî  ÖÇ                                                                        *
C*    ³ÌÐòÃû:                                                                                         *Á

C*                           SOLVE                                                                        *
C*    ¹¦ÄÜ:
C*                           ÏûÔªÇó½âÅ  ¡¡                                                             *
C*                                                                                                              *
C*    ±»µ÷ÓÃ³ÌÐò:                          ¡                                                     *
C*                           SANS                                                                 *
C*    µ÷ÓÃ³ÌÐò:                          ¡¡                                                            *
c*                            ÎÞ                                                                            *
C*    ¶ÁÈ¡ÎÄ¼þ:                           ¡¡                                                     *
c*                            ÎÞ                                                                            *
c*    Ð´ÈëÎÄ¼þ:                                                                           ¡¡     *
c*                            ÎÞ                                                                            *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C------------------H D Q J----------------------------

	    SUBROUTINE SOLVE(NF0,JSIX,DJX,JYS,IDJX,FORCE,D,ZK)


	    IMPLICIT DOUBLE PRECISION(A-H,O-Z)
	    DIMENSION ZK(NF0)
	    DIMENSION JYS(JSIX),IDJX(JSIX)
	    DIMENSION FORCE(JSIX),DJX(JSIX),D(JSIX)

	    DO 5 I=1,JSIX
5             D(I)=FORCE(I)

	    DO 20 I=1,JSIX
		S=D(I)
		II=IDJX(I)
		JMIN=JYS(I)
		IF(JMIN.EQ.0)GOTO 20
		DO 10 JX=1,JMIN
10                          S=S-ZK(II-JX)*D(I-JX)
20            D(I)=S/ZK(II)

	    DO 30 I=1,JSIX
30            D(I)=D(I)/DJX(I)
	    DO 50 IX=1,JSIX
		I=JSIX+1-IX
		II=IDJX(I)
		D(I)=D(I)/ZK(II)
		JMIN=JYS(I)
		IF(JMIN.EQ.0)GOTO 50
		S=D(I)
		DO 40 JX=1,JMIN
40                          D(I-JX)=D(I-JX)-ZK(II-JX)*S
50            CONTINUE
	    RETURN
	    END                                       

		
		

C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    ×÷Õß:             Àî  ÖÇ                                                                        *
C*    ³ÌÐòÃû:                                                                                         *Á

C*                            SPRING                                                                 *
C*    ¹¦ÄÜ:                         Ý  ¡¡                                                             *
C*                            Ñ¡Ôñµ¯»É                           ö                                     *
C*    ±»µ÷ÓÃ³ÌÐò:                          ¡                                                     *
C*                            SANS                                                                         *
C*    µ÷ÓÃ³ÌÐò:                          ¡¡                                                            *
c*                            ÎÞÊ                                                                          *
C*    ¶ÁÈ¡ÎÄ¼þ:                           ¡¡                                                     *
c*                        Í¨µÀºÅ            ÄÚÈÝ  ¡¡                                                   *
c*                        9                ¹¤×÷ºÉÔØ(3¹¤¿ö Àäµõ2)                           *
c*                        15               ¹¤×÷ºÉÔØ(4¹¤¿ö Àäµõ3)                           *
c*    Ð´ÈëÎÄ¼þ:                                                                           ¡¡     *
c*                        1                Êä³ö·ÖÎöÊý¾Ý                                                *
c*                        23               ¹©Êý¾Ý´¦Àí³ÌÐòÓÃµÄÊä³ö·ÖÎöÊý¾Ý               *
c*                        20               ²¢ÁªÊý£¬´®ÁªÊý£¬µ¯»ÉºÅ£¬Ñ¹ËõÖµ               *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    ÐÞ¸Ä»òÔö¼Ó                                                                             ¡¡  *
c*    ÐÞÔöÕß:                                                                                       ¡¡ *
c*    ÐÞÔöÈÕÆÚ:                                                                                     ¡¡ *
c*    ÐÞÔöÄÚÈÝ:                                                                                     ¡¡ *
c*                                                                                                               *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	    SUBROUTINE SPRING(NF0,NFZ,NCDH,JSIX,LX,IMH,PGZ1,
     1       D,FORCE,THL,ITH,ZK,IDJX,IS,IXH,JCTAB,ITLX,JKTH)

	    IMPLICIT DOUBLE PRECISION(A-H,O-Z)
	    COMMON/NXHXS/FXHXS,IZBXZ
	    COMMON/KZKG/kzdw,IMTH,NTH,THXS
	    COMMON/SPRI/IDW,IRD,TKZ,KTH,TK(5,50),TMINSZ(5,50)
c             COMMON/GK/IGK
	    COMMON/GK/IGK,IFJ5,IFJ6,IFJ7,IFJ8,IFJ9,IFJ10,IFJ11,IFJ12
	    common/prin/iprn,Iprin1,Isz,ILTD0,izh
	    DIMENSION LX(NFZ),IMH(NFZ),ITLX(NFZ),ITH(NCDH),IDJX(JSIX)
	    DIMENSION PGZ1(JSIX),D(JSIX),FORCE(JSIX),THL(NCDH),ZK(NF0)
	    DIMENSION JCTAB(NCDH),JKTH(5)
	    CHARACTER*20 AUNIT
	    IXH=IXH+1
	    IS=5
	    IGD=0
	    tmin=30
	    IF(ILTD0.LT.1)THEN
	    IF(IGK.EQ.3)THEN
		REWIND(9)
		READ(9)PGZ1
	    ELSE IF(IGK.EQ.4)THEN
			 REWIND(15)
			 READ(15)PGZ1
	    END IF
	    ELSE IF(ILTD0.GE.1)THEN
	    IF(IGK.EQ.2)THEN
		REWIND(9)
		READ(9)PGZ1
	    ELSE IF(IGK.EQ.3)THEN
           REWIND(15)
           READ(15)PGZ1
	    END IF
	    END IF
	    
	    DO 1000 IFZ=1,NFZ
	      JDLX=LX(IFZ)
	      LTH=ITLX(IFZ)
	      if(lth.eq.9)lth=3
	      KTH=JKTH(LTH)
	      J=IMH(IFZ)
	      IF(J.EQ.0)GOTO 1000
	      I=(J-1)*6+IZBXZ
	      JTH=ITH(J)
	      IF(JDLX.EQ.99)THEN
		THL(J)=PGZ1(I)
		ITH(J)=9999
	      END IF
		IF(JDLX.LE.1000.AND.IGD.EQ.0)IGD=1            
		IF(JDLX.GT.90.AND.JDLX.NE.99.AND.
     +                         JDLX.LT.1000.OR.JDLX.LT.80)GOTO 1000
		   if(iltd0.lt.1.and.jdlx.eq.99)goto 1000
c---------------hot status distribute loads
			IF(ILTD0.LT.1)THEN
			IF(IGK.EQ.4)THEN
			  IF(JTH.NE.9999)FORCE(I)=FORCE(I)-PGZ1(I)
c                                                  IF(JTH.EQ.9999)FORCE(I)=FORCE(I)-THL(J)
			  GOTO 1000
		

			ELSE IF(IGK.EQ.3.AND.JDLX.GT.1000)THEN
				N=JDLX/1000
				IF(JDLX.EQ.5000)N=9
				IF(N.GT.NTH)GOTO 300
C                                                             ____________________
				IB=JDLX-N*1000
				NBL=IB/100
				II=IB-NBL*100
	    TG=TK(LTH,II)
					GOTO 300
			END IF
c-----------cold status distribute load
			ELSE IF(ILTD0.GE.1)THEN
			IF(IGK.EQ.3)THEN
			  IF(JTH.NE.9999)FORCE(I)=FORCE(I)-THL(J)
			  IF(JTH.EQ.9999)FORCE(I)=FORCE(I)-THL(J)
			  GOTO 1000


			ELSE IF(IGK.EQ.2.AND.JDLX.GT.1000)THEN
				N=JDLX/1000
				IF(JDLX.EQ.5000)N=9
				IF(N.GT.NTH)GOTO 300
C                                                             ____________________
				IB=JDLX-N*1000
				NBL=IB/100
				II=IB-NBL*100
	    TG=TK(LTH,II)
					GOTO 300
			END IF
			END IF

		NBL=JDLX-80
		
	     PGZZ=ABS(PGZ1(I))/NBL
	     PGZA=ABS(PGZ1(I))/NBL
	     DLTT=ABS(D(I))
	     N1=1
	     N2=1
		IF(DLTT.GE.2*TKZ)THEN
			N=9
		GOTO 300
		END IF
		IF(JTH.NE.9999.AND.IXH.GE.20)THEN
			N1=JTH/1000
			N3=JTH/100

			N3=N3*100
			N2=JTH-N3
		ELSE
			N1=1
			IF(DLTT.GT.TKZ)N1=2
			N2=1
		END IF       
	    IF(D(I).GE.0.0)THEN
	     DO 100 N=N1,NTH
	     DO 100 II=N2,KTH
	     n5=n 
	     TG=TK(LTH,II)
          if(iltd0.ge.1)pgzz=pgzA+dltt/tg/N
          TMIN=TMINSZ(LTH,II)
          
          IF((PGZZ.LT.(TMIN*1./TG)).OR.
     +       (PGZZ/(TKZ-DLTT/N)*TKZ).LT.(TMIN/TG))THEN
              IGZAZ=1
              GOTO 100
          ELSE
              IGZAZ=0
          END IF
C          IF((PGZZ.GE.(TMIN*1./TG)).AND.
C     +       (PGZZ/(TKZ-DLTT/N)*TKZ).GE.(TMIN/TG).AND.
C     +     PGZZ.LE.(TKZ*1./TG).AND.
           IF(PGZZ.LE.(TKZ*1./TG).AND.
     +             (DLTT/(N*TG*PGZZ)).LE.THXS)GOTO 300
100          CONTINUE
	     N=N+5
	    ELSE
	     DO 200 N=N1,NTH
	     DO 200 II=N2,KTH
	     n5=n 
	     TG=TK(LTH,II)
		 TMIN=TMINSZ(LTH,II)
          IF((PGZZ.LT.(TMIN*1./TG)).OR.
     +       (PGZZ/(TKZ-DLTT/N)*TKZ).LT.(TMIN/TG))THEN
              IGZAZ=1
              GOTO 200
          ELSE
              IGZAZ=0
          END IF
C	     IF((PGZZ/(TKZ-DLTT/N)*TKZ).GE.(TMIN/TG).AND.
C     +             PGZZ.GE.(TMIN*1./TG).AND.
C     +            (PGZZ/(TKZ-DLTT/N)*TKZ).LE.(TKZ/TG).AND.
               IF((PGZZ/(TKZ-DLTT/N)*TKZ).LE.(TKZ/TG).AND.
     +            (DLTT/(N*TG*PGZZ)).LE.THXS)GOTO 300
200                                       CONTINUE
	     
	     N=N+5
	    END IF
300                         IJ=IDJX(I)

	     IF(N.LE.NTH)THEN

	       ZK(IJ)=ZK(IJ)+1./TG/N*NBL                                         

	       III=II+N*1000+NBL*100
	       IF(III.NE.JTH)IS=0
	       ITH(J)=III
	       THL(J)=PGZ1(I)-D(I)/TG/N*NBL
	     ELSE
	       THL(J)=PGZ1(I)
	       IF(ITH(J).NE.9999)IS=0
	       ITH(J)=9999
            if(((PGZZ/(TKZ-DLTT/N5)*TKZ).lt.(TMIN/TG).OR.
     +      PGZZ.LT.(TMIN*1./TG)).AND.IGZAZ.EQ.1)ITH(J)=8888
C	       IF(D(I).GE.0.0)THEN
C              if(PGZZ.lt.(TMIN*1./TG))ITH(J)=8888
C	       else
C              if((PGZZ/(TKZ-DLTT/N5)*TKZ).lt.(TMIN/TG).AND.(N.EQ.N5))ITH(J)=8888
C	       end if
	     END IF
1000    CONTINUE
	    
	    IF(IS.GT.0.OR.IXH.GE.20.OR.(IGD.EQ.0.AND.IS.GT.0))THEN
      if((igk.eq.3.AND.ILTD0.LT.1).OR.(IGK.EQ.2.AND.ILTD0.GE.1))then
		ihh=0
	     DO 66 IFZ=1,NFZ
	      JMH=IMH(IFZ)
	      MTH=ITH(JMH)
	      LTH=ITLX(IFZ)
c                            if(lx(ifz).gt.80.and.lx(ifz).lt.100)lx(ifz)=mth
		IF(JMH.EQ.0)THEN
			HGZ=0
			HAZ=0
			GOTO 55
		END IF
		I=(JMH-1)*6+IZBXZ
		N=MTH/1000
		NB=(MTH-MTH/1000*1000)/100
		J=MTH-MTH/100*100
	     THRD1=TK(LTH,J)
	     THRD=TK(LTH,J)*10.
		if(thrd1.gt.0.0000001)rkd=1/thrd1
		IF(N.EQ.0.OR.MTH.EQ.9999)THEN
			HGZ=0.
			HAZ=0
			THRD=0
			RKD=0
			HUY=0
			GOTO 55
		END IF  
		IF(ILTD0.LT.1)HGZ=THRD1*PGZ1(I)/NB*N
		IF(ILTD0.GE.1)HGZ=THRD1*(PGZ1(I)/NB*N-D(I))
		HUY=(HGZ+D(I))*10
		HGZ=TKZ*RKD*NB
55                          CONTINUE
		ICD=93
		IF(MTH.GT.0.and.jmh.ne.0)THEN
		IDH=JCTAB(JMH)
		DF=-D(I)*10
		IF(ILTD0.LT.1)PGZ=PGZ1(I)
		IF(ILTD0.GE.1)PGZ=(PGZ1(I)-D(I)*RKD/N*NB)
		thrd=thrd1*10
		rkd=rkd/10
		IF(KZDW.NE.1)THEN
		 pgz=pgz*9.807
		 HGZ=HGZ*9.807
		 thrd=thrd/9.807
		 rkd=rkd*9.807
		END IF 

        if(iprn.eq.3)THEN
         I93=93
         WRITE(50,5007)I93,IGK,IDH,MTH,MTH,THRD,rkd,TKZ*RKD,HUY,DF
5007  FORMAT(5I12,5F12.3)
        ELSE if(iprn.eq.1)THEN

		WRITE(23,201)ICD,IGK,IDH,MTH,THRD,rkd,HGZ,HUY,DF,PGZ

	     IHH=IHH+1
c            IF(ihh.eq.1.or.IHH.GE.24)THEN
c                IH=1
c                CALL CPRINT(1)
c                WRITE(1,203)
			  AUNIT='[mm,Kgf]'
c                IF(KZDW.NE.1)AUNIT='[mm,N]'
c                WRITE(1,204)IXH,IGK,AUNIT
c                write(1,205)
c            END IF
c            write(1,202)IDH,MTH,N,NB,THRD,rkd,hgz,HUY,DF,PGZ
	     END IF
	     write(20,*)nb,n,j,huy
		END IF
201            FORMAT(2I5,2I10,6F15.3)
202            format(2i10,2i5,6f15.3,/)
203            FORMAT(//,55X,'SPRING TABLE',/)
c205            format(7X,'POINT','  SP-NUMBER',' S','    P',
c     + '                RD  ','                        KD   ','             MAX-LOAD',
c     + '             PP-HIGHT',9X,'H-D','               WK-LOAD',/)
204            format(80X,'N-CIRCLE: ',I4,6X,'CASE No: ',I4,5X,A15,/)
66                          CONTINUE

	    END IF
	    end if
	    RETURN
	    END



C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    ×÷Õß:             Àî  ÖÇ                                                                        *
C*    ³ÌÐòÃû:                                                                                         *Á

C*                           DIVÉ                                                                          *
C*    ¹¦ÄÜ:
C*                           ¾ØÕó·Ö½âÅ  ¡¡                                                             *
C*                                                                                                              *
C*    ±»µ÷ÓÃ³ÌÐò:                          ¡                                                     *
C*                           SANS                                                                 *
C*    µ÷ÓÃ³ÌÐò:                          ¡¡                                                            *
c*                            ÎÞ                                                                            *
C*    ¶ÁÈ¡ÎÄ¼þ:                           ¡¡                                                     *
c*                            ÎÞ                                                                            *
c*    Ð´ÈëÎÄ¼þ:                                                                           ¡¡     *
c*                            ÎÞ                                                                            *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	    SUBROUTINE DIV(NF0,JSIX,DJX,JYS,IDJX,ZK)
	    IMPLICIT DOUBLE PRECISION(A-H,O-Z)
	    DIMENSION ZK(NF0)
	    DIMENSION DJX(JSIX),JYS(JSIX),IDJX(JSIX)

	    
	    


	    DO 5 I=1,JSIX
5                           DJX(I)=1.

	    DO 30 I=1,JSIX
		JMIN=I-JYS(I)
		K=IDJX(I)
		Z=ZK(K)
		DO 20 J=JMIN,I
			IJ=K-I+J
			SUM=ZK(IJ)
			JJJJJ=J-JMIN
			JXMIN=MIN0(JYS(J),JJJJJ)
			IF(JXMIN.EQ.0)GOTO 15
			JJ=IDJX(J)
	    DO 10 JX=1,JXMIN
10           SUM=SUM-ZK(IJ-JX)*ZK(JJ-JX)*DJX(J-JX)
15                                        IF(DJX(J).EQ.0.)THEN
			WRITE(1,*)'******************DUIJIAOXIANG YUANSU=0'
			RETURN
			END IF
			ZK(IJ)=SUM/DJX(J)
20                          CONTINUE
		DJX(I)=ZK(IJ)
30            ZK(IJ)=1.

	    RETURN
	    END

C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    ×÷Õß:             Àî  ÖÇ                                                                        *
C*    ³ÌÐòÃû:                                                                                         *Á

C*                            BOUND0                                                                 *
C*    ¹¦ÄÜ:                         Ý  ¡¡                                                             *
C*                            ÉèÖÃÍ¨ÓÃ±ß½çÌõ¼þ              ö                                     *
C*    ±»µ÷ÓÃ³ÌÐò:                          ¡                                                     *
C*                            SANS                                                                         *
C*    µ÷ÓÃ³ÌÐò:                          ¡¡                                                            *
c*                            ÎÞÊ                                                                          *
C*    ¶ÁÈ¡ÎÄ¼þ:                           ¡¡                                                     *
c*                            ÎÞÊ                                                                          *
c*    Ð´ÈëÎÄ¼þ:                                                                           ¡¡     *
c*                            ÎÞÊ                                                                          *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    ÐÞ¸Ä»òÔö¼Ó                                                                             ¡¡  *
c*    ÐÞÔöÕß:                                                                                       ¡¡ *
c*    ÐÞÔöÈÕÆÚ:                                                                                     ¡¡ *
c*    ÐÞÔöÄÚÈÝ:                                                                                     ¡¡ *
c*                                                                                                               *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *



	    SUBROUTINE BOUND0(JSIX,NF0,NFZ,LX,ZK,JMH,IDJX,ncdh,jctab)
	    IMPLICIT DOUBLE PRECISION(A-H,O-Z)
c             COMMON/GK/IGK
	    COMMON/GK/IGK,IFJ5,IFJ6,IFJ7,IFJ8,IFJ9,IFJ10,IFJ11,IFJ12
	    COMMON/PRIN/IPRIN,Iprin1,Isz,ILTD0,izh
	    COMMON/NXHXS/FXHXS,IZBXZ
	    COMMON/HDGD/NHD,ihd(400)
	    COMMON/IZDJ/IZD(10),ZDFC
	    DIMENSION ZK(NF0),IDJX(JSIX)
	    DIMENSION LX(NFZ),JMH(NFZ),jctab(ncdh)

	    
	    DIMENSION W(6)


	    YZ=10.**ZDFC

	    DO 100 IFZ=1,NFZ
		IMH=JMH(IFZ)
		idh=jctab(imh)
		ILX=LX(IFZ)

		IF(ILTD0.LT.1)THEN
		 IF(ILX.LT.80.AND.(IGK.EQ.1.or.igk.eq.100))THEN
		  ILX=ILX+100
		 ELSE IF(ILX.LT.80.AND.(igk.NE.1.and.igk.ne.100))THEN
		  ILX=100
		 END IF
c********************************or
	     IF(IMH.EQ.0.OR.ILX.EQ.100.OR.ILX.EQ.98.OR.
     +                     (ILX.GT.200.AND.ILX.LT.300))GOTO 100
		 IF(ILX.GT.300.AND.ILX.LT.400)GOTO 100
		 IF(ILX.EQ.91.AND.IGK.EQ.1)THEN
			LX(IFZ)=81
			GOTO 100
		 ELSE IF(ILX.EQ.92.AND.IGK.EQ.1)THEN
			LX(IFZ)=82
			GOTO 100
		 ELSE IF(ILX.EQ.94.AND.IGK.EQ.1)THEN
			LX(IFZ)=84
			GOTO 100
		 END IF
		ELSE IF(ILTD0.GE.1)THEN
		 IF(ILX.LT.80.AND.(IGK.EQ.0))THEN
		    ILX=ILX+100
		 ELSE IF(ILX.LT.80.AND.IGK.NE.0)THEN
		    ILX=100
		 END IF
		 IF(IMH.EQ.0.OR.ILX.EQ.100
     1.OR.ILX.EQ.98.OR.(ILX.GT.200.AND.ILX.LT.300))GOTO 100
		 IF(ILX.GT.300.AND.ILX.LT.400)GOTO 100
		 
		 IF(ILX.EQ.91.AND.IGK.EQ.0)THEN
			LX(IFZ)=81
			GOTO 100
		 ELSE IF(ILX.EQ.92.AND.IGK.EQ.0)THEN
			LX(IFZ)=82
			GOTO 100
		 ELSE IF(ILX.EQ.94.AND.IGK.EQ.0)THEN
			LX(IFZ)=84
			GOTO 100
		 END IF
		END IF
		 
		if(igk.ne.100.and.nhd.gt.0.and.
     +   (ilx.gt.80.and.ilx.le.99))then
		   do 111 ijhd=1,nhd
			if(idh.eq.ihd(ijhd))goto 100
111                            continue
		end if
		DO 40 I=1,6
40                          W(I)=0.
		IH=0
		JH=0
				
		IF(ILX.GT.80.AND.ILX.LE.94.OR.
     +                         ILX.GT.1000.or.ilx.eq.99)THEN
			IF(IGK.EQ.1.and.ILTD0.LT.1)W(IZBXZ)=YZ
			IF(IGK.EQ.0.and.ILTD0.GE.1)W(IZBXZ)=YZ
			IF(IGK.EQ.100)W(IZBXZ)=YZ
		ELSE
			IU=ILX-100
			IV=IU/10
			IW=IU-IV*10
			DO 50 I=1,7
			IF(IV.EQ.I)THEN
				JH=1
			 IF(I.EQ.1.OR.I.EQ.4.OR.I.EQ.5.OR.I.EQ.7)W(1)=YZ
			 IF(I.EQ.2.OR.I.EQ.4.OR.I.EQ.6.OR.I.EQ.7)W(2)=YZ
			 IF(I.EQ.3.OR.I.EQ.5.OR.I.EQ.6.OR.I.EQ.7)W(3)=YZ
			END IF
			IF(IW.EQ.I)THEN
				IH=1
			 IF(I.EQ.1.OR.I.EQ.4.OR.I.EQ.5.OR.I.EQ.7)W(4)=YZ
			 IF(I.EQ.2.OR.I.EQ.4.OR.I.EQ.6.OR.I.EQ.7)W(5)=YZ
			 IF(I.EQ.3.OR.I.EQ.5.OR.I.EQ.6.OR.I.EQ.7)W(6)=YZ
			END IF
			IF(JH.EQ.1.AND.IH.EQ.1)GOTO 60
50                          CONTINUE
		END IF


60            DO 70 I=1,6
	    J=6*(IMH-1)+I
	    IJ=IDJX(J)
	    ZK(IJ)=ZK(IJ)+W(I)

70            CONTINUE
100     CONTINUE
	    RETURN
       END     


C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    ×÷Õß:             Àî  ÖÇ                                                                        *
C*    ³ÌÐòÃû:                                                                                         *Á

C*                            BOUND2                                                                 *
C*    ¹¦ÄÜ:                         Ý  ¡¡                                                             *
C*                           ¡55 6 8 ¹¤¿öÖÃÕæµ¯»É¸Õ¶È  ö                                     *
C*    ±»µ÷ÓÃ³ÌÐò:                          ¡                                                     *
C*                            SANS                                                                         *
C*    µ÷ÓÃ³ÌÐò:                          ¡¡                                                            *
c*                            ÎÞÊ                                                                          *
C*    ¶ÁÈ¡ÎÄ¼þ:                           ¡¡                                                     *
c*                            ÎÞÊ                                                                          *
c*    Ð´ÈëÎÄ¼þ:                                                                           ¡¡     *
c*                            ÎÞÊ                                                                          *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    ÐÞ¸Ä»òÔö¼Ó                                                                             ¡¡  *
c*    ÐÞÔöÕß:                                                                                       ¡¡ *
c*    ÐÞÔöÈÕÆÚ:                                                                                     ¡¡ *
c*    ÐÞÔöÄÚÈÝ:                                                                                     ¡¡ *
c*                                                                                                               *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	    SUBROUTINE BOUND2(NF0,NFZ,NCDH,JSIX,IMH,
     +                        ITH,ZK,IDJX,TK,ITLX)
	    IMPLICIT DOUBLE PRECISION(
     +A-H,O-Z)
	    COMMON/NXHXS/FXHXS,IZBXZ
c             COMMON/KZKG/ISCXZ,ISCDW,I7,NTH,KTH
	    DIMENSION IMH(NFZ),ITH(NCDH),IDJX(JSIX)
	    DIMENSION ZK(NF0),ITLX(NFZ)
	    DIMENSION TK(5,50)

	    DO 1000 IFZ=1,NFZ
	      LTH=ITLX(IFZ)
		J=IMH(IFZ)
		IF(J.EQ.0)GOTO 1000

	     MTH=ITH(J)
	     IF(MTH.EQ.0.OR.(MTH.EQ.9999.or.mth.eq.8888))GOTO 1000
	     N=MTH/1000
	     NBL=(MTH-MTH/1000*1000)/100
	     JJ=MTH-MTH/100*100
	     TG=TK(LTH,JJ)
	     IF(N.EQ.0.OR.(MTH.EQ.9999.or.mth.eq.8888))THEN
	      GOTO 1000
	     END IF
	     I=(J-1)*6+IZBXZ

	     IJ=IDJX(I)
	     thgd = 1./tg/n*nbl
	     ZK(IJ)=ZK(IJ)+thgd

1000    CONTINUE

	    RETURN

	    END


C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    ×÷Õß:             Àî  ÖÇ                                                                        *
C*    ³ÌÐòÃû:                                                                                         *Á

C*                            BOUND2                                                                 *
C*    ¹¦ÄÜ:                         Ý  ¡¡                                                             *
C*                           ¡99 ¹¤¿öµ¯»É¸ÕËø¶¨             ö                                     *
C*    ±»µ÷ÓÃ³ÌÐò:                          ¡                                                     *
C*                            SANS                                                                         *
C*    µ÷ÓÃ³ÌÐò:                          ¡¡                                                            *
c*                            ÎÞÊ                                                                          *
C*    ¶ÁÈ¡ÎÄ¼þ:                           ¡¡                                                     *
c*                            ÎÞÊ                                                                          *
c*    Ð´ÈëÎÄ¼þ:                                                                           ¡¡     *
c*                            ÎÞÊ                                                                          *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    ÐÞ¸Ä»òÔö¼Ó                                                                             ¡¡  *
c*    ÐÞÔöÕß:                                                                                       ¡¡ *
c*    ÐÞÔöÈÕÆÚ:                                                                                     ¡¡ *
c*    ÐÞÔöÄÚÈÝ:                                                                                     ¡¡ *
c*                                                                                                               *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	    SUBROUTINE BOUND1(NF0,NFZ,JSIX,IMH,
     +                                    ZK,IDJX,LX)
	    IMPLICIT DOUBLE PRECISION(A-H,O-Z)
	    COMMON/NXHXS/FXHXS,IZBXZ
c             COMMON/KZKG/ISCDW,NTH,KTH
	    COMMON/IZDJ/IZD(10),ZDFC
	    DIMENSION IMH(NFZ),IDJX(JSIX)
	    DIMENSION ZK(NF0)
	    DIMENSION LX(NFZ)
	    YZ=10.**ZDFC

	    DO 1000 IFZ=1,NFZ
		J=IMH(IFZ)
		ILX=LX(IFZ)
		IF(J.EQ.0)GOTO 1000

		IF(ILX.GT.80.AND.ILX.Le.99.OR.ILX.GE.1000)THEN
			W=YZ
		ELSE IF(ILX.EQ.199)THEN
			W=YZ
		ELSE
			GOTO 1000
		END IF

		I=(J-1)*6+IZBXZ

		IJ=IDJX(I)
		ZK(IJ)=ZK(IJ)+YZ
1000    CONTINUE

	    RETURN

	    END

C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    ×÷Õß:             Àî  ÖÇ                                                                        *
C*    ³ÌÐòÃû:                                                                                         *Á

C*                            BOUND3                                                                 *
C*    ¹¦ÄÜ:                         Ý  ¡¡                                                             *
C*                           ¡ ÉèÖÃÔÚ¸ø¶¨¹¤¿öÆð×÷ÓÃµÄÖ§µõ¼Ü                             *
C*    ±»µ÷ÓÃ³ÌÐò:                          ¡                                                     *
C*                            SANS                                                                         *
C*    µ÷ÓÃ³ÌÐò:                          ¡¡                                                            *
c*                            ÎÞÊ                                                                          *
C*    ¶ÁÈ¡ÎÄ¼þ:                           ¡¡                                                     *
c*                            ÎÞÊ                                                                          *
c*    Ð´ÈëÎÄ¼þ:                                                                           ¡¡     *
c*                            ÎÞÊ                                                                          *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    ÐÞ¸Ä»òÔö¼Ó                                                                             ¡¡  *
c*    ÐÞÔöÕß:                                                                                       ¡¡ *
c*    ÐÞÔöÈÕÆÚ:                                                                                     ¡¡ *
c*    ÐÞÔöÄÚÈÝ:                                                                                     ¡¡ *
c*                                                                                                               *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	    SUBROUTINE BOUND3(JSIX,NF0,NFZ,LX,ZK,JMH,IDJX)
	    IMPLICIT DOUBLE PRECISION(A-H,O-Z)

	    COMMON/IZDJ/IZD(10),ZDFC
	    DIMENSION ZK(NF0),IDJX(JSIX)
	    DIMENSION LX(NFZ),JMH(NFZ)

	    DIMENSION W(6)

	    YZ=10.**ZDFC

	    DO 100 IFZ=1,NFZ
		IMH=JMH(IFZ)
		ILX=LX(IFZ)

		DO 40 I=1,6
40                          W(I)=0.
		IH=0
		JH=0

		IF(ILX.GT.300.AND.ILX.LT.400)THEN
			IU=ILX-300
			IV=IU/10
			IW=IU-IV*10
			DO 50 I=1,7
			IF(IV.EQ.I)THEN
				JH=1
			 IF(I.EQ.1.OR.I.EQ.4.OR.I.EQ.5.OR.I.EQ.7)W(1)=YZ
			 IF(I.EQ.2.OR.I.EQ.4.OR.I.EQ.6.OR.I.EQ.7)W(2)=YZ
			 IF(I.EQ.3.OR.I.EQ.5.OR.I.EQ.6.OR.I.EQ.7)W(3)=YZ
			END IF
			IF(IW.EQ.I)THEN
				IH=1
			 IF(I.EQ.1.OR.I.EQ.4.OR.I.EQ.5.OR.I.EQ.7)W(4)=YZ
			 IF(I.EQ.2.OR.I.EQ.4.OR.I.EQ.6.OR.I.EQ.7)W(5)=YZ
			 IF(I.EQ.3.OR.I.EQ.5.OR.I.EQ.6.OR.I.EQ.7)W(6)=YZ
			END IF
			IF(JH.EQ.1.AND.IH.EQ.1)GOTO 60
50                          CONTINUE
		END IF


60            DO 70 I=1,6
	    J=6*(IMH-1)+I
	    IJ=IDJX(J)
	    ZK(IJ)=ZK(IJ)+W(I)

70            CONTINUE
100     CONTINUE
	    RETURN
	    END
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    ×÷Õß:             Àî  ÖÇ                                                                        *
C*    ³ÌÐòÃû:                                                                                         *Á

C*                            BOUND4                                                                 *
C*    ¹¦ÄÜ:                         Ý  ¡¡                                                             *
C*                           ¡77 ¹¤¿öÊ±ÐÞÕýÓÒ¶ËÏî     ö                                     *
C*    ±»µ÷ÓÃ³ÌÐò:                          ¡                                                     *
C*                            SANS                                                                         *
C*    µ÷ÓÃ³ÌÐò:                          ¡¡                                                            *
c*                            ÎÞÊ                                                                          *
C*    ¶ÁÈ¡ÎÄ¼þ:                           ¡¡                                                     *
c*                        Í¨µÀºÅ            ÄÚÈÝ  ¡¡                                                   *
c*                        9                ¹¤×÷ºÉÔØ                                                   *
c*    Ð´ÈëÎÄ¼þ:                                                                           ¡¡     *
c*                            ÎÞÊ                                                                          *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    ÐÞ¸Ä»òÔö¼Ó                                                                             ¡¡  *
c*    ÐÞÔöÕß:                                                                                       ¡¡ *
c*    ÐÞÔöÈÕÆÚ:                                                                                     ¡¡ *
c*    ÐÞÔöÄÚÈÝ:                                                                                     ¡¡ *
c*                                                                                                               *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	    subroutine bound4(jsix,pgz1,force)
	    IMPLICIT DOUBLE PRECISION(A-H,O-Z)

	     dimension force(jsix),pgz1(jsix)
	     rewind 9
	     read(9)pgz1
	     do 1000 ifz=1,jsix
	     force(ifz)=force(ifz)-pgz1(ifz)
1000     continue
	     return
	     end
