c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    作者:       李  智                                   *
c*    程序名:                                              *
c*                RIGHT                                   *
c*    功能:                                                *
c*                建立矩阵右端项                             *
c*    被调用程序:                                          *
c*                SANS                                         *
c*    调用程序:                                            *
c*                SLOAD                                   *
c*                BLOAD                                   *
c*    读取文件:                                             *
c*            通道号      内容                              *
c*            7          转置矩阵,旋转矩阵,单元刚度矩阵         *
c*            8          STRUCK处理后的内部原始数据           *
c*            13         内部原始数据                        *
c*            14         水压实验用的内部原始数据(9工况)        *
c*            21         超温超压用的内部原始数据(7工况)        *
cc*    写入文件:                                            *
c*            通道号      内容                      　       *
c*            4          单元始末端自重力向量或末端热胀力向量    *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    修改或增加                                           *
c*    修增者:                                              *
c*    修增日期:                                            *
c*    修增内容:                                            *
c*                                                         *
c*                                                         *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      SUBROUTINE RIGHT(JSIX,NFZ,NF0,FORCE,JSH,JMH,IDJX,ZK)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON/GK/IGK,IFJ5,IFJ6,IFJ7,IFJ8,IFJ9,IFJ10
      COMMON/PRIN/Iprin,Iprin1,Isz,ILTD0,izh
      COMMON/NXHXS/FXHXS,IZBXZ
      COMMON/SCDS/NFZ1,NDD,JD1(50),DD(50,6),D8(50,6),D6(50,6)
      COMMON/LLGD/NLL,MLL(50),RLL(50,3)
      COMMON/LLLL/NLLLL,MLLLL(50),RLLLL(50)
      COMMON/KZKG/ISCKZ,it2,it3,t4,isrdw
      COMMON/SCHZ/NHZ,IHZ(300),JHZ(300),HZ(300,6),
     +nhz5,ihz5(300),hz5(300,6)
      COMMON/HDGD/NHD,IHD(400),HD(400)
      COMMON/YYGD/NWY,IWY(50),JWY(50),WY(50,6)

      DIMENSION FORCE(JSIX),ZK(NF0)
      DIMENSION JSH(NFZ),JMH(NFZ),IDJX(JSIX)

      DIMENSION F(6),RZL(6),DL(6),SLL(6),GW1(6),
     +                 GW2(6),WG1(6),WG2(6),RZ1(6)
      DIMENSION G1(6,6),G2(6,6),DK(6,6),ROT(6,6),SC(6,6),T(6,6),RZ2(6)
      DIMENSION GLLLL(6)

      DATA GLLLL/6*0.0/
      DATA G1,G2,SLL/78*0.0/
      IDD=0
      ILL=0
      REWIND 4
      REWIND 7
      REWIND 8

      DO 66 I=1,JSIX
66      FORCE(I)=0.0
      DO 100 IFZ=1,NFZ
            IMH=JMH(IFZ)
            ISH=JSH(IFZ)
            DO 30 I=1,6
                  F(I)=0.0
                  RZL(I)=0.0
                  GW1(I)=0.0
                  GW2(I)=0.0
                  DO 30 J=1,6
                        G1(I,J)=0.0
                        G2(I,J)=0.0
30              CONTINUE
            READ(7)SC,ROT,DK
            READ(8)NC,RL,E0,ET,RI,A1,ALF,GGZ,GWAX,GWAY,GWAZ,
     +                 GWB,T0,TJ,WT,OD,SK,
     1                 PU8,C5,C6,C7,C8,C9,C10,D1,SA,IS,IM,AA,AA,AA
      
      if(igk.eq.100)gsz=3.1415926535*((od-2*wt)/2)**2/10         

            IF((IGK.LE.2.AND.ILTD0.LT.1).OR.
     +             (IGK.GE.3.AND.ILTD0.GE.1).OR.IGK.GE.5)E=ET
            IF((IGK.EQ.3.AND.ILTD0.LT.1).OR.
     +             (IGK.LE.2.AND.ILTD0.GE.1).OR.IGK.EQ.4)E=E0
            if(igk.eq.10)e=e0

                  RL1=RL*1.E2


C-----------------------------SUAN ZIZONG-------------------------------
            IF(((IGK.EQ.1.OR.IGK.EQ.4).AND.ILTD0.LT.1).OR.
     + ((IGK.EQ.0.OR.IGK.EQ.3.OR.IGK.EQ.1).AND.ILTD0.GE.1).OR.
     +             IGK.EQ.6.OR.IGK.EQ.7.OR.
     +             IGK.EQ.9.or.igk.eq.10.or.igk.eq.100)THEN

                   DO 300 I=1,6
                   GW1(I)=1.0
300                    CONTINUE

              IF(NC.NE.0)CALL SLOAD(RL1,G1,G2)
              IF(NC.EQ.0)CALL BLOAD(C5,C6,C7,C8,C9,C10,RL1,A1,G1,G2)
                     
                     IF(IGK.EQ.6)THEN
                       GW1(1)=GWAX/1.D2
                       GW1(2)=GWAY/1.D2
                       GW1(3)=GWAZ/1.D2
                     else if(igk.eq.10.or.
     +                            (igk.eq.4.AND.ILTD0.LT.1).OR.
     +                            (IGK.EQ.1.AND.ILTD0.GE.1))then
                       gw1(izbxz)=-0.0000000001
                     ELSE
                       GW1(IZBXZ)=-GGZ/1.D2
                       if(igk.eq.100)gw1(izbxz)=-gsz/1.d2
                     END IF
                  
                  CALL JZZZ(ROT,T)

                  CALL JZXL(T,GW1,WG1)

                  CALL JZXL(G2,WG1,GW2)
                  CALL JZXL(G1,WG1,GW1)
                  
                  CALL JZXL(ROT,GW1,WG1)
                  CALL JZXL(ROT,GW2,WG2)
                  CALL XLCC(WG1,GW1)
                  CALL XLCC(WG2,GW2)

C---------------------------SUAN DUANDIAN FUJIA WEIYI-----------------------
                  if(igk.eq.6)THEN
                  IF(ISH.EQ.0.OR.IMH.EQ.0)THEN
                        DO 48 J=1,NDD
                        IF(JD1(J).EQ.IS.OR.JD1(J).EQ.IM)THEN
                        DO 41 I=1,6
                        DL(I)=D6(J,I)
41                                CONTINUE
                        IF(ISH.EQ.0)THEN
                              CALL JZCH(DK,SC,T)
                              CALL JZZF(T,DK)
                        END IF  
                        CALL JZXL(DK,DL,WG1)
                        CALL XLAD(WG1,GW2,DL)
                        CALL XLCC(DL,GW2)
                        END IF
48                              CONTINUE
                  END IF
                  end if
C----------------------------------------------------------------------


C---------------------------SUAN LENGLA 444444GK-------------------------
                  IF((((IGK.EQ.4.AND.ILTD0.LT.1).OR.
     +                       (IGK.EQ.1.AND.ILTD0.GE.1)).or.
     +                       igk.eq.10).AND.
     +                       NLL.NE.0.AND.NLLLL.EQ.0)THEN
                     DO 35 J=1,NLL
                       IF(MLL(J).EQ.IM)THEN       
                        
                        DO 34 I=1,3
34                              SLL(I)=RLL(J,I)
                        CALL XLZF(SLL,WG1)
                        CALL JZXL(DK,WG1,DL)
c                               CALL JZXL(DK,sll,DL)
                        CALL XLAD(DL,GW2,WG2)
                        CALL XLCC(WG2,GW2)
                        
                        CALL JZZZ(SC,T)
                        CALL JZXL(T,DL,WG1)
                        CALL XLZF(WG1,DL)
                        CALL XLAD(DL,GW1,WG2)
                        CALL XLCC(WG2,GW1)

                       END IF
35                        CONTINUE



C---------------------------SUAN LENGLA 444444GK-------------------------
                  ELSE IF((((IGK.EQ.4.AND.ILTD0.LT.1).OR.
     +                            (IGK.EQ.1.AND.ILTD0.GE.1)).or.
     +                             igk.eq.10).AND.
     +                              NLLLL.NE.0)THEN
                     DO 351 J=1,NLLLL
                       IF(MLLLL(J).EQ.IM)THEN     
                        GLLLL(1)=RLLLL(J)
                         GLLLL(1)=E*RI/A1*RLLLL(J)/RL/100.0
c                                GLLLL(1)=E*RI/A1*RLLLL(J)/100.
                        CALL JZXL(ROT,GLLLL,DL)
                        CALL XLAD(DL,GW1,WG2)
                        CALL XLCC(WG2,GW1)
                        
                        CALL JZZZ(SC,T)
                        CALL JZXL(T,DL,WG1)
                        CALL XLZF(WG1,DL)
                        CALL XLAD(DL,GW2,WG2)
                        CALL XLCC(WG2,GW2)

                       END IF
351                       CONTINUE
                  
                  END IF
            
            

C-------------------------------------------------------------------------

              WRITE(4) GW1,GW2
C__________________________________________________________________________




C---------------------------##REZANG GK##----------------------------------
            ELSE IF(((IGK.EQ.2.OR.IGK.EQ.3).AND.ILTD0.LT.1).OR.
     +                  ((IGK.EQ.2.OR.IGK.EQ.4).AND.ILTD0.GE.1).OR.
     +                    IGK.EQ.8)THEN

                  DO 310 I=1,6
310                     RZL(I)=0.0      
                  IF(NC.NE.0)THEN
                        RZL(1)=-E*RI/A1*(TJ-T0)*ALF/100.0D0
                  ELSE
                        RZL(1)=-E*RI*D1*2.0D0*RL1*DSIN(A1/2.D0)*
     1                          ALF*(TJ-T0)/100.0D0
                        RZL(6)=RZL(1)*RL1*PU8
                  END IF

                  CALL JZXL(ROT,RZL,WG2)
                  
C---------------------------SUAN DUANDIAN FUJIA WEIYI-----------------------
                  IF(ISH.EQ.0.OR.IMH.EQ.0)THEN
                        DO 38 J=1,NDD
                        IF(JD1(J).EQ.IS.OR.JD1(J).EQ.IM)THEN
                        DO 31 I=1,6
                        DL(I)=DD(J,I)
                        IF(IGK.EQ.8)DL(I)=D8(J,I)
31                                CONTINUE
                        IF(ISH.EQ.0)THEN
                              CALL JZCH(DK,SC,T)
                              CALL JZZF(T,DK)
                        
                        END IF  
                        CALL JZXL(DK,DL,WG1)
                        CALL XLAD(WG1,WG2,DL)
                        
                        CALL XLCC(DL,WG2)
                        END IF
38                              CONTINUE
                  END IF
C----------------------------------------------------------------------
C-----------------------------SUN LENGLA----------------------------------
                  
                  IF(((IGK.EQ.2.AND.ILTD0.LT.1).OR.
     +                      (IGK.EQ.4.AND.ILTD0.GE.1)))THEN
                  IF(NLL.NE.0.AND.NLLLL.EQ.0)THEN
                     DO 37 J=1,NLL
                        IF(MLL(J).EQ.IM)THEN
                        CS=2./3.
                        DO 36 I=1,3
36                              SLL(I)=RLL(J,I)
                        CALL JZXL(DK,SLL,WG1)
                        CALL SCXL(CS,WG1,DL)
                        CALL XLAD(DL,WG2,WG1)
                        CALL XLCC(WG1,WG2)
                        END IF
37                        CONTINUE
                        
                  ELSE IF(NLLLL.NE.0)THEN

                     DO 371 J=1,NLLLL
                       IF(MLLLL(J).EQ.IM)THEN     
                        CS=2./3.
                        GLLLL(1)=RLLLL(J)
                         GLLLL(1)=E*RI/A1*RLLLL(J)/RL/100.0*CS
c                                GLLLL(1)=E*RI/A1*RLLLL(J)/100.0*cs
                        CALL JZXL(ROT,GLLLL,DL)
                        CALL XLAD(DL,WG2,WG1)
                        CALL XLCC(WG1,WG2)
                        
                       END IF
371                       CONTINUE
                  END IF
                  ENDIF
            
                  CALL XLZF(WG2,F)
                  IF((IGK.EQ.3.AND.ILTD0.LT.1).OR.
     +                     (IGK.EQ.2.AND.ILTD0.GE.1))THEN
                       WRITE(4)F
                       CALL XLCC(WG2,F)
                  END IF
                  CALL XLZF(F,WG2)
                  IF((IGK.NE.3.AND.ILTD0.LT.1).OR.
     +                     (IGK.NE.2.AND.ILTD0.GE.1))WRITE(4)WG2
            
            END IF          
C_________________________________________________________________________--
                  

70      IF(ISH.NE.0)THEN
            CALL JZZZ(SC,T)
            CALL JZXL(T,F,RZ1)
            CALL XLZF(RZ1,RZ2)
            CALL XLAD(RZ2,GW1,RZ1)
            DO 130 I=1,6
            K=6*(ISH-1)+I
            
130             FORCE(K)=FORCE(K)+RZ1(I)
      END IF
      IF(IMH.NE.0)THEN
            CALL XLAD(F,GW2,RZ2)
            DO 150 I=1,6
            K=6*(IMH-1)+I
150             FORCE(K)=FORCE(K)+RZ2(I)
      END IF
      
      if(igk.eq.100)goto 100    

c----constant spring        
      IF(((IGK.EQ.1.AND.ILTD0.LT.1).OR.
     +      (iltd0.GE.1.and.igk.eq.0).or.igk.eq.100).AND.
     +     NHD.LE.400.and.nhd.gt.0)THEN
            DO 180 I=0,NHD
            IF(I.EQ.0)GOTO 180
            IF(IM.EQ.IHD(I))THEN
                  IF(IZBXZ.EQ.1)K=6*(IMH-1)+1
                  IF(IZBXZ.EQ.2)K=6*(IMH-1)+2
                  IF(IZBXZ.EQ.3)K=6*(IMH-1)+3
                  FORCE(K)=FORCE(K)+HD(I)
            END IF
180             CONTINUE
      END IF


c---------add load
      IF(NHZ.gt.0)THEN
      DO 1801 I=1,NHZ
      IF(I.EQ.0)GOTO 1801
      IF(IM.EQ.IHZ(I).AND.IGK.EQ.JHZ(I))THEN
      DO 1082 J=1,6
      K=6*(IMH-1)+J
1082                    FORCE(K)=FORCE(K)+HZ(I,J)
      END IF
1801            CONTINUE
      END IF

c------5 case load
      IF(NHZ5.gt.0.AND.IGK.EQ.5)THEN
            DO 1803 I=0,NHZ5
            IF(I.EQ.0)GOTO 1803
            IF(IM.EQ.IHZ5(I))THEN
                  DO 1084 J=1,6
                  K=6*(IMH-1)+J
1084                    FORCE(K)=FORCE(K)+HZ5(I,J)
            END IF
1803            CONTINUE
      END IF

c-------constant displacement
      IF(NWY.NE.0)THEN
      DO 190,I=0,NWY
      IF(I.EQ.0)GOTO 190
            IF(IM.EQ.JWY(I).AND.IGK.EQ.IWY(I))THEN
            DO 200 J=1,6
                  WYY=ABS(WY(I,J))
                  IF(WYY.GT.0.0001)THEN
                  K=6*(IMH-1)+J
                  IJ=IDJX(K)
                  SZ=100000000000000000000.D0
                  FORCE(K)=SZ*WY(I,J)
                  ZK(IJ)=SZ
                  END IF
200             CONTINUE
            END IF
190     CONTINUE
      END IF

100     CONTINUE
      RETURN

1111    FORMAT(6F10.2)

      END

            
            
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    作者:       李  智                                   *
c*    程序名:                                              *
c*                SLOAD                                  *
c*    功能:                                                *
c*                建立直单元分配荷载力向量                     *
c*    被调用程序:                                          *
c*                RIGHT                                  *
c*    调用程序:                                            *
c*                无                                        *
c*    读取文件:                                            *
c*                无                                        *
c*    写入文件:                                            *
c*                无                                        *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE SLOAD(RL,G1,G2)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)  
      DIMENSION G1(6,6),G2(6,6)
            
      RG=RL/2
      G1(1,1)=RG
      G1(2,2)=RG
      G1(3,3)=RG
      GR=-RL*RG/6
      G1(5,3)=GR
      G1(6,2)=-GR
      G2(1,1)=RG
      G2(2,2)=RG
      G2(3,3)=RG
      G2(5,3)=-GR
      G2(6,2)=GR
      
      RETURN
      END

      
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    作者:       李  智                                   *
c*    程序名:                                              *
c*                BLOAD                                  *
c*    功能:                                                *
c*                建立弯单元分配荷载力向量                     *
c*    被调用程序:                                          *
c*                RIGHT                                  *
c*    调用程序:                                            *
c*                无                                        *
c*    读取文件:                                            *
c*                无                                        *
c*    写入文件:                                            *
c*                无                                        *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      
      SUBROUTINE BLOAD(C5,C6,C7,C8,C9,C10,RL,A1,G1,G2)
      IMPLICIT DOUBLE PRECISION(
     +A-H,O-Z)
      DIMENSION G1(6,6),G2(6,6)
      RA=RL*A1/2      
      G1(1,1)=RA
      G1(2,2)=RA
      G1(3,3)=RA
      G1(1,2)=-C5
      G1(2,1)=-C6
      G1(4,3)=C7
      G1(5,3)=-C8
      G1(6,1)=C9
      G1(6,2)=-C10
      G2(1,1)=RA
      G2(2,2)=RA
      G2(3,3)=RA
      G2(1,2)=C5
      G2(2,1)=C6
      G2(4,3)=C7
      G2(5,3)=C8
      G2(6,1)=C9
      G2(6,2)=C10
      RETURN
      END
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    作者:       李  智                                   *
c*    程序名:                                              *
c*                STRUCTK                                  *
c*    功能:                                                *
c*                建立总刚矩阵                               *
c*    被调用程序:                                          *
c*                SANS                                         *
c*    调用程序:                                            *
c*                SELEK                                   *
c*                SROT                                    *
c*                BELEK                                  *
c*                BROT                                   *
c*                CPRINT                                 *
c*    读取文件:                                            *
c*            通道号      内容                             *
c*            13         内部原始数据                      *
c*            14         水压实验用的内部原始数据(9工况)      *
c*            21         超温超压用的内部原始数据(7工况)      *
c*            31         应力内部转换文件                   *
c*            32         应力内部转换文件                   *
c*    写入文件:                                            *
c*            通道号      内容                             *
c*            31         应力内部转换文件                   *
c*            32         应力内部转换文件                   *
c*            7          转置矩阵,旋转矩阵,单元刚度矩阵         *
c*            8          STRUCK处理后的内部原始数据           *
c*            12         总刚度矩阵                        *
c*            1          输出分析数据                        *
c*            23         供数据处理程序用的输出分析数据         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    修改或增加                                           *
c*    修增者:                                              *
c*    修增日期:                                            *
c*    修增内容:                                            *
c*                                                         *
c*                                                         *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      
C***************THIS IS ZONGGaNG****************

      SUBROUTINE STRUCTK(NF0,NCDH,NFZ,KQR,KQL,ZK,ISH1,IMH1)
c       IMPLICIT INTEGER*4(K)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER*4 I,J,LI,L
      common/kzkg/it1,it2,it3,t4,isrdw
      common/prin/iprn,Iprin1,Isz,ILTD0,izh
	common/jssz/isz9
      COMMON/GK/IGK,IFJ5,IFJ6,IFJ7,IFJ8,IFJ9,IFJ10
      COMMON/SRST/ISTDH(20000),RYLQ(20000)
      COMMON/NXHXS/FXHXS,IZBXZ
      common/sz/zgz,zsz
      DIMENSION ZK(NF0)
      integer*4 KQR(NCDH),KQL(NCDH)
      integer*4 kk
      DIMENSION ISH1(NFZ),IMH1(NFZ)
      DIMENSION DK(6,6)
      DIMENSION T(6,6),T1(6,6),T2(6,6),TT(6,6)
      DIMENSION SC(6,6),ROT(6,6),ZJDK(6,6)
      LOGICAL DIA     
      

      DATA SC/1.,0.,0.,0.,0.,0.,
      
     1          0.,1.,0.,0.,0.,0.,
     1          0.,0.,1.,0.,0.,0.,
     1          0.,0.,0.,1.,0.,0.,
     1          0.,0.,0.,0.,1.,0.,
     1          0.,0.,0.,0.,0.,1./
      DATA ROT/36*0./
                  
      PI=3.1415926535
      ik=0
      DO 10 I=1,NF0
10         ZK(I)=0.0
      REWIND 13
      rewind 14
      rewind 21
      REWIND 12
      REWIND 7
      REWIND 8
      IQ=1
      if((igk.eq.1.AND.ILTD0.LT.1).OR.
     +     (IGK.EQ.0.AND.ILTD0.GE.1))
     +       open(unit=31,file='fort40',status='new')

      DO 120 IFZ=1,NFZ
            DO 15 I=1,6
            DO 15 J=1,6
                ROT(I,J)=0.0
15              DK(I,J)=0.0             
            
      IF(IGK.LT.10.or.igk.eq.100)
     +          READ(13)NC,IS,IM,LX1,OD,WT,ET,E0,GGZ,
     +                  GW1X,GW1Y,GW1Z,RL,RC,FI,
     +                  X,Y,Z,
     +                  SK,GW2,TJ,T0,ALF,SA,RK,PJ,XM0,XMT
      IF(NC.EQ.4.OR.NC.EQ.5.OR.NC.EQ.6)GGZ=GGZ/RL

      if(nc.eq.0)then
         zgz=zgz+ggz*rl*fi
      else
         zgz=zgz+ggz*rl
      end if
      gsz=3.1415926535*((od-2*wt)/2)**2/10
      if(nc.eq.0)then
         zsz=zsz+gsz*rl*fi
      else
         zsz=zsz+gsz*rl
      end if

      IF(IGK.EQ.9)THEN
       
       READ(14)ET,E0,
     +                  TJ,T0,ALF,PJ,XM0,XMT,GGZAD
       GW2=3.1415926535*((od-2*wt)/2)**2/10
       IF(NC.EQ.4.OR.NC.EQ.5.OR.NC.EQ.6)GGZAD=GGZAD/RL
       if(isz9.gt.0)then
          GGZ=GGZAD+GW2
       else
          GGZ=GGZAD
       end if

      ELSE IF(IGK.EQ.7.OR.IGK.EQ.8)THEN
       READ(21)ET,E0,
     +                  TJ,T0,ALF,PJ,XM0,XMT,GGZAD
      END IF


      IF((IGK.EQ.1.AND.ILTD0.LT.1).OR.(IGK.EQ.0.AND.ILTD0.GE.1))THEN
      ik=ik+1
c        IF(ik.eq.1.or.ik.ge.50)then
c                ik=1
c                CALL CPRINT(5)
c                WRITE(1,9991)
c                WRITE(1,9992)
c9991    FORMAT(//,60X,'PIPE DATA TABLE',/)
c9992    FORMAT(3X,'NC',5X,'START',7X,'END',6X,'TYPE',
c     +          7X,'X',9X,'Y',9X,'Z',3X,' RADIUS  ANGLE B-KIND     ',
c     +          'Si-S Si-E    OD      WT',7X,'GW',/)
c        END IF
           RSYJQ=RYLQ(IS)
           RMYJQ=RYLQ(IM)
           DW2=10*OD
           S2=10*WT
           PHI=FI*180/(PI)
c             IF(NC.EQ.0)THEN
c              WRITE(1,54926)
c     +             NC,IS,IM,LX1,RC,PHI,SK,RSYJQ,RMYJQ,DW2,S2,GGZ
c
c             ELSE
c              WRITE(1,54925)
c     +             NC,IS,IM,LX1,X,Y,Z,RSYJQ,RMYJQ,DW2,S2,GGZ
c             END IF
           ICD=70
           ITJ=TJ
           IT0=T0
           IF(LX1.NE.100.and.iprn.eq.1)
     +         write(23,54927)ICD,IM,ITJ,IT0,PJ,XM0,XMT,E0,ET,ALF
54925  FORMAT(I5,3I10,1X,3F10.3,24X,'*',2F5.2,1X,2F8.2,F10.3,'*')
54926  FORMAT(I5,3I10,1X,30X,3f7.3,3X,'*',
     +              2F5.2,1X,2F8.2,F10.3,'*')
54927  format(2i5,2i10,6f15.3)
            if(nc.eq.0.or.nc.eq.1)then
            write(31,5550)od,wt,tj,t0,pj,xm0,xmt,e0,et,alf,ggz
            end if
5550   format(11f10.3)
       END IF

C                IF(IGK.EQ.1)THEN
C                DZB1(IFZ,1)=X
C                DZB1(IFZ,2)=Y
C                DZB1(IFZ,3)=Z
C                END IF
         ET=ET*1.0E6
         E0=E0*1.0E6
C       IF(NC.EQ.4)E0=E0*50.
C       IF(NC.EQ.4)ET=ET*50.
         IF(ILTD0.LT.1)THEN
         IF(IGK.EQ.3)E=E0
         IF(IGK.EQ.1)E=ET
         ELSE
         IF(IGK.EQ.3)E=ET
         IF(IGK.EQ.0)E=E0
         END IF
         if(igk.eq.100)e=e0
         IF(IGK.GE.5.AND.IGK.LE.10)E=ET
         ALF=ALF*1.0E-4
         ISH=ISH1(IFZ)

         IMH=IMH1(IFZ)
         RW=0.5*OD
         RN=RW-WT
         RI=0.25*PI*(RW**4-RN**4)
         A1=0.25*(RW*RW+RN*RN)
         SI=RN/RW
         AS=DASIN(SI)
         S1=SI*SI
      AA=A1/PI
      RMUE=5.2*A1*(1./3.+(2.+7.*S1+8.*S1*(2.*AS+DSIN(2*AS))/PI
     1        -4.*(AS-4*DSIN(4.*AS))/PI)/(9*(1.+S1)**2.))
            
        A2=2.6*A1-2.6*A1
         A3=A2
         B1=1.3
         B2=SK
         B3=SK
         IF(NC.NE.0)THEN
            RL1=RL*1.E2
CCMCM
       W1=A2+RL1**2/12
       W2=A2/(RL1)+RL1/3
            CALL SELEK(A1,B1,W1,W2,RL1,E,RI,DK)
            IF(NC.EQ.6)THEN
                   IF(ILTD0.LT.1)THEN
                  IF(IGK.EQ.1.or.igk.eq.100)THEN
                  READ(13)DK
                  ELSE IF(IGK.EQ.3)THEN
                  READ(13)DK
                  ELSE IF(IGK.EQ.5)THEN
                  READ(14)DK
                  END IF
                   ELSE
                  IF(IGK.EQ.0.or.igk.eq.100)THEN
                  READ(13)DK
                  ELSE IF(IGK.EQ.3)THEN
                  READ(13)DK
                  ELSE IF(IGK.EQ.5)THEN
                  READ(14)DK
                  END IF
                   END IF
            END IF
            CALL SROT(X,Y,Z,ROT)

         ELSE
            PU1=FI+DSIN(FI)
            PU2=FI-DSIN(FI)
            PU3=(FI*DCOS(FI)-DSIN(FI))/2
            PU5=(FI-DSIN(FI)*DCOS(FI))/2
            PU6=2*DSIN(FI/2)-FI*DCOS(FI/2)
            PU4=-PU6*DSIN(FI/2)
            PU7=PU2+4*PU4/FI
            PU8=PU6/FI
            RC1=RC*1.E2
            RC2=RC1*RC1
c               RC2=RC*RC*1.E4
            FI2=FI/2
            C1=(8*B1*DSIN(FI2)**2)/(B1*PU1+B2*PU2)
            C2=DCOS(FI2)-C1/(2*DSIN(FI2))
            D1=2/(RC1*(A1*PU1+A2*PU2+B2*RC2*PU7))
            D2=2/(RC1*(A1*PU2+A2*PU1+B2*RC2*PU2))
            D3=1/(RC1*(A3*FI+B1*RC2*(FI-C1)))
            C3=C2*D3
            C4=C3*DSIN(FI2)
            D4=2/(RC1*(B1*PU1+B2*PU2))+RC2*C2*C3
            D5=2/(RC1*(B1*PU2+B2*PU1))+D3*RC2*DSIN(FI2)**2
            D6=1/(B3*RC1*FI)+RC2*(D2*DSIN(FI2)**2+D1*PU8**2)
c               D5=D5/1.0E4
c               D6=D6/1.0E4
c               D4=D4/1.0E4
            CALL BELEK(D1,D2,D3,D4,D5,D6,FI,RC1,
     +                               PU8,E,RI,DK,C3,C4)
            CALL BROT(X,Y,Z,RL,ROT,QX,QY,QZ,QL)
            C5=(D1*RC1**2/2)*((A1-A2)*PU3+
     +                             B3*RC2*(PU3-PU2-8*PU4/FI))
            C6=(D2*RC1**2/2)*((A1-A2)*PU3+B3*RC2*(PU2+PU3))
            C7=-RC1**2*PU6/2
            C8=RC1**2*(DCOS(FI2)+FI*DSIN(FI2)/
     +                    2-2*(B1*PU6+2*B2*SIN(FI2))/
     1                  (B1*PU2+B2*PU1))
            C9=-C7-RC1*C6*DSIN(FI2)

            C10=RC1*(C5+2*RC1)*PU8-RC1**2*FI*DSIN(FI2)/2

      END IF
      CALL JZZZ(ROT,T2)
      CALL JZCH(DK,T2,T1)
      CALL JZCH(ROT,T1,DK)
      QX=X
      QY=Y
      QZ=Z
      QL=RL
      SC(2,6)=X*1.E2
      SC(3,4)=Y*1.E2
      SC(1,5)=Z*1.E2
      SC(3,5)=-X*1.E2
      SC(1,6)=-Y*1.E2
      SC(2,4)=-Z*1.E2
      AA=0.
      WRITE(7)SC,ROT,DK
      IF(NC.NE.0)THEN
        WRITE(8)NC,RL,E0,ET,RI,A1,ALF,GGZ,GW1X,GW1Y,GW1Z,GW2,T0,TJ,WT,
     1            OD,RK,AA,AA,AA,AA,AA,AA,AA,AA,SA,IS,IM,PJ,XM0,XMT
      ELSE
        WRITE(8)NC,RC,E0,ET,RI,FI,ALF,GGZ,GW1X,GW1Y,GW1Z,GW2,T0,TJ,WT,
     1            OD,RK,PU8,C5,C6,C7,C8,C9,C10,D1,SA,IS,IM,PJ,XM0,XMT
      END IF

      DO 110 IIII=1,4
            GOTO(60,70,80,90),IIII
60              IF(IMH.EQ.0)GOTO 110
            ISU=IMH
            JSU=IMH
            IF(LX1.GT.200.AND.LX1.LT.300)THEN
                  YZ=1.E13
                  DO 77 IZ=1,6
                  DO 77 JZ=1,6
77                      ZJDK(IZ,JZ)=0.
                  IF(LX1.EQ.220)THEN
                        ZJDK(2,2)=YZ
                  ELSE
                        IU=LX1-200
                        IV=IU/10
                        IW=IU-IV*10
                        IH=0
                        JH=0
                        DO 55 I=1,7
                        IF(IV.EQ.I)THEN
                        JH=1
                  IF(I.EQ.1.OR.I.EQ.4.OR.I.EQ.5.OR.I.EQ.7)ZJDK(1,1)=YZ
                  IF(I.EQ.2.OR.I.EQ.4.OR.I.EQ.6.OR.I.EQ.7)ZJDK(2,2)=YZ
                  IF(I.EQ.3.OR.I.EQ.5.OR.I.EQ.6.OR.I.EQ.7)ZJDK(3,3)=YZ
                        END IF
                        IF(IW.EQ.I)THEN
                        IH=1
                  IF(I.EQ.1.OR.I.EQ.4.OR.I.EQ.5.OR.I.EQ.7)ZJDK(4,4)=YZ
                  IF(I.EQ.2.OR.I.EQ.4.OR.I.EQ.6.OR.I.EQ.7)ZJDK(5,5)=YZ
                  IF(I.EQ.3.OR.I.EQ.5.OR.I.EQ.6.OR.I.EQ.7)ZJDK(6,6)=YZ
                        END IF
                        IF(IH.EQ.1.AND.JH.EQ.1)GOTO 66
55                              CONTINUE
                  END IF
66              CONTINUE

                  CALL JZZZ(ROT,T2)
                  CALL JZCH(ZJDK,T2,T1)
                  CALL JZCH(ROT,T1,ZJDK)
                  CALL JZAD(DK,ZJDK,TT)
                  GOTO 100
            END IF

            CALL JZCC(DK,TT)
            GOTO 100


70              IF(ISH.EQ.0)GOTO 110
            ISU=ISH
            JSU=ISH
            CALL JZCH(DK,SC,T)
            CALL JZZZ(SC,T1)
            CALL JZCH(T1,T,TT)
            GOTO 100



80           IF(.NOT.(ISH.GT.0.AND.IMH.GT.0.AND.IMH.GT.ISH))GOTO 110
            ISU=IMH
            JSU=ISH
            CALL JZCH(DK,SC,T)
            CALL JZZF(T,TT)
            GOTO 100


90           IF(.NOT.(ISH.GT.0.AND.IMH.GT.0.AND.ISH.GT.IMH))GOTO 110
            ISU=ISH
            JSU=IMH
            CALL JZZZ(SC,T)
            CALL JZCH(T,DK,T1)
            CALL JZZF(T1,TT)



100             DIA=.FALSE.
            IF(ISU.EQ.JSU)DIA=.TRUE.
            I=6*(ISU-1)+1
            J=6*(JSU-1)+1
            KK=6*KQR(ISU)
            LI=(I-1)*I/2+J
            IF(ISU.GT.1)LI=LI-36*KQL(ISU-1)-KK
            DO 104 II=1,6
                  L=LI
                  JUP=6
                  IF(DIA)JUP=II
                  DO 102 JJ=1,JUP
                        ZK(L)=ZK(L)+TT(II,JJ)
                        L=L+1
102                     CONTINUE
                  LI=LI+I-KK
                  I=I+1
104             CONTINUE
110        CONTINUE

120     CONTINUE
      do 876 i=1,nf0
876        WRITE(12)ZK(i)
555     FORMAT(//,6F12.1)
      if((igk.eq.1.AND.ILTD0.LT.1).OR.(IGK.EQ.0.AND.ILTD0.GE.1))then
      ij=0
      call cprint(1)
      write(1,5559)
      WRITE(1,5558)
5559    format(//,30x,'PIPING PARAMETERS',/)
5558    FORMAT(' No ','OD',6X,'WT',4X,'TJ',6X,'T0',6X,'PJ',
     +3X,'SIGMA0',
     +2X,'SIGMAT',2X,'E0',6X,'ET',6X,'ALPHA',5X,'WG',/)
5551    continue
      ij=ij+1
       if(ij.eq.1.or.ij.eq.3.or.ij.eq.5.or.ij.eq.7.or.ij.eq.9.or.
     +      ij.eq.11.or.ij.eq.13.or.ij.eq.15.or.ij.eq.17.or.
     +      ij.eq.19.or.
     +      ij.eq.21.or.ij.eq.23.or.ij.eq.25.or.ij.eq.27.or.
     +      ij.eq.29)then
       close(31)
       open(unit=31,file='fort40',status='old')
       close(32,STATUS='delete')
       open(unit=32,file='fort41',status='new')
       read(31,5550,end=5554)
     +       dw1,s1,tj1,t01,pj1,xm01,xmt1,e01,et1,alf1,ggz1
       else
       close(32)
       open(unit=32,file='fort41',status='old')
       close(31,STATUS='delete')
       open(unit=31,file='fort40',status='new')
       read(32,5550,end=5554)
     +       dw1,s1,tj1,t01,pj1,xm01,xmt1,e01,et1,alf1,ggz1
       end if
        dw2=dw1*10
        s2=s1*10
        if(isrdw.ne.1)then
        xm02=xm01*9.807
        xmt2=xmt1*9.807
        e02=e01*98.07
        et2=et1*98.07
        pj2=pj1*9.807/100
        else
        xm02=xm01
        xmt2=xmt1
        e02=e01
        et2=et1
        pj2=pj1
        end if
      write(1,5552)
     +ij,dw2,s2,tj1,t01,pj2,xm02,xmt2,e02,et2,alf1,ggz1
      if(iprn.eq.2)then
       ngd1=51
       ngd2=52
       write(23,4385)
     + ngd1,ij,izbxz,dw2,s2,tj1,t01,pj2,pj2,'-','-'
       write(23,4385)
     + ngd2,ij,izbxz,xm02,xmt2,e02,et2,alf1,ggz1,'-','-'
4385    format(i5,i7,i7,6f15.3,1x,a13,1x,a13)
      ELSE if(iprn.eq.3)then
       ngd1=70
       ngd2=71
       write(50,5005)
     + ngd1,ij,tj1,t01,pj2,xm02,xmt2,e02,et2,alf1
       write(50,5006)
     + ngd2,ij,dw2,s2,ggz1,'-',F,F,F,F
5005    format(2I12,8F12.3)
5006    FORMAT(2I12,3F12.3,2X,A10,4F12.3)
      else if(iprn.eq.1)then
       ngd1=71
       ngd2=72
       write(23,201)ngd1,ngd1,ij,izbxz,dw2,s2,tj1,t01,pj2,pj2
       write(23,201)ngd2,ngd2,ij,izbxz,xm02,xmt2,e02,et2,alf1,ggz1
201     FORMAT(2I5,2I10,6F15.3)
      end if
5552    format(i3,5f7.2,4f8.3,f7.3,f7.1)
       do 5553 i=1,99000
       if(ij.eq.1.or.ij.eq.3.or.ij.eq.5.or.ij.eq.7.or.ij.eq.9.or.
     +      ij.eq.11.or.ij.eq.13.or.ij.eq.15.or.ij.eq.17.or.
     +      ij.eq.19.or.
     +      ij.eq.21.or.ij.eq.23.or.ij.eq.25.or.ij.eq.27.or.
     +      ij.eq.29)then
          read(31,5550,end=5551)
     +           dw,s,tj,t0,pj,xm0,xmt,e0,et,alf,ggz
          if(dw1.eq.dw.and.s1.eq.s.and.tj1.eq.tj.and.t01.eq.t0.and.
     +         pj1.eq.pj.and.xmt1.eq.xmt
     +         )then
             aa=1
          else
          write(32,5550)dw,s,tj,t0,pj,xm0,xmt,e0,et,alf,ggz
          end if
       else
          read(32,5550,end=5551)
     +           dw,s,tj,t0,pj,xm0,xmt,e0,et,alf,ggz
          if(dw1.eq.dw.and.s1.eq.s.and.tj1.eq.tj.and.t01.eq.t0.and.
     +         pj1.eq.pj.and.xmt1.eq.xmt
     +         )then
             aa=1
          else
          write(31,5550)dw,s,tj,t0,pj,xm0,xmt,e0,et,alf,ggz
          end if
       end if
5553     continue
5554     continue
      close(31,STATUS='delete')
      close(32,STATUS='delete')
      open(unit=31,file='fort31',form='unformatted',status='new')
      open(unit=32,file='fort32',form='unformatted',status='new')
      end if
      RETURN
      END     
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    作者:       李  智                                   *
c*    程序名:                                              *
c*                SROT                                  *
c*    功能:                                                *
c*                直单元矩阵旋转                    *
c*    被调用程序:                                          *
c*                STRUCTK                                  *
c*    调用程序:                                            *
c*                无                                        *
c*    读取文件:                                            *
c*                无                                        *
c*    写入文件:                                            *
c*                无                                        *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C****************** Z  R  O  T ****************

      SUBROUTINE SROT(X,Y,Z,ROT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION ROT(6,6)
      
      RL=DSQRT(X**2+Y**2+Z**2)
      X1=X/RL
      X2=Y/RL
      X3=Z/RL
      Y1=1.
      Y2=0.
      Y3=0.
      IF(X.EQ.0.0.AND.Z.EQ.0.0)GOTO 10
      Y1=-(X*Y)/RL**2
      Y2=1-Y**2/RL**2
      Y3=-Y*Z/RL**2
      YR=DSQRT(Y1**2+Y2**2+Y3**2)
      Y1=Y1/YR
      Y2=Y2/YR
      Y3=Y3/YR
10      Z1=X2*Y3-X3*Y2
      Z2=X3*Y1-X1*Y3
      Z3=X1*Y2-X2*Y1
      ROT(1,1)=X1
      ROT(2,1)=X2
      ROT(3,1)=X3
      ROT(1,2)=Y1
      ROT(2,2)=Y2
      ROT(3,2)=Y3
      ROT(1,3)=Z1
      ROT(2,3)=Z2
      ROT(3,3)=Z3
      DO 20 I=1,3
      DO 20 J=1,3
20      ROT(I+3,J+3)=ROT(I,J)
      RETURN
      END
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    作者:       李  智                                   *
c*    程序名:                                              *
c*                BROT                                  *
c*    功能:                                                *
c*                弯单元矩阵旋转                             *
c*    被调用程序:                                          *
c*                STRUCTK                                  *
c*    调用程序:                                            *
c*                无                                        *
c*    读取文件:                                            *
c*                无                                        *
c*    写入文件:                                            *
c*                无                                        *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C---------------------------W R O T-------------------------------

      SUBROUTINE BROT(X,Y,Z,RL,ROT,QX,QY,QZ,QL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)  
      DIMENSION ROT(6,6)
      X1=X/RL
      X2=Y/RL
      X3=Z/RL
      Z1=QY*Z-QZ*Y
      Z2=QZ*X-QX*Z
      Z3=QX*Y-QY*X
      QL=DSQRT(Z1**2+Z2**2+Z3**2)
      Z1=Z1/QL
      Z2=Z2/QL
      Z3=Z3/QL
      Y1=Z2*X3-Z3*X2
      Y2=Z3*X1-Z1*X3
      Y3=Z1*X2-Z2*X1
      ROT(1,1)=X1
      ROT(2,1)=X2
      ROT(3,1)=X3
      ROT(1,2)=Y1
      ROT(2,2)=Y2
      ROT(3,2)=Y3
      ROT(1,3)=Z1
      ROT(2,3)=Z2
      ROT(3,3)=Z3
      DO 20 I=1,3
      DO 20 J=1,3
20      ROT(I+3,J+3)=ROT(I,J)
      RETURN
      END
            
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    作者:       李  智                                   *
c*    程序名:                                              *
c*                SELEK                                  *
c*    功能:                                                *
c*                建立直单元刚度矩阵                         *
c*    被调用程序:                                          *
c*                STRUCTK                                  *
c*    调用程序:                                            *
c*                无                                        *
c*    读取文件:                                            *
c*                无                                        *
c*    写入文件:                                            *
c*                无                                        *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C***************Z    D    K************

      SUBROUTINE SELEK(A1,B1,W1,W2,RL,E,RII,DK)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION DK(6,6)
      EC=E*RII
            DK(1,1)=EC/(A1*RL)
       EW=EC/(W1*RL)
            DK(2,2)=EW
            DK(3,3)=EW

            DK(4,4)=EC/(B1*RL)
            WC=W2*EC/W1
            DK(5,5)=WC
            DK(6,6)=WC
            CW=-EC/(2*W1)
            DK(2,6)=CW
            DK(6,2)=CW
            DK(3,5)=-CW
            DK(5,3)=-CW
      RETURN
      END
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    作者:       李  智                                   *
c*    程序名:                                              *
c*                BELEK                                  *
c*    功能:                                                *
c*                建立弯单元刚度矩阵                         *
c*    被调用程序:                                          *
c*                STRUCTK                                  *
c*    调用程序:                                            *
c*                无                                        *
c*    读取文件:                                            *
c*                无                                        *
c*    写入文件:                                            *
c*                无                                        *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C************W     D    K*************

      SUBROUTINE BELEK(D1,D2,D3,D4,D5,D6,FI,RC,PU8,E,RII,DK,C3,C4)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION DK(6,6)
      EC=E*RII

      R=RC
      DK(1,1)=EC*D1
      DK(2,2)=EC*D2
      DK(3,3)=EC*D3
      DK(4,4)=EC*D4
      DK(5,5)=EC*D5
      DK(6,6)=EC*D6
      DP=D1*R*PU8*EC
      DK(1,6)=DP
      DK(6,1)=DP
      DR=-D2*R*DSIN(FI/2)*EC
      DK(2,6)=DR
      DK(6,2)=DR
      DS=C3*R*EC
      DK(3,4)=DS
      DK(4,3)=DS
      DC=D3*R*DSIN(FI/2)*EC
      DK(3,5)=DC
      DK(5,3)=DC
      RE=C4*R*R*EC
      DK(4,5)=RE
      DK(5,4)=RE
      RETURN
      END



