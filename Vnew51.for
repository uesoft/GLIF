C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    作者:       李  智                                    *
C*    程序名:                                               **
C*                ANSELE                                   *
C*    功能:             *  　                               *
C*                由解得的位移求内力  节点力*                  *
C*    被调用程序:             *                             *
C*                SANS                                    *
C*    调用程序:             　                              *
c*                CPRINT                                  *
c*                VECTOR                                  *
c*                SELEF                                   *
c*                BELEF                                   *
c*                ANCHORS                                 *
C*    读取文件:               　                             *
c*            通道号      内容  　                           *
c*            7          转置矩阵,旋转矩阵,单元刚度矩阵         *
c*            8          STRUCK处理后的内部原始数据           *
c*            4          单元始末端自重力向量或末端热胀力向量    *
c*            31         1工况分配荷载                       *
c*            32         2工况热胀荷载                       *
c*            15         3工况(松冷)荷载                     *
c*            33         热位移                             *
c*            20         并联数，串联数，弹簧号，压缩值         *
c*            19         ENCODE DECODE 数据文本转换用        *
c*            11         全部标准输出应力                    *
c*    写入文件:                                       　     *
c*            19         ENCODE DECODE 数据文本转换用        *
c*            1          输出分析数据                        *
c*            23         供数据处理程序用的输出分析数据         *
c*            9          冷态吊零时存支反力                   *
c*            31         1工况分配荷载                       *
c*            32         2工况热胀荷载                       *
c*            15         3工况(松冷)荷载                     *
c*            33         热位移                             *
c*            18         全部标准输出应力(冷态吊零时由11转储)    *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    修改或增加                                         　  *
c*    修增者:                                             　 *
c*    修增日期:                                           　 *
c*    修增内容:                                           　 *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       SUBROUTINE ANSELE(IFD,JSH,JMH,NFZ,JSIX,D,
     1     EP2,PZF,PSZ,NCDH,JCTAB,LX,ITH,psl,thl)

      IMPLICIT DOUBLE PRECISION(
     +A,B,C,D,E,F,G,H,O,P,Q,R,S,T,U,V,W,X,Y,Z)
      CHARACTER AUNIT*15,AFX*2,ath*18,auy*18
      common/scnd/isc,iscnd(50),iscgk(50),iscis(50,50),iscim(50,50),
     +f1(50,50,6),f2(50,50,6)
      common/prin/iprn,iprn1,isz,iltd0,izh
      COMMON/SRST/ISTDH(20000),RYLQ(20000),RKWJ(20000)
      COMMON/SCST/NST,JST(50),YLJQ(50)
      COMMON/GK/IGK,IFJ5,IFJ6,IFJ7,IFJ8,IFJ9,IFJ10
      common/sz/zgz1,zsz1
c       COMMON/NXHXS/FXHXS,IZBXZ,NNDS,NNDB
      COMMON/NXHXS/FXHXS,IZBXZ,NNDS,NNDB,NRG
      COMMON/KZKG/KZDW
      COMMON/ZLHZ/ZZL,ZHZ,E0,ET,ZSZ,ZSZZ,ZZLZ,ZHZZ
      COMMON/GXYL/ZXXM,DLXM,XMNY      
      COMMON/LLGD/NLL,LL(50),RLL(50,3)
      COMMON/SCHZ/NHZ,IHZ(300),JHZ(300),HZ(300,6),
     +nhz5,ihz5(300),hz5(300,6)
c      COMMON/SCHZ/NHZ,IHZ(50),JHZ(50),HZ(50,6)
      COMMON/LLSC/DKQZ(20,4),DKHZ(20,3),SCXSB(20)
      COMMON/TLSC/CLTLZ(40,7),CRTLZ(40,7),
     +              SLTLZ(40,7),SRTLZ(40,7),ZZTLZ(40,7),
     +              QTTLZ(40,7)
      common/szsz/rsz(6000)
	common/disk/lth,paz(6000)
      DIMENSION JSH(NFZ),JMH(NFZ),PZF(JSIX),PSZ(JSIX),psl(jsix)
      DIMENSION D(JSIX),PB(30,6),thl(ncdh)
      DIMENSION DK1(6,6),DK2(6,6),DK3(6,6),DK4(6,6),DK(6,6)
      DIMENSION SC(6,6),SCT(6,6),ROT(6,6)
C,PS2(6),PM2(6)
      DIMENSION D1(6),D2(6),RZL(6),RZLS(6),W(6)
      DIMENSION GW1(6),GW2(6),PS(6),PM(6),PS1(6),PM1(6)
      DIMENSION P1(6),P2(6),P3(6),P4(6),WG(6),NDH(0:20)
C,QS(150,6),qm(150,6)
      DIMENSION JCTAB(NCDH),LX(NFZ),ITH(NCDH)
      SAVE /ZLHZ/,/GXYL/,JD,KLL,ND,ID,LD,NDM,/LLSC/,J2
      IF(KZDW.NE.1)then
         f98=9.807
      else
         f98=1
      end if
      REWIND 4
      REWIND 8
      REWIND 7
      REWIND 9
      REWIND 24
      REWIND 25
      if(igk.eq.3.and.iltd0.ge.1)
     +     open(unit=11,file='fort111',status='new')
c        IF(IGK.EQ.1.OR.IGK.EQ.3)REWIND(20)
            
      NTL=0
      if((igk.eq.3.and.iltd0.lt.1).or.(igk.eq.2.and.iltd0.ge.1))
     +                                                   EP2=-1.0E5
      JT=0
c       DATA H,DT/1.,0.2617994/
      h=1.
      DO 5 I=1,JSIX
            PZF(I)=0.
5       CONTINUE
      IF((IGK.EQ.1.and.iltd0.lt.1).or.igk.eq.0.or.igk.eq.100)THEN
            ZZLZ=0.
            ZHZZ=0.

            ZSZZ=0.
            JD=0
            KLL=0
            ND=0
            ID=0
            LD=0
            NDM=0
            ZSZ=0.
            ZZL=0.
            ZHZ=0.
      END IF
      IK=0
      
      IDS=0
      IDD=0
      IDM=0
      
      MD=0
      
      NDS=0
      NDD=0
      N11=11
      ik=0
      KC=0    
      DO 150 I1=1,NFZ
       if(iprn1.ne.0.and.((igk.ne.2.and.igk.ne.4.and.
     +                      igk.ne.10.and.iltd0.lt.1).or.
     +                      (igk.eq.3.and.iltd0.ge.1.and.
     +                      igk.ne.10)))then
         ik=ik+1
        IF(ik.EQ.1.OR.ik.ge.35)then
           ik=1
                  AUNIT='[Kgf/mm**2]'
         IF(KZDW.NE.1)AUNIT='[MPa]'
         CALL CPRINT(1)
         WRITE(1,131)
         WRITE(1,133)IGK,AUNIT
         WRITE(1,132)
131      FORMAT(//,62X,'STRESS TABLE',/)
133      FORMAT(100X,'CASE No:',I4,5X,A15,/)
132      FORMAT(4X,'START POINT ','   END POINT ',4X,'IJ',11X,
     +   'SS',13X,'SP',10X,
     +   'STRESS-1',7X,'STRESS-2',7X,'STRESS-C',7X,'STRESS-A')

        END IF
        end if
      IF(I1.EQ.1)J2=0
            DO 10 K=1,6
                  D1(K)=0.
                  D2(K)=0.
10              CONTINUE                
            ISH=JSH(I1)
            IMH=JMH(I1)
            READ(7)SC,ROT,DK4
            READ(8)NC,RL,E0,ET,RI,SI,ALF,GGZ,
     +           W1X,W1Y,W1Z,W2,T0,TJ,S,DW,FMM,
     +           A1,A2,A3,A4,A5,A6,A7,A8,FAI,IS,IM,PJ,XM0,XMT
       gsz=3.1415926535*((dw-2*s)/2)**2/10
      if(nc.eq.3.or.nc.eq.2)then
            READ(8)NCh,rex,rex,rex,rex,rex,rex,GGZh,
     +           rex,rex,rex,rex,rex,rex,Sh,DWh,rex,
     +   rex,rex,rex,rex,rex,rex,rex,rex,rex,Iex,Iex,rex,rex,rex
            backspace(8)
           if(nch.ne.nc)then
             dw=dwh
             s=sh
             gzz=ggzh
           else if(nc.eq.2)then
             dw=dw
             s=s
             gzz=gzz
           else
             dw=dwq
             s=sq
             gzz=gzzq
           end if
      end if
            IF(((IGK.EQ.1.OR.IGK.EQ.4).and.iltd0.lt.1).OR.
     + ((IGK.EQ.0.OR.IGK.EQ.3.OR.IGK.EQ.1).AND.ILTD0.GE.1).OR.
     +             IGK.EQ.6.OR.IGK.EQ.7.OR.IGK.EQ.10.OR.IGK.EQ.9.or.
     +             igk.eq.100)
     +             READ(4)GW1,GW2
            IF(((IGK.EQ.2.OR.IGK.EQ.3).AND.ILTD0.LT.1).OR.
     +             ((IGK.EQ.2.OR.IGK.EQ.4).AND.ILTD0.GE.1).OR.
     +               IGK.EQ.8)READ(4)RZL
            IF(ISH.NE.0)THEN                        
                  I=(ISH-1)*6
                  DO 15 K=1,6
                        I=I+K
                        D1(K)=D(I)
                        I=I-K
15                      CONTINUE
      
            END IF
            IF(IMH.NE.0)THEN
                  J=(IMH-1)*6
                  DO 20 K=1,6
                        J=J+K
                        D2(K)=D(J)
                        J=J-K
20                      CONTINUE
            
            END IF

            CALL JZZZ(SC,SCT)
            CALL JZCH(SCT,DK4,DK)
            CALL JZZF(DK,DK2)
            
            CALL JZCH(SCT,DK4,DK)
            CALL JZCH(DK,SC,DK1)

            CALL JZCH(DK4,SC,DK)
            CALL JZZF(DK,DK3)

            CALL JZXL(DK3,D1,P3)
            CALL JZXL(DK4,D2,P4)
            CALL JZXL(DK1,D1,P1)
            CALL JZXL(DK2,D2,P2)
            CALL XLAD(P3,P4,PM)
            CALL XLAD(P1,P2,PS)



            IF(((IGK.EQ.1.OR.IGK.EQ.4).AND.ILTD0.LT.1).OR.
     + ((IGK.EQ.0.OR.IGK.EQ.1.OR.IGK.EQ.3).AND.ILTD0.GE.1).OR.
     +          IGK.EQ.6.OR.IGK.EQ.7.OR.IGK.EQ.10.OR.IGK.EQ.9.or.
     +          igk.eq.100)THEN
                  CALL XLZF(GW2,WG)
                  CALL XLCC(WG,GW2)
                  CALL XLZF(GW1,WG)
                  CALL XLCC(WG,GW1)
                  CALL XLAD(PM,GW2,WG)
                  CALL XLCC(WG,PM)
                  CALL XLAD(PS,GW1,WG)
                  CALL XLCC(WG,PS)
            ELSE IF(((IGK.EQ.2.OR.IGK.EQ.3).AND.ILTD0.LT.1).OR.
     +                  ((IGK.EQ.2.OR.IGK.EQ.4).AND.ILTD0.GE.1).OR.
     +                    IGK.EQ.8)THEN
                  CALL XLAD(PM,RZL,WG)
                  CALL XLCC(WG,PM)
                  CALL JZZF(SCT,DK)
                  CALL JZCC(DK,SCT)
                  CALL JZXL(SCT,RZL,RZLS)
                  CALL XLAD(PS,RZLS,WG)
                  CALL XLCC(WG,PS)
            END IF
c*********************************
c---------add load
      IF(NHZ.gt.0)THEN
      DO 1801 I=1,NHZ
      IF(I.EQ.0)GOTO 1801
      IF     (IS.EQ.IHZ(I).AND.IGK.EQ.JHZ(I))THEN
      DO 1086 J=1,6
1086                    PS(J)=PS(J)-HZ(I,J)/2
      ELSE IF(IM.EQ.IHZ(I).AND.IGK.EQ.JHZ(I))THEN
      DO 1082 J=1,6
1082                    PM(J)=PM(J)-HZ(I,J)/2
      END IF
1801            CONTINUE
      END IF

c------5 case load
      IF(NHZ5.gt.0.AND.IGK.EQ.5)THEN
            DO 1803 I=0,NHZ5
            IF(I.EQ.0)GOTO 1803
            IF     (IS.EQ.IHZ5(I))THEN
                  DO 1085 J=1,6
1085                    PS(J)=PS(J)-HZ5(I,J)/2
            ELSE IF(IM.EQ.IHZ5(I))THEN
                  DO 1084 J=1,6
1084                    PM(J)=PM(J)-HZ5(I,J)/2
            END IF
1803            CONTINUE
      END IF

c*********************************
            

            PS(4)=PS(4)/100.
            PS(5)=PS(5)/100.
            PS(6)=PS(6)/100.
            PM(4)=PM(4)/100.
            PM(5)=PM(5)/100.
            PM(6)=PM(6)/100.
            

       if(iprn.eq.1.and.igk.lt.99)then
         I51=51
         I52=52
         WRITE(23,201)I51,IGK,IS,IM,PS(1),PS(2),PS(3),
     +                              PS(4),PS(5),PS(6)
         WRITE(23,201)I52,IGK,IS,IM,PM(1),PM(2),PM(3),
     +                              PM(4),PM(5),PM(6)
       end if
201     FORMAT(2I5,2I10,6F15.3)
       if(isc.ne.0)then
          do 2010 i23=1,isc
           if(iscnd(i23).eq.im)then
             iscis(i23,igk)=is
             iscim(i23,igk)=im
             f1(i23,igk,1)=pm(1)*f98
             f1(i23,igk,2)=pm(2)*f98
             f1(i23,igk,3)=pm(3)*f98
             f1(i23,igk,4)=pm(4)*f98
             f1(i23,igk,5)=pm(5)*f98
             f1(i23,igk,6)=pm(6)*f98
           end if
2010        continue
       end if


            DO 190 KI=1,50
            IF(IS.EQ.0.OR.IM.EQ.0)GOTO 190
          IF(KZDW.EQ.1)THEN
                  
           IF(IS.EQ.JST(KI))THEN
                        JI=(ISH-1)*6
                        J2=J2+1
C                                DO 24 I=1,6
C                        IF(IGK.EQ.1)QS(J2,I)=PS(I)
C24                      IF(IGK.EQ.2)PS2(I)=PS(I)+QS(J2,I)

            DO 333 K3=1,6
             IF((IGK.EQ.3.AND.ILTD0.LT.1).OR.
     +             (IGK.EQ.2.AND.ILTD0.GE.1))THEN
              D1(K3)=-D(JI+K3)
             ELSE
              D1(K3)=D(JI+K3)
             END IF

333         CONTINUE
           ELSE IF(IM.EQ.JST(KI))THEN
                        JI=(IMH-1)*6
                        J2=J2+2
C                                DO 26 I=1,6
C                                IF(IGK.EQ.1)QM(J2,I)=PM(I)
C26                              IF(IGK.EQ.2)PM2(I)=PM(I)+QM(J2,I)
            DO 334 K3=1,6
             IF((IGK.EQ.3.AND.ILTD0.LT.1).OR.
     +             (IGK.EQ.2.AND.ILTD0.GE.1))THEN
                D1(K3)=-D(JI+K3)
             ELSE
                D1(K3)=D(JI+K3)
             END IF
334         CONTINUE
           END IF
                  
          ELSE
                        DO 25 K=1,6
                              P1(K)=PS(K)*9.807
                              P2(K)=PM(K)*9.807
25                      CONTINUE
                  IF(IS.EQ.JST(KI))THEN
                  JI=(ISH-1)*6
                  J2=J2+1
C                        DO 27 I=1,6
C                        IF(IGK.EQ.1)QS(J2,I)=P1(I)
C27                      IF(IGK.EQ.2)PS2(I)=P1(I)+QS(J2,I)
            DO 335 K3=1,6
             IF((IGK.EQ.3.AND.ILTD0.LT.1).OR.
     +             (IGK.EQ.2.AND.ILTD0.GE.1))THEN

               D1(K3)=-D(JI+K3)
             ELSE
               D1(K3)=D(JI+K3)
             END IF
335         CONTINUE
           ELSE IF(IM.EQ.JST(KI))THEN
                  JI=(IMH-1)*6
                        J2=J2+1
C                                DO 28 I=1,6
C                                IF(IGK.EQ.1)QM(J2,I)=P2(I)
C28                              IF(IGK.EQ.2)PM2(I)=P2(I)+QM(J2,I)
            DO 336 K3=1,6
             IF((IGK.EQ.3.AND.ILTD0.LT.1).OR.
     +             (IGK.EQ.2.AND.ILTD0.GE.1))THEN
              D1(K3)=-D(JI+K3)
             ELSE
              D1(K3)=D(JI+K3)
             END IF
336         CONTINUE
           END IF
          END IF
190     CONTINUE

      IF(((IGK.EQ.1.OR.IGK.EQ.3).AND.ILTD0.LT.1).OR.
     +     ((igk.eq.0.or.IGK.EQ.2.OR.IGK.EQ.3).AND.ILTD0.GE.1).OR.
     +      (IGK.GE.5.and.igk.ne.10).or.
     +       igk.eq.100)THEN

            DO 30 I=1,6
                  W(I)=0.
30              CONTINUE

            PI=3.1415926535
            DN=DW-2.*S
            FX=PI*(DW*DW/4.-DN*DN/4.)
            WX=PI*(DW**4-DN**4)/(32*DW)
C---ST T PIPE OR CUPPLING KWJ DIFERENT
            IF(ISTDH(IS).EQ.2.AND.NC.EQ.2)THEN
                WX=RKWJ(IS)
            ELSE IF(ISTDH(IM).EQ.2.AND.NC.EQ.2)THEN
                WX=RKWJ(IM)
            END IF


            IF((IGK.EQ.1.AND.ILTD0.LT.1).OR.igk.eq.100.or.
     +              IGK.EQ.0.OR.IGK.EQ.7.OR.IGK.EQ.9)THEN
                  W(IZBXZ)=-GGZ
                  if(igk.eq.100)w(izbxz)=-gsz
                  IF(NC.EQ.0)THEN
                        ZR=RL*SI
                        WL=W(IZBXZ)*ZR
                  ELSE
                        WL=W(IZBXZ)*RL
                  END IF
C----------CHECK ZZL AND ZHZ
                        ZZL=ZZL-WL
                        ZHZ=ZHZ+PS(IZBXZ)+PM(iZBXZ)

            ELSE IF(IGK.EQ.6)THEN
                W(1)=W1X
                W(2)=W1Y
                W(3)=W1Z
            END IF
            
            CALL JZZZ(ROT,DK)
            CALL JZCC(DK,ROT)
            CALL JZXL(ROT,PS,PS1)
            CALL JZXL(ROT,PM,PM1)
            CALL JZXL(ROT,W,WG)
            CALL XLCC(WG,W)
c nc.eq.3
       if(igk.ne.0)then
       IF(NC.EQ.1.OR.NC.EQ.2.OR.NC.EQ.3.OR.NC.EQ.5)THEN
       IF(RL.LT.H)THEN
        NNDS=NNDS
       ELSE
        NNDS=NNDS
       END IF
       CALL SELEF(nc,RL,FX,WX,NNDS,IS,
     1            IM,PS1,PM1,W,FMM,XMA,xma1,IJ,NDH,PJ,DW,S,XMT,XM0,
     +            tj,t0)
        if(igk.eq.100)xma=0
        if(igk.eq.100)xma1=0
        if(igk.eq.100)xmt=0
        if(igk.eq.100)xm0=0
       ELSE IF(NC.EQ.0)THEN
        N=NNDB

        cALL BELEF(RL,SI,S,DW,N,IS,IM,FX,WX,PS1,W,
     1                  FMM,XMA,IJ,NDH,PJ,XMT,XM0,tj,t0)
        if(igk.eq.100)xma=0
        if(igk.eq.100)xma1=0
        if(igk.eq.100)xmt=0
        if(igk.eq.100)xm0=0
       END IF
             dwq=dw
              sq=s
               gzzq=gzz
                  
       IF((IGK.EQ.3.AND.ILTD0.LT.1).OR.
     +             (IGK.EQ.2.AND.ILTD0.GE.1))THEN
        IF(XMA.EQ.0.or.tj.lt.430)GOTO 999
        EP=(1.-(XMT/XMA)*(E0/ET))
        if(ep.gt.1)goto 999
        IF(EP.GT.EP2)then
         isep=is
         imep=im
         EP2=EP
        end if
       END IF
      end if
999             IF((IGK.EQ.1.AND.ILTD0.LT.1).OR.
     +              IGK.EQ.0.OR.IGK.GE.5)THEN
                  IF(ISH.EQ.0.OR.IMH.EQ.0)THEN
            CALL ANCHORS(ISH,IMH,IS,IM,EP2,NTL,
     1                  PS,PM,PB)

                  END IF
            END IF
            IF(((IGK.EQ.3.AND.ILTD0.LT.1).OR.
     +              (IGK.EQ.2.AND.ILTD0.GE.1)).AND.
     +               (ISH.EQ.0.OR.IMH.EQ.0))THEN
                  JD=JD+1
                  IF(ISH.EQ.0)THEN
                  
                        DO 65 K=1,6
                              IF(KZDW.EQ.1)THEN
                                    PB(JD,K)=PS(K)
                              ELSE
                                    PB(JD,K)=PS(K)*9.807
                              END IF
65                              CONTINUE
                  END IF
                  IF(IMH.EQ.0)THEN
                        
                        DO 75 K=1,6
                              IF(KZDW.EQ.1)THEN
                                    PB(JD,K)=PM(K)
                              ELSE
                                    PB(JD,K)=PM(K)*9.807
                              END IF
75                              CONTINUE
                  END IF
            END IF
      WS=PI*DN*DN/40
            DO 85 I=1,6
                  K=6*(ISH-1)+I
                  J=6*(IMH-1)+I
                        PZF(K)=PZF(K)-PS(I)
                        PZF(J)=PZF(J)-PM(I)
85          CONTINUE

                                          
      
      ELSE
                  
            DO 95 I=1,6
                  K=6*(ISH-1)+I
                  J=6*(IMH-1)+I
                  PZF(K)=PZF(K)-PS(I)
                  PZF(J)=PZF(J)-PM(I)
95              CONTINUE
            IF(ISH.EQ.0.OR.IMH.EQ.0)THEN
            CALL ANCHORS(ISH,IMH,IS,IM,EP2,NTL,
     1                  PS,PM,PB)
            END IF

            IF((IGK.EQ.4.AND.ILTD0.LT.1).OR.
     +             (IGK.EQ.1.AND.ILTD0.GE.1))THEN
            DO 100 I=1,20
            IF(IM.EQ.LL(I))THEN
            KLL=KLL+1
            J=(IMH-1)*6
            DKQZ(KLL,1)=IM
            DO 110 K=1,3
            J=J+K
            DKHZ(KLL,K)=D(J)
            J=J-K
            DKHZ(KLL,K)=DKHZ(KLL,K)
            DKQZ(KLL,K+1)=DKHZ(KLL,K)+RLL(I,K)
110             CONTINUE
            END IF
100             CONTINUE
            END IF
      END IF
150     CONTINUE

      IF((IGK.EQ.1.AND.ILTD0.LT.1).OR.IGK.EQ.0)WRITE(9)PZF

      IF(((IGK.EQ.3.AND.ILTD0.LT.1).OR.
     +      (IGK.EQ.2.AND.ILTD0.GE.1)).AND.EP2.GT.0.)THEN
            REWIND(8)
            DO 160 I=1,NFZ
                  ISH=JSH(I)
                  IMH=JMH(I)
            READ(8)NC,RL,E0,ET,RI,SI,ALF,GGZ,W1X,W1Y,W1Z,
     +                          W2,T0,TJ,S,DW,FMM,
     1                          A1,A2,A3,A4,A5,A6,A7,A8,FAI,IS,IM

                  IF(ISH.EQ.0.OR.IMH.EQ.0)THEN
                  CALL ANCHORS(ISH,IMH,IS,IM,EP2,NTL,
     1                  PS,PM,PB)
                  END IF
160             CONTINUE
      END IF
      IF((IGK.EQ.3.AND.ILTD0.LT.1).OR.
     +     (IGK.EQ.2.AND.ILTD0.GE.1))THEN
      SCXSB(IFD)=EP2
      END IF

      I20=20
      IK=0
      if(((igk.eq.3.or.igk.eq.4).AND.ILTD0.LT.1).or.
     +     ((IGK.EQ.2.OR.IGK.EQ.1).AND.ILTD0.GE.1).OR.
     +       igk.eq.5.or.igk.eq.6.or.igk.eq.8)then
           IF(IGK.EQ.5.OR.IGK.EQ.6.OR.IGK.EQ.8)THEN
             REWIND(33)
             READ(33)PSZ
           END IF
      DO 151 I1=1,NFZ
            IMH=JMH(I1)
            NMH=JCTAB(IMH)
            ILX=LX(I1)
            IF(IMH.NE.0)THEN
            IF(ILX.NE.100)then
            IK=IK+1
           if((ik.eq.1.or.ik.ge.35).and.igk.gt.4)then
           ik=1
           AUNIT = '[mm,mrad]'
           CALL CPRINT(1)
             WRITE(1,171)
             WRITE(1,179)IGK,AUNIT
             WRITE(1,173)
         END IF
            end if
                  J=(IMH-1)*6
                  DO 21 K=1,6
                        J=J+K
                        IF((IGK.EQ.3.AND.ILTD0.LT.1).OR.
     +                             (IGK.EQ.2.AND.ILTD0.GE.1))THEN
                         D2(K)=-D(J)*10.
                         IF(K.GE.4)D2(K)=-D(J)*1000.
                        ELSE
                         IF(IGK.EQ.5.OR.IGK.EQ.6.OR.
     +                                          IGK.EQ.8)THEN
                           DAD=PSZ(J)
                         ELSE
                           DAD=0.
                         END IF
                         D2(K)=(D(J)-DAD)*10.
                         IF(K.GE.4)D2(K)=(D(J)-DAD)*1000.
                        END IF
                        J=J-K
21                      CONTINUE

            END IF
       IF(ILX.NE.100.and.igk.gt.4)WRITE(1,161)NMH,ILX,(D2(K),K=1,6)
ccccc      10yue8rijiagai
cccccexa      IF(IGK.GE.5)WRITE(23,4385)ngd,IDH,idh,(QTTLZ(I,K),K=2,7),' -',' -'
      if(iprn.eq.2)WRITE(23,4385)I20,IGK,NMH,(D2(K),K=1,6),' -',' -'
      if(iprn.eq.1)WRITE(23,201)I20,IGK,NMH,ILX,(D2(K),K=1,6)
CCCCC            if(iprn.eq.3)WRITE(50,201)I20,IGK,NMH,ILX,(D2(K),K=1,6)

       if(isc.ne.0)then
          do 2020 i23=1,isc
           if(iscnd(i23).eq.nmh)then
             f2(i23,igk,1)=d2(1)
             f2(i23,igk,2)=d2(2)
             f2(i23,igk,3)=d2(3)
             f2(i23,igk,4)=d2(4)
             f2(i23,igk,5)=d2(5)
             f2(i23,igk,6)=d2(6)
           end if
2020        continue
       end if

151      CONTINUE
            IF((IGK.EQ.3.AND.ILTD0.LT.1).OR.
     +             (IGK.EQ.2.AND.ILTD0.GE.1))THEN
              REWIND(33)
              WRITE(33)D
            END IF
       end if
161      FORMAT(2I5,6F12.2,1X,A2)
171      FORMAT(//,27X,'DISPLACEMENT OF RESTRAINTS',/)
1713     FORMAT(//,47X,'DISPLACEMENT OF RESTRAINTS (WORK-STATUS)',/)
1714     FORMAT(//,47X,'DISPLACEMENT OF RESTRAINTS (COLD-STATUS)',/)
179      format(60X,'CASE No: ',I4,5X,A15,/)
1783     FORMAT(40X,'CR-SCALE: ',F6.3,5X,'START:',I5,5X,'END:',I5,/)
172      FORMAT(//,31X,'LOAD OF RESTRAINTS',/)
1784     format(//,21X,'LOAD OF RESTRAINTS (COLD&RELEASE)',/)
177      FORMAT(//,27X,'STRUCTURE LOAD OF RESTRAINTS',/)
277      FORMAT('POINT',' TYPE  ',1X,'1C-LOAD',5X,
     +  'IN-LOAD',5X,'WK-LOAD',2X,'CR-LOAD/SCALE',
     +   2X,'WT-LOAD',5X,'ST-LOAD')
174      FORMAT('POINT',' TYPE',7X,'FX',10X,
     +          'FY',10X,'FZ',10X,'MX',10X,'MY',10X,'MZ')
173      FORMAT('POINT',' TYPE  ',5X,'DX',10X,
     +          'DY',10X,'DZ',10X,'SX',10X,'SY',10X,'SZ')
         ICD=10
         IK=0
                  AUNIT='[Kgf,Kgf-m]'
         if(kzdw.ne.1)AUNIT='[N,N-m]'
      if(((igk.eq.2.or.igk.eq.4).AND.ILTD0.LT.1).OR.
     +     ((IGK.EQ.1.OR.IGK.EQ.2.OR.IGK.EQ.4).AND.ILTD0.EQ.1))then
          rewind(31)
          read(31)psz
      else if(igk.eq.8.or.igk.eq.5.or.igk.eq.6)then
          rewind(32)
          read(32)psz
      end if
      DO 170 I=1,NFZ
         IMH=JMH(I)
         ILX=LX(I)
      IF(ILX.NE.100.AND.ILX.NE.200.AND.ILX.NE.300.AND.ILX.NE.400)then
C            IF(((IGK.NE.3.and.igk.ne.2.and.igk.ne.4.and.igk.ne.1)THEN
           IF(IGK.GE.5.and.igk.ne.100)THEN
             IF(IGK.NE.7.AND.IGK.NE.10)IK=IK+1
             if(ik.eq.1.or.ik.ge.35)then
             ik=1
             CALL CPRINT(1)
             WRITE(1,172)
             WRITE(1,179)IGK,AUNIT
             WRITE(1,174)
             END IF
          END IF

         IDH=JCTAB(IMH)
         J=(IMH-1)*6
                  DO 31 K=1,6
                        J=J+K
        if(((igk.eq.2.or.igk.eq.4).AND.ILTD0.LT.1).or.
     +       ((IGK.EQ.1.OR.IGK.EQ.2.OR.IGK.EQ.4).AND.ILTD0.GE.1).OR.
     +       igk.eq.5.or.igk.eq.6.or.igk.eq.8)then
           pzf(j)=pzf(j)+psz(j)
         end if
                        IF(IGK.EQ.3)THEN
                         D2(K)=-PZF(J)
                        ELSE
                         D2(K)=PZF(J)
                        END IF
                         IF(KZDW.NE.1)D2(K)=D2(K)*9.807
                        J=J-K
31                      CONTINUE
         if(iprn.eq.1.and.igk.lt.99)
     +           WRITE(23,201)ICD,IGK,IDH,ILX,(D2(K),K=1,6)
C            if(IGK.NE.3.AND.igk.ne.10.and.igk.ne.7.and.
C     +         igk.ne.1.and.igk.ne.2.and.igk.ne.4)then
           IF(IGK.GE.5.AND.IGK.NE.10.AND.IGK.NE.7.and.
     +          igk.ne.100)THEN
            WRITE(1,161)IDH,ILX,(D2(K),K=1,6)
c            ELSE IF(IGK.EQ.3.AND.EP2.GT.0.AND.
c     +              ILX.GT.100.AND.ILX.LT.1000)THEN
c                D2(1)=D2(1)*EP2
c                D2(2)=D2(2)*EP2
c                D2(3)=D2(3)*EP2
c                D2(4)=D2(4)*EP2
c                D2(5)=D2(5)*EP2
c                D2(6)=D2(6)*EP2
c                WRITE(1,161)IDH,ILX,(D2(K),K=1,6)
          end if
         end if
170     CONTINUE
      if((igk.eq.1.AND.ILTD0.LT.1).OR.IGK.EQ.0)then
          rewind(31)
          write(31)pzf
      else if((igk.eq.2.AND.ILTD0.LT.1).OR.
     +          (IGK.EQ.1.AND.ILTD0.GE.1))then
          rewind(32)
          write(32)pzf
      else if((igk.eq.3.AND.ILTD0.LT.1).OR.
     +          (IGK.EQ.2.AND.ILTD0.GE.1))then
          rewind(15)
          write(15)pzf
      end if
c+++++++standard displacement
      if(igk.eq.4)then
      rewind(20)
      rewind (33)
      read(33)psz
      ik=0
      aunit='[mm]'
      do 260 i=1,nfz
           ILX=LX(I)
           IMH=JMH(I)
           IDH=JCTAB(IMH)
           if(ilx.ne.100)then
             ik=ik+1
           IF(IK.EQ.1.OR.IK.ge.35)then
             ik=1
             CALL CPRINT(1)
             WRITE(1,9877)
             WRITE(1,179)IGK,AUNIT
             WRITE(1,9878)
9877    format(//,33x,'CW-DISPLACEMENT (cold/work-status)')
9878    format('POINT',3x,'TYPE',4x,
     +        'Dcx',4x,'Dcy',4x,'Dcz',5x,
     +        'Dwx',4x,'Dwy',4x,'Dwz',2x,'SP-set',7x,'SP-press',/)
          END IF


           nb=0
           nc=0
           ijth=0
          huy=0
          auy=' -'
          ath=' -'
            JJ=(IMH-1)*6
            JK=JJ+IZBXZ
           if((ilx.gt.80.and.ilx.lt.100).or.
     +           ilx.gt.1000)then
              read(20,*)nb,nc,ijth,huy
           end if
           if(nb.gt.4.or.nb.le.0.or.igk.ne.4)then
              nb=0
              nc=0
              ijth=0
              huy=0
              goto 989
           end if
c        write(1,9876)idh,ilx,nb,nc,ijth,(abs(huy)),
c     +                    (d(jj+1)*10),(d(jj+2)*10),(d(jj+3)*10),
c     +       (-psz(jj+1)*10),(-psz(jj+2)*10),(-psz(jj+3)*10),ath,auy
9876    format(i5,i7,
     +         3f7.0,1x,3f7.0,2x,a13,1x,a13)
4385    format(i5,i7,i7,6f15.3,1x,a13,1x,a13)
         NC2=0
         NC2=NC-2
         IF(NC2.GT.0)THEN
            NC1=NC-NC2
         ELSE
            NC1=NC
         END IF
         nijth1=nc1*100+ijth
         nijth2=nc2*100+ijth
      IF(NB.GE.2)THEN
         IF(NC2.GT.0)THEN

         
C            ENCODE(11,985,AUY)nb,'X','(',IUY1,'+',IUY2,')'
           open(unit=19,file='encode',status='new')
           write(19,981)nb,'X','(',nijth1,'+',nijth2,')'
           backspace 19
           read(19,980)ath 
           close(19,status='delete')
         
         ELSE
C            encode(5,982,ath)nb,'X',NIJTH1
           open(unit=19,file='encode',status='new')
           write(19,982)nb,'X',NIJTH1
           backspace 19
           read(19,980)ath 
           close(19,status='delete')
         END IF
      ELSE
         IF(NC2.GT.0)THEN
C            encode(7,983,ath)NIJTH1,'+',NIJTH2
           open(unit=19,file='encode',status='new')
           write(19,983)NIJTH1,'+',NIJTH2
           backspace 19
           read(19,980)ath 
           close(19,status='delete')
         ELSE
C            encode(3,984,ath)NIJTH1
           open(unit=19,file='encode',status='new')
           write(19,984)NIJTH1
           backspace 19
           read(19,980)ath 
           close(19,status='delete')
         END IF
      END IF
          iiii=0
          iuy= nint(abs(huy))
          IUY1=NINT(ABS(HUY*NC1/NC))
          IUY2=NINT(abs(huy*nc2/nc))
      IF(NB.GE.2)THEN
       IF(NC2.GT.0)THEN
          iiii=iuy-iuy1-iuy2
          iuy1=iuy1+iiii
C            ENCODE(11,985,AUY)nb,'X','(',IUY1,'+',IUY2,')'
           open(unit=19,file='encode',status='new')
           write(19,985)nb,'X','(',IUY1,'+',IUY2,')'
           backspace 19
           read(19,980)auy 
           close(19,status='delete')
       ELSE
C            ENCODE(5,986,AUY)NB,'X',IUY1
           open(unit=19,file='encode',status='new')
           write(19,986)NB,'X',IUY1
           backspace 19
           read(19,980)auy 
           close(19,status='delete')
       END IF
      ELSE
       IF(NC2.GT.0)THEN
          iiii=iuy-iuy1-iuy2
          iuy1=iuy1+iiii
C            ENCODE(7,987,AUY)IUY1,'+',IUY2
           open(unit=19,file='encode',status='new')
           write(19,987)IUY1,'+',IUY2
           backspace 19
           read(19,980)auy
           close(19,status='delete')
       ELSE
C            ENCODE(3,988,AUY)IUY1
           open(unit=19,file='encode',status='new')
           write(19,988)iuy1
           backspace 19
           read(19,980)auy 
           close(19,status='delete')
       END IF
      END IF
981         FORMAT(I1,A1,a1,I3,A1,I3,a1)
982         FORMAT(I1,A1,I3)
983         FORMAT(I3,A1,I3)
984         FORMAT(I3)
985         FORMAT(I1,A1,a1,I3,A1,i3,a1)
986         FORMAT(I1,A1,I3,a1,I3)
987         FORMAT(I3,A1,i3)
988         FORMAT(I3)
989          continue
980         format(a18)
           
           if(ilx.gt.80.and.ilx.lt.100)ilx=ith(imh)
	if(lth.eq.9)then
	ath='*****'
	auy='*****'
	end if
      write(1,9876)idh,ilx,
     +       (d(jj+1)*10),(d(jj+2)*10),(d(jj+3)*10),
     +       (-psz(jj+1)*10),(-psz(jj+2)*10),(-psz(jj+3)*10),ath,auy
              ngd=21
             if(iprn.eq.2)
     +        write(23,4385)ngd,idh,ilx,
     +       (d(jj+1)*10),(d(jj+2)*10),(d(jj+3)*10),
     +       (-psz(jj+1)*10),(-psz(jj+2)*10),(-psz(jj+3)*10),ath,auy
             IF(iprn.eq.3)THEN
              NGD3=20
              write(50,5002)ngd3,IGK,idh,ilx,
     +                (d(jj+1)*10),(d(jj+2)*10),(d(jj+3)*10),
     +       (-psz(jj+1)*10),(-psz(jj+2)*10),(-psz(jj+3)*10)
5002  FORMAT(4I12,6F12.3)     
             END IF
           end if
260     continue
      end if

C+++++++STRUCTURL LOAD
      IF((IGK.EQ.4.AND.ILTD0.LT.1).OR.
     +     (IGK.EQ.4.AND.ILTD0.GE.1))THEN
          rewind(31)
          read(31)psz
          REWIND(32)
          READ(32)D
          rewind(15)
          read(15)psl
          REWIND(8)
          SZ=0
          IK=0
       DO 270 I=1,NFZ
        if(isz.gt.0)then
c           else IF(IFJ9.GT.0)THEN
         READ(8)NC,RL,E0,ET,RI,SI,ALF,GGZ,W1X,W1Y,W1Z,
     +                          W2,T0,TJ,S,DW,FMM,
     +                          A1,A2,A3,A4,A5,A6,A7,A8,FAI,IS,IM
         SZ=3.1415926535*((DW-2*S)/2)**2./10.
         GW=ABS(GGZ)
         else
         gw=ggz
         END IF
          ILX=LX(I)
          IMH=JMH(I)
           IDH=JCTAB(IMH)
           if(ilx.gt.80.and.ilx.lt.100)ilx=ith(imh)
         JLX=100+10*IZBXZ
         KLX=300+10*IZBXZ
         IF(ILX.EQ.JLX.OR.ILX.EQ.KLX.OR.
     +       (ILX.GT.80.AND.ILX.LT.100).or.ilx.ne.100)THEN
           if((ilx.gt.100.and.ilx.lt.400).and.
     +               ilx.ne.jlx.and.ilx.ne.klx)then
             ik=ik+6
           else
             IK=IK+1
           end if
          if(ik.eq.1.or.ik.ge.35)then
             ik=1
             if((ilx.gt.100.and.ilx.lt.400).and.
     +                  ilx.ne.jlx.and.ilx.ne.klx)ik=ik+6
             CALL CPRINT(1)
             WRITE(1,177)
                  AUNIT='[Kgf,Kgf-m]'
         if(kzdw.ne.1)AUNIT='[N,N-m]'
             WRITE(1,179)IGK,AUNIT
             WRITE(1,277)
          END IF
          if(imh.ne.0.and.ilx.gt.100.and.
     +         ilx.ne.jlx.and.ilx.ne.klx.and.ilx.lt.400)then
           do 271 i1i = 1,6
             JJ=(IMH-1)*6
             J=JJ
            JK=JJ+i1i
            if(iltd0.lt.1)then
             FAZ=PZF(JK)
ccccccccccccccccccccccccccccccc
	       if(lth.eq.9.and.ilx.gt.80.and.ilx.lt.100)faz=paz(jk)
ccccccccccccccccccccccccccccccc
             FFP=PSZ(JK)
             FGZ=D(JK)
            else if(iltd0.ge.1)then
             FAZ=D(JK)
             FFP=PSZ(JK)
             FGZ=PZF(JK)
            end if
            if(ep2.gt.0)then
             fxs=-psl(jk)*ep2
            else
             fxs=0
            end if
c              FSZ=FFP*SZ/GW
             fsz=zsz1/zgz1*ffp
            if(isz.gt.0)then
             fsz=rsz(jk)  
            else
             fsz=0
            end if
             Fjghz=abs(FFP)
             IF(abs(FAZ).gT.Fjghz)Fjghz=abs(FAZ)
C               if(izbxz.eq.i1i)FJGHZ=FJGHZ+FSZ
             IF(abs(Fxs).gT.Fjghz)Fjghz=abs(Fxs)
             IF(abs(Fgz).gT.Fjghz)Fjghz=abs(Fgz)
c              IF(IZBXZ.EQ.I1I)FJGHZ=FJGHZ+(FSZ)
             IF(IZBXZ.EQ.I1I)FJGHZ=FJGHZ+abs(FSZ)
        IF(KZDW.NE.1)THEN
                 FFP=9.807*FFP
                 FAZ=9.807*FAZ
                 FGZ=9.807*FGZ
                 FSZ=9.807*FSZ
                 fxs=9.807*fxs
                 FJGHZ=9.807*FJGHZ
              END IF
              IF(I1I.EQ.1)afx='Fx'
              IF(I1I.EQ.2)afx='Fy'
              IF(I1I.EQ.3)afx='Fz'
              IF(I1I.EQ.4)afx='Mx'
              IF(I1I.EQ.5)afx='My'
              IF(I1I.EQ.6)afx='Mz'
              if(izbxz.eq.i1i)fjghz=fjghz+(abs(0.5*ffp))
            WRITE(1,161)IDH,ILX,FFP,FAZ,FGZ,FXS,FSZ,FJGHZ,afx
            if(iprn.eq.2)
     +   WRITE(23,4385)n11,IDH,ILX,FFP,FAZ,FGZ,FXS,FSZ,FJGHZ,AFX,afx
            if(iprn.eq.3)
     +   WRITE(50,5004)n11,afx,IDH,ILX,FFP,FAZ,FGZ,FXS,FSZ,FJGHZ
5004  FORMAT(I12,2X,A10,2I12,6F12.3)     
            if(iprn.eq.1)
     +        WRITE(23,201)N11,I1I,IDH,ILX,FFP,FAZ,FGZ,FXS,FSZ,FJGHZ

271          continue
          else
            JJ=(IMH-1)*6
             J=JJ
            JK=JJ+IZBXZ
           IF(ILTD0.LT.1)THEN
           FAZ=PZF(JK)
ccccccccccccccccccccccccccccccc
	       if(lth.eq.9.and.ilx.gt.80.and.ilx.lt.100)faz=paz(jk)
ccccccccccccccccccccccccccccccc
           FFP=PSZ(JK)
           FGZ=D(JK)
           ELSE IF(ILTD0.GE.1)THEN
           FAZ=D(JK)
           FFP=PSZ(JK)
           FGZ=PZF(JK)
           if(iltd0.ge.1.and.
     +         (ilx.gt.80.and.ilx.lt.100.or.ilx.gt.1000))fgz=thl(imh)
           END IF
c             FSZ=FFP*SZ/GW
           fsz=zsz1/zgz1*ffp
           fsz=rsz(jk)  
            if(isz.gt.0)then
             fsz=rsz(jk)  
            else
             fsz=0
            end if
           IF(ILX.GT.80.AND.ILX.LT.100.or.ilx.gt.1000)THEN
            PAZJG=1.2*FAZ
            PFPJG=1.5*FFP
            if(iltd0.ge.1)pfpjg=1.5*fgz
            IF((ABS(PAZJG)).GT.(ABS(PFPJG)))THEN
              FJGHZ=PAZJG
              FXS=1.2
            ELSE
              FJGHZ=PFPJG
              FXS=1.5
            END IF
           ELSE if(ilx.eq.jlx.or.ilx.eq.klx)then
             FZY=FFP
	       if(fzy.gt.0.0)fzy=-fzy
c	write(*,*)fzy,ilx
             fzy1=0
             fzy2=0
             IF(FAZ.LT.FZY)FZY1=FAZ-fzy
             IF(FGZ.LT.FZY)FZY2=FGZ-fzy
             FJGHZ=1.5*FFP+FZY1+fzy2
             FXS=1.5
           END IF
	        if(fsz.gt.0.0)fsz=-fsz
c	write(*,*)fsz
              FJGHZ=FJGHZ+FSZ
              IF(KZDW.NE.1)THEN
                 FFP=9.807*FFP
                 FAZ=9.807*FAZ
                 FGZ=9.807*FGZ
                 FSZ=9.807*FSZ
                 FJGHZ=9.807*FJGHZ
              END IF
              IF(IZBXZ.EQ.1)afx='Fx'
              IF(IZBXZ.EQ.2)afx='Fy'
              IF(IZBXZ.EQ.3)afx='Fz'
          IF(FFP.GT.0.0001.OR.FAZ.GT.0.0001.OR.FGZ.GT.0.0001)
     +    FJGHZ=9999999999999.999
            WRITE(1,161)IDH,ILX,FFP,FAZ,FGZ,FXS,FSZ,FJGHZ,AFX
            if(iprn.eq.2)
     +   WRITE(23,4385)n11,IDH,ILX,FFP,FAZ,FGZ,FXS,FSZ,FJGHZ,AFX,afx
            if(iprn.eq.3)
     +   WRITE(50,5004)n11,afx,IDH,ILX,FFP,FAZ,FGZ,FXS,FSZ,FJGHZ
            if(iprn.eq.1)
     +      WRITE(23,201)N11,IZBXZ,IDH,ILX,FFP,FAZ,FGZ,FXS,FSZ,FJGHZ
          end if
         END IF
270     CONTINUE
      END IF
c        if(iltd0.ge.1.and.igk.eq.3)then
c            rewind 25
c            rewind 24
c            do 272 i=1,nfz
c272         continue
c        end if
      IF((IGK.EQ.4.AND.ILTD0.LT.1).OR.(IGK.EQ.1.AND.ILTD0.GE.1))THEN
       if(nll.gt.0)then
             AUNIT='[mm]'
             CALL CPRINT(1)
             WRITE(1,176)
             WRITE(1,179)IGK,AUNIT
             WRITE(1,175)

          ICD=24
       DO 180 I=1,KLL
          IDH=DKQZ(I,1)
          DQ1=DKQZ(I,2)*10
          DQ2=DKQZ(I,3)*10
          DQ3=DKQZ(I,4)*10
          DH1=DKHZ(I,1)*10
          DH2=DKHZ(I,2)*10
          DH3=DKHZ(I,3)*10
          ngd=24
          if(iprn.eq.2)
     +      WRITE(23,4385)ngd,idh,IDH,DQ1,DQ2,DQ3,DH1,DH2,DH3,' -',' -'
          if(iprn.eq.3)
     +      WRITE(50,5002)ngd,IGK,idh,IDH,DQ1,DQ2,DQ3,DH1,DH2,DH3
          if(iprn.eq.1)
     +       WRITE(23,201)ICD,IGK,IDH,IDH,DQ1,DQ2,DQ3,DH1,DH2,DH3
          WRITE(1,161)IDH,IDH,DQ1,DQ2,DQ3,DH1,DH2,DH3
180      CONTINUE
       end if
      END IF
175      FORMAT('POINT',' TYPE',6X,'DFX',9X,
     +          'DFY',9X,'DFZ',9X,'DBX',9X,'DBY',9X,'DBZ',/)
176      FORMAT(//,29X,'COLD SPRING JOINT POINT',/)
178      FORMAT(//,25X,'REACTION OF PINING ON ANCHORS',/)
1780      FORMAT(//,26X,
     +'REACTION OF PINING ON ANCHORS (WORK-INITIAL)',/)
1781      FORMAT(//,26X,
     +'REACTION OF PINING ON ANCHORS (COLD-INITIAL)',/)
1782   FORMAT(//,26X,
     +'REACTION OF PIPING ON ANCHORS (COLD-RELEASE)',10X,/)
                      AUNIT='[Kgf,Kgf-m]'
             IF(KZDW.NE.1)AUNIT='[N,N-m]'
             ngd=30+igk
      IF(((IGK.NE.3.AND.ILTD0.LT.1).OR.
     +     ((igk.eq.0.or.igk.eq.1.or.IGK.EQ.4).AND.ILTD0.GE.1)).AND.
     +       IGK.NE.7.AND.IGK.NE.9.and.igk.ne.100)THEN
             CALL CPRINT(1)
             IF(IGK.EQ.2.AND.ILTD0.LT.1)THEN
             WRITE(1,1780)
             ELSE IF(IGK.EQ.4.AND.ILTD0.LT.1)THEN
             WRITE(1,1781)
             ELSE
             WRITE(1,178)
             END IF
             WRITE(1,179)IGK,AUNIT
             WRITE(1,174)
      DO 193 I=1,NTL
           IDH=ZZTLZ(I,1)
           if(iltd0.lt.1)then
           IF(IGK.EQ.1)WRITE(1,161)IDH,IDH,(ZZTLZ(I,K),K=2,7)
           IF(IGK.EQ.2)WRITE(1,161)IDH,IDH,(CRTLZ(I,K),K=2,7)
           IF(IGK.EQ.4)WRITE(1,161)IDH,IDH,(CLTLZ(I,K),K=2,7)
           IF(IGK.GE.5.and.igk.ne.100)
     +                   WRITE(1,161)IDH,IDH,(QTTLZ(I,K),K=2,7)
           if(iprn.eq.2)then
      IF(IGK.EQ.2)WRITE(23,4385)ngd,IDH,idh,(CRTLZ(I,K),K=2,7),' -',' -'
      IF(IGK.EQ.4)WRITE(23,4385)ngd,IDH,idh,(CLTLZ(I,K),K=2,7),' -',' -'
      IF(IGK.GE.5)WRITE(23,4385)ngd,IDH,idh,(QTTLZ(I,K),K=2,7),' -',' -'
           end if
           if(iprn.eq.3)then
           N24=30
      IF(IGK.EQ.2)WRITE(50,5002)n24,IGK,IDH,idh,(CRTLZ(I,K),K=2,7)
      IF(IGK.EQ.4)WRITE(50,5002)n24,IGK,IDH,idh,(CLTLZ(I,K),K=2,7)
      IF(IGK.GE.5)WRITE(50,5002)n24,IGK,IDH,idh,(QTTLZ(I,K),K=2,7)
           end if
           else if(iltd0.ge.1)then
           IF(IGK.EQ.0)WRITE(1,161)IDH,IDH,(ZZTLZ(I,K),K=2,7)
           IF(IGK.EQ.4)WRITE(1,161)IDH,IDH,(CRTLZ(I,K),K=2,7)
           IF(IGK.EQ.1)WRITE(1,161)IDH,IDH,(CLTLZ(I,K),K=2,7)
           IF(IGK.GE.5.and.igk.ne.100)
     +                   WRITE(1,161)IDH,IDH,(QTTLZ(I,K),K=2,7)
           end if
193    CONTINUE
      ELSE if((igk.eq.3.and.iltd0.lt.1).or.
     +          (igk.eq.2.and.iltd0.ge.1))then

      IF(NTL.NE.0.AND.EP2.GT.0)THEN
           CALL CPRINT(1)
           WRITE(1,1782)
           WRITE(1,179)IGK,AUNIT
           write(1,1783)ep2,ISep,IMep
           WRITE(1,174)
      DO 191 I=1,NTL
           IDH=SLTLZ(I,1)
           WRITE(1,161)IDH,IDH,(SLTLZ(I,K),K=2,7)
      if(iprn.eq.2)WRITE(23,4385)
     + ngd,IDH,idh,(SLTLZ(I,K),K=2,7),' -',' -'
191     CONTINUE
      END IF
      END IF

      if(iltd0.ge.1.and.igk.eq.3)then
      rewind 11
      do 200 i=1,100000
      read(11,202,end=199)ICD,IGK3,IS,IM,ij3,FJQ3,YLXS,
     +xmm3,xmat3,XMAD3,XM0T3,
     +tj,t0,pj,dw1,s1
      write(18,202)ICD,IGK3,IS,IM,ij3,FJQ3,YLXS,
     +xmm3,xmat3,XMAD3,XM0T3,
     +tj,t0,pj,dw1,s1
202     FORMAT(2I5,3I10,11F15.3)
200     continue
199     continue
      close (11)
      end if

230     FORMAT(I8,6F13.3)
240     FORMAT(I8,I8,3F13.3,F14.3)
220     FORMAT(I8,I8,3F13.3)
      RETURN
      END



C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    作者:       李  智                                    *
C*    程序名:                                               **
C*                ANCHORS                                   *
C*    功能:             *  　                               *
C*                算端点推力                                *
C*    被调用程序:             *                             *
C*                ANSELE                                    *
C*    调用程序:             　                              *
c*                无                                       *
C*    读取文件:               　                             *
c*                无                                       *
c*    写入文件:                                       　     *
c*            通道号      内容  　                           *
c*            23         供数据处理程序用的输出分析数* *         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    修改或增加                                         　  *
c*    修增者:                                             　 *
c*    修增日期:                                           　 *
c*    修增内容:                                           　 *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      SUBROUTINE anchorS(ISH,IMH,IS,IM,EP2,NTL,PS,PM,PB)

      IMPLICIT DOUBLE PRECISION(
     +A,B,C,D,E,F,G,H,O,P,Q,R,S,T,U,V,W,X,Y,Z)
c       COMMON/GK/IGK
      COMMON/GK/IGK,IFJ5,IFJ6,IFJ7,IFJ8,IFJ9,IFJ10
      COMMON/KZKG/KZDW
       common/prin/iprn,iprn1,isz,iltd0,izh
       
      COMMON/TLSC/CLTLZ(40,7),CRTLZ(40,7),
     +              SLTLZ(40,7),SRTLZ(40,7),ZZTLZ(40,7),
     +              QTTLZ(40,7)
      DIMENSION PB(30,6)

      DIMENSION PS(6),PM(6)
      
      NTL=NTL+1
      ICD=30
      IF((IGK.EQ.1.and.iltd0.lt.1).or.igk.eq.0)THEN
            IF(ISH.EQ.0)THEN
                  IDH=IS
                  ZZTLZ(NTL,1)=IDH
                  DO 2 K=1,6
                        J=K+1
                        IF(KZDW.EQ.1)THEN
                              ZZTLZ(NTL,J)=-PS(K)
                        ELSE
                              ZZTLZ(NTL,J)=-PS(K)*9.807
                        END IF
2                       CONTINUE
            ELSE IF(IMH.EQ.0)THEN
                  IDH=IM
                  ZZTLZ(NTL,1)=IDH
                  DO 5 K=1,6
                        J=K+1
                        IF(KZDW.EQ.1)THEN
                              ZZTLZ(NTL,J)=-PM(K)
                        ELSE
                              ZZTLZ(NTL,J)=-PM(K)*9.807
                        END IF
5                       CONTINUE
            END IF
            if(iprn.eq.1.and.igk.lt.99)
     +          WRITE(23,201)ICD,IGK,IDH,IDH,(ZZTLZ(NTL,K),K=2,7)
      ELSE IF((IGK.EQ.2.and.iltd0.lt.1).or.
     +          (igk.eq.4.and.iltd0.ge.1))THEN
            IF(ISH.EQ.0)THEN
            IDH =IS
            CRTLZ(NTL,1)=IDH
                  DO 10 K=1,6
                        J=K+1
                        IF(KZDW.EQ.1)THEN
                           CRTLZ(NTL,J)=ZZTLZ(NTL,J)-PS(K)
                        ELSE
                           CRTLZ(NTL,J)=ZZTLZ(NTL,J)-PS(K)*9.807
                        END IF
10                      CONTINUE
            ELSE IF(IMH.EQ.0)THEN
            IDH=IM
            CRTLZ(NTL,1)=IDH
                  DO 15 K=1,6
                        J=K+1
                        IF(KZDW.EQ.1)THEN
                           CRTLZ(NTL,J)=ZZTLZ(NTL,J)-PM(K)
                        ELSE
                           CRTLZ(NTL,J)=ZZTLZ(NTL,J)-PM(K)*9.807
                        END IF
15                      CONTINUE
            END IF
            if(iprn.eq.1)
     +          WRITE(23,201)ICD,IGK,IDH,IDH,(CRTLZ(NTL,K),K=2,7)
      ELSE IF((IGK.EQ.3.and.iltd0.lt.1).or.
     +          (igk.eq.2.and.iltd0.ge.1))THEN
            IF(ISH.EQ.0)THEN
                  IDH=IS
                  
                  SLTLZ(NTL,1)=IDH
                  SRTLZ(NTL,1)=IDH
                  DO 20 K=1,6
                  J=K+1
               SLTLZ(NTL,J)=ZZTLZ(NTL,J)-EP2*PB(NTL,K)
20                      CONTINUE
            ELSE IF(IMH.EQ.0)THEN
                  IDH=IM
                  
                  SLTLZ(NTL,1)=IDH
                  SRTLZ(NTL,1)=IDH
                  DO 25 K=1,6
                  J=K+1
               SLTLZ(NTL,J)=ZZTLZ(NTL,J)-EP2*PB(NTL,K)
25                      CONTINUE
            END IF
            if(iprn.eq.1)
     +          WRITE(23,201)ICD,IGK,IDH,IDH,(SLTLZ(NTL,K),K=2,7)
      ELSE IF((IGK.EQ.4.and.iltd0.lt.1).or.
     +          (igk.eq.1.and.iltd0.ge.1))THEN
            IF(ISH.EQ.0)THEN
            IDH=IS
            CLTLZ(NTL,1)=IDH
                  DO 30 K=1,6
                  J=K+1
                        IF(KZDW.EQ.1)THEN
                         CLTLZ(NTL,J)=zztlz(ntl,j)-PS(K)
                        ELSE
                         CLTLZ(NTL,J)=zztlz(ntl,j)-PS(K)*9.807
                        END IF
30                      CONTINUE
            ELSE IF(IMH.EQ.0)THEN
            IDH=IM
                  CLTLZ(NTL,1)=IDH
                  DO 35 K=1,6
                  J=K+1
                        IF(KZDW.EQ.1)THEN
                         CLTLZ(NTL,J)=zztlz(ntl,j)-PM(K)
                        ELSE
                         CLTLZ(NTL,J)=zztlz(ntl,j)-PM(K)*9.807
                        END IF
35                      CONTINUE
            END IF
            if(iprn.eq.1)
     +          WRITE(23,201)ICD,IGK,IDH,IDH,(CLTLZ(NTL,K),K=2,7)
      ELSE
            IF(ISH.EQ.0)THEN
            IDH=IS
                  QTTLZ(NTL,1)=IDH
                  DO 40 K=1,6
                  J=K+1
                        IF(KZDW.EQ.1)THEN
                              QTTLZ(NTL,J)=-PS(K)
                        ELSE
                              QTTLZ(NTL,J)=-PS(K)*9.807
                        END IF
40                      CONTINUE
            ELSE IF(IMH.EQ.0)THEN
            IDH=IM
                  QTTLZ(NTL,1)=IDH
                  DO 45 K=1,6
                  J=K+1
                        IF(KZDW.EQ.1)THEN
                              QTTLZ(NTL,J)=-PM(K)
                        ELSE
                              QTTLZ(NTL,J)=-PM(K)*9.807
                        END IF
45                      CONTINUE
            END IF
                 DO 51 K =2,7
                  CLTLZ(NTL,K)=QTTLZ(NTL,K)
51                   CONTINUE
            IF(IGK.EQ.5.OR.IGK.EQ.6.OR.IGK.EQ.8.OR.IGK.EQ.10)THEN
                 DO 50 K =2,7
                  QTTLZ(NTL,K)=QTTLZ(NTL,K)+CRTLZ(NTL,K)
50                   CONTINUE
            ELSE IF(IGK.EQ.7.OR.IGK.EQ.9)THEN

                 DO 55 K =2,7
                  CRTLZ(NTL,K)=QTTLZ(NTL,K)
55                   CONTINUE
            END IF
            if(iprn.eq.1)
     +          WRITE(23,201)ICD,IGK,IDH,IDH,(CLTLZ(NTL,K),K=2,7)
      END IF
201     FORMAT(2I5,2I10,6F15.3)
      RETURN
      END

C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    作者:       李  智                                    *
C*    程序名:                                               **
C*                ERROR                                   *
C*    功能:             *  　                               *
C*                错误处理                                  *
C*    被调用程序:             *                             *
C*                                                        *
C*    调用程序:             　                              *
c*                无                                       *
C*    读取文件:               　                             *
c*                无                                       *
c*    写入文件:                                       　     *
c*                无                                       *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    修改或增加                                         　  *
c*    修增者:                                             　 *
c*    修增日期:                                           　 *
c*    修增内容:                                           　 *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      SUBROUTINE ERROR(I,IH)
      IMPLICIT DOUBLE PRECISION(
     +A,B,C,D,E,F,G,H,O,P,Q,R,S,T,U,V,W,X,Y,Z)
      COMMON/CWSJ/NCW,LCW(20,2),NJG,LJG(20,2)
       common/prin/iprn,iprn1,isz,iltd0,izh

      IF(I.LT.1000)THEN
            NJG=NJG+1
            IF(NJG.LE.20)THEN
                  LJG(NJG,1)=IH
                  LJG(NJG,2)=I
            ELSE
                  LJG(20,1)=9999
                  RETURN
            END IF
      ELSE
            NCW=NCW+1
            IF(NCW.LE.20)THEN
                  LCW(NCW,1)=IH
                  LCW(NCW,2)=I
            ELSE
                  LCW(20,1)=9999
                  RETURN  
            END IF
      END IF


      RETURN
      END



C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    作者:       李  智                                    *
C*    程序名:                                               **
C*                SELEF                                   *
C*    功能:             *  　                               *
C*                解直单元内力/应力                            *
C*    被调用程序:             *                             *
C*                ANSELE                                    *
C*    调用程序:             　                              *
c*                VECTOR                                  *
C*    读取文件:               　                             *
c*            通道号      内容  　                           *
c*            24         计算的自重应力*                      *
c*            25         计算的热胀应力*                      *
c*    写入文件:                                       　     *
c*            通道号      内容  　                           *
c*            24         计算的自重应力*                      *
c*            25         计算的热胀应力*                      *
c*            23         供数据处理程序用的输出分析数* *         *
c*            11         全部标准输出应力(冷态吊零)             *
c*            18         全部标准输出应力                     *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    修改或增加                                         　  *
c*    修增者:                                             　 *
c*    修增日期:                                           　 *
c*    修增内容:                                           　 *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE SELEF(nc,RL,FX,WX,NN,IS,IM,
     +                 PS1,PM1,W,FMM,XMATSC,XM0T,IJ,
     +                 NDH,PJ,DW,S,XMT,XM0,tj,t0)

      IMPLICIT DOUBLE PRECISION(
     +A,B,C,D,E,F,G,H,O,P,Q,R,S,T,U,V,W,X,Y,Z)
      CHARACTER*15 AJL
      common/prin/iprn,iprn1,isz,iltd0,izh
      COMMON/SRST/ISTDH(20000),RYLQ(20000),RKWJ(20000)
c       COMMON/GK/IGK
      COMMON/GK/IGK,IFJ5,IFJ6,IFJ7,IFJ8,IFJ9,IFJ10
      COMMON/CAD/ICAD
      COMMON/KZKG/KZDW
      COMMON/GXYL/RYLJQ,DLXM,XMMMM
      COMMON/XS/XS10(10)
      COMMON/NXHXS/FXHXS,IZBXZ,NNDS,NNDB,NRG
      DIMENSION F1(6),FP(6),NDH(0:20),W(6)
      DIMENSION FQ(6),BA(6,6),PS1(6),PM1(6)
      DIMENSION BB(6,6)
      

      DATA BA,BB/72*0./

      XMMMM= PJ*((DW-2*S)**2.0)/(100*(DW**2.0-(DW-2*S)**2.0))
      IF((IGK.EQ.3.and.iltd0.lt.1).OR.
     +     (igk.eq.2.and.iltd0.ge.1).or.IGK.EQ.8)THEN
           FXS=FXHXS
      ELSE
           FXS=1.
      END IF

            N=NN
            BA(1,1)=1.0
            BA(2,2)=1.0
            BA(3,3)=1.0
            BA(4,4)=1.0
            BA(5,5)=1.0
            BA(6,6)=1.0
            RLN=RL/N
            BA(5,3)=RLN
            BA(6,2)=-RLN

            BB(1,1)=RLN
            BB(2,2)=RLN
            BB(3,3)=RLN
            BB(5,3)=(RLN**2.)/2.
            BB(6,2)=-BB(5,3)

            FMM=1.0         
            IJ=0
            DO 5 K=1,6
                  F1(K)=-PS1(K)
5               CONTINUE


      IF(ISTDH(IM).ge.1.and.istdh(im).le.5.
     +       AND.RYLQ(IM).GT.1)THEN
            DO 6 K=1,6
              F1(K)=-PM1(K)
6               CONTINUE
       IJ=N
      END IF
         FMD=DSQRT(F1(4)*F1(4)+F1(5)*F1(5)+F1(6)*F1(6))
         XMW=FMD/WX
         if(fmd.le.0.0000001.and.igk.eq.3)XMW=abs(F1(1)/FX)
C old yinggui (old stress reguler)
                IF(NRG.EQ.1)XMW=XMW+abs(F1(1)/FX)
C EARTH QUWCK ONLY HALF -F-
                IF(IGK.EQ.6)THEN
                  XMW=XMW/2.
                END IF

        DLXM=XMW

                ryljq=rylq(is)

            IF(RYLQ(IM).GT.ryljq)THEN
                RYLJQ=RYLQ(IM)
            END IF

         IF ((IGK.EQ.1.and.iltd0.lt.1).OR.
     +         (igk.eq.3.and.iltd0.ge.1).or.
     +         (IGK.GE.5.AND.IGK.LE.9.and.igk.ne.8))THEN
                RYLJQ1=RYLJQ*0.75
                IF(NRG.EQ.1)RYLJQ1=RYLJQ
                IF(RYLJQ1.LT.1)RYLJQ1=1.
                RYLJQ=RYLJQ1
       ryljq3=ryljq
            END IF

       XMA=XMW*RYLJQ
             jk=0
       IF((IGK.EQ.1.and.iltd0.lt.1).OR.
     +             (igk.eq.3.and.iltd0.ge.1).or.
     +              igk.eq.7.OR.IGK.EQ.9)THEN
                 XMAD=XMMMM + XMA
                 WRITE(24,*)XMAD,is,im,jk
                 if(iltd0.ge.1)then
            read(25)xmad3
            xma3=fxs*xmad+xmad3
                 end if
       ELSE if(igk.ne.100)then
                 IF((IGK.EQ.3.and.iltd0.lt.1).or.
     +                  (igk.eq.2.and.iltd0.ge.1))
     +                    WRITE(25)XMA

                 if(igk.ne.2)then
            READ(24,*)XMAD
            XMMMM=XMAD*FXS
                 else
            xmad=0.
            xmmmm=0.
                 end if
                 
                 XMA=XMA+XMMMM
                 
                 IF(IGK.EQ.8)THEN
                  READ(25)XMAD
                  XMA=XMA+XMAD
                 END IF
       END IF
                 XM2A=XMMMM

                FJQ=RYLJQ
                fjq3=ryljq3

            IF(ISTDH(IM).ge.1.and.istdh(im).le.5.AND.
     +                           RYLQ(IM).GT.1.00001)GOTO 50
c     dui dy xun huan do circle for element
      DO 45 I=1,N
            
                  CALL JZXL(BA,F1,FP)
                  CALL JZXL(BB,W,FQ)
                  CALL XLZF(FQ,F1)
                  CALL XLCC(F1,FQ)
                  CALL XLAD(FP,FQ,F1)
                  CALL XLCC(F1,FP)
C                       FP(4)=-PS1(4)
                  FMD=DSQRT(FP(4)*FP(4)+FP(5)*FP(5)+FP(6)*FP(6))
                     XMW=FMD/WX
                    if(xmw.le.0.0000001)xmw=abs(fp(1)/fx)
                     IF(NRG.EQ.1)XMW=XMW+abs(FP(1)/FX)
                  IF(IGK.EQ.6)THEN
                     XMW=XMW/2.
                  END IF
c=========================
      if(i.eq.n)then
                RYLJQ=RYLQ(IM)

       IF ((IGK.EQ.1.and.iltd0.lt.1).OR.
     +         (igk.eq.3.and.iltd0.ge.1).or.
     +         (IGK.GE.5.AND.IGK.LE.9.and.igk.ne.8))THEN
                RYLJQ1=RYLJQ*0.75
                IF(NRG.EQ.1)RYLJQ1=RYLJQ
                IF(RYLJQ1.LT.1)RYLJQ1=1.
                RYLJQ=RYLJQ1
                ryljq3=ryljq
       END IF

        XMw=XMW*RYLJQ
        
        FJQM=RYLJQ
        fjq3M=ryljq3
      end if
      
c========================
      XM=XMW

      IF((IGK.EQ.1.and.iltd0.lt.1).OR.
     +   (igk.eq.3.and.iltd0.ge.1).or.
     +           IGK.EQ.7.OR.IGK.EQ.9)THEN
                        XMAD=XMMMM+XM
                        WRITE(24,*)XMAD,is,im,i

                        if(iltd0.ge.1)then
             read(25)xmad3
             xm3=fxs*xmad+xmad3
                        end if
                  ELSE if(igk.ne.100)then
                        IF((IGK.EQ.3.and.iltd0.lt.1).or.
     +                           (igk.eq.2.and.iltd0.ge.1))
     +                            WRITE(25)XM

                        if(igk.ne.2)then

                          READ(24,*)XMAD
                          XMMMM=XMAD*FXS
                        else if(igk.eq.2.and.iltd0.ge.1)then
                          xmad=0.
                          xmmmm=0.
                        end if
                        
                        XM=XM+XMMMM
                        IF(IGK.EQ.8)THEN
                        READ(25)XMAD
       XM=XM+XMAD
                        END IF
                  END IF
c=========================
      IF(XM.GT.XMA.and.iltd0.lt.1)THEN
       DLXM=XMW
       xm2a=XMMMM
       XMA=XM
       IJ=I
       NDH(IJ)=I
       if(i.ne.n)then
         FJQ=1.0
         RYLJQ=1.0
       else if(i.eq.n)then
         FJQ=FJQM
         RYLJQ=RYLJQ
       end if
      else if(xm3.gt.xma3.and.iltd0.ge.1)then
       DLXM3=XMW
       xm2a3=XMMMM
       XMA3=XM3
       IJ3=I
c                                NDH(IJ3)=I
       if(i.ne.n)then
         FJQ3=1.0
         RYLJQ3=1.0
       else if(i.eq.n)then
         FJQ3=FJQM
         RYLJQ3=RYLJQ
      end if
      END IF

                  CALL XLCC(FP,F1)
45              CONTINUE

50      CONTINUE
      IF((IGK.EQ.1.and.iltd0.lt.1).OR.
     +     (igk.eq.3.and.iltd0.ge.1).or.
     +      IGK.EQ.7.OR.IGK.EQ.9)THEN
          XMAD= XMMMM+XMA
          if(iltd0.ge.1)xmad3=xma3
      ELSE
          XMAD=XMA
      END IF
          icd=60+nc
          icd=63
c          if(nc.gt.1.and.nc.ne.4)icd=65

            YLXS=XS10(IGK)
       IF((IGK.EQ.3.and.iltd0.lt.1).OR.
     +      (igk.eq.2.and.iltd0.ge.1).or.IGK.EQ.8)THEN
            XM0T=1.2*(XMT+XM0)
       ELSE IF(IGK.EQ.9)THEN
            if(ylxs.ge.1)then
            XM0T=0.9*XMT
            else
            xm0t=xmt
            end if
       ELSE
            XM0T=XMT
            if(iltd0.ge.1.and.igk.eq.3)xm0t3=fxs*1.2*(xmt+xm0)
       END IF
            XM0T=XM0T*YLXS
            if(iltd0.ge.1.and.igk.eq.3)xm0t=xm0t/ylxs
       IF(XMAD.GT.XM0T)THEN
          AJL=' ****'
       ELSE
          AJL=' -ok-'
       END IF
         XMM=XM2A
         if(igk.ne.2)then
         xMAT=xmad-xmM
         else
         xmat=xmad
         end if
         XMATSC=XMAT

         if(iltd0.ge.1.and.igk.eq.3)then
         XMM3=XM2A3
         xMAT3=xmad3-xmM3
         end if

         if(kzdw.ne.1)then
         xmad=xmad*9.807
         xm0t=xm0t*9.807
         xmat=xmat*9.807
         xmm = xmm*9.807
         if(iltd0.ge.1.and.igk.eq.3)then
         xmad3=xmad3*9.807
         xm0t3=xm0t3*9.807
         xmat3=xmat3*9.807
         xmm3 = xmm3*9.807
         end if
         end if
         dw1=dw*10
         s1=s*10

       if(xmad.gt.99999)xmad=99999
c---igk.ne.100
      if(igk.ne.2.and.igk.ne.100)then
         if(iprn.eq.1)
     +   WRITE(23,201)ICD,IGK,IS,IM,DW1,S1,FJQ,YLXS,XMAD,XM0T
      WRITE(18,202)ICD,IGK,IS,IM,ij,FJQ,YLXS,xmm,xmat,XMAD,XM0T,
     +tj,t0,pj,dw1,s1
         if(iltd0.ge.1.and.igk.eq.3)then
          igk3=2
          WRITE(11,202)ICD,IGK3,IS,IM,ij3,FJQ3,YLXS,
     +    xmm3,xmat3,XMAD3,XM0T3,
     +    tj,t0,pj,dw1,s1
         end if
         
      if(iprn1.ne.0)
     +   WRITE(1,161)IS,IM,IJ,FJQ,YLXS,XMAD,XM0T,AJL
      end if

202     FORMAT(2I5,3I10,11F15.3)
201     FORMAT(2I5,2I10,9F15.3)
161      FORMAT(5X,3I5,4F15.3,2X,A6)
      RETURN
      END
C============================================
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    作者:       李  智                                    *
C*    程序名:                                               **
C*                BELEF                                   *
C*    功能:             *  　                               *
C*                解弯单元内力/应力                            *
C*    被调用程序:             *                             *
C*                ANSELE                                    *
C*    调用程序:             　                              *
c*                VECTOR                                  *
C*    读取文件:               　                             *
c*            通道号      内容  　                           *
c*            24         计算的自重应力*                      *
c*            25         计算的热胀应力*                      *
c*    写入文件:                                       　     *
c*            通道号      内容  　                           *
c*            24         计算的自重应力*                      *
c*            25         计算的热胀应力*                      *
c*            23         供数据处理程序用的输出分析数* *         *
c*            11         全部标准输出应力(冷态吊零)             *
c*            18         全部标准输出应力                     *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    修改或增加                                         　  *
c*    修增者:                                             　 *
c*    修增日期:                                           　 *
c*    修增内容:                                           　 *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
                              
      SUBROUTINE BELEF(RL,SI,S,DW,N,IS,IM,
     1                  FX,WX,PS1,W,FMM,XMATSC,IJ,
     +                 NDH,PJ,XMT,XM0,tj,t0)

      IMPLICIT DOUBLE PRECISION(
     +A,B,C,D,E,F,G,H,O,P,Q,R,S,T,U,V,W,X,Y,Z)
      CHARACTER*15 AJL
      common/prin/iprn,iprn1,isz,iltd0,izh
       COMMON/SRST/ISTDH(20000),RYLQ(20000),RKWJ(20000)
      COMMON/NXHXS/FXHXS,IZBXZ,NNDS,NNDB,NRG
      COMMON/GXYL/RYLJQ,DLXM,XMNY
      COMMON/CAD/ICAD
c       COMMON/GK/IGK
      COMMON/GK/IGK,IFJ5,IFJ6,IFJ7,IFJ8,IFJ9,IFJ10
      COMMON/KZKG/KZDW
      COMMON/XS/XS10(10)
      DIMENSION H(6,6),FI(6,6),PSI(6,6),X(6,6),Q(6),Q1(6)
      DIMENSION F(6),FP(6),F1(6),F2(6),F3(6),
     +               PS1(6),W(6),WG(6),NDH(0:20)
      DIMENSION FIQ(6,6)
      XMMMM= PJ*((DW-2*S)**2)/(100*(DW**2-(DW-2*S)**2))
      IF((IGK.EQ.3.and.iltd0.lt.1).OR.
     +     (igk.eq.2.and.iltd0.ge.1).or.
     +      IGK.EQ.8)THEN
           FXS=FXHXS
      ELSE
           FXS=1.
      END IF
            RN=N
            SIIN=SI/RN
            CO=DCOS (SI/2)
            SIS=DSIN(SI/2)

            COF=DCOS(SIIN)
            SIF=DSIN(SIIN)

            SIP=RL*RL*(DSIN(SIIN)-SIIN)
            COP=RL*RL*(DCOS(SIIN)-1)

            DO 15 I=1,6
            DO 15 J=1,6
                  H(I,J)=0.0                      
                  FIQ(I,J)=0.0
                  FI(I,J)=0.0
                  PSI(I,J)=0.0
                  X(I,J)=0.0
15              CONTINUE
            H(1,1)=CO
            H(1,2)=-SIS
            H(2,1)=SIS
            H(2,2)=CO
            H(3,3)=1.0
            H(4,4)=CO
            H(4,5)=-SIS
            H(5,4)=SIS
            H(5,5)=CO
            H(6,6)=1.0

            FI(1,1)=COF
            FI(1,2)=SIF
            FI(2,1)=-SIF    
            FI(2,2)=COF
            FI(3,3)=1.0


            PSI(1,1)=COF
            PSI(1,2)=SIF
            PSI(2,1)=-SIF
            PSI(2,2)=COF
            PSI(3,3)=1
            PSI(4,4)=COF
            PSI(4,5)=SIF
            PSI(5,4)=-SIF
            PSI(5,5)=COF
            PSI(6,6)=1.
            PSI(4,3)=(1.-COF)*RL
            PSI(5,3)=SIF*RL
            PSI(6,1)=(1.-COF)*RL
            PSI(6,2)=-SIF*RL

            FIQ(1,1)=SIIN*RL
            FIQ(2,2)=SIIN*RL
            FIQ(3,3)=SIIN*RL

            X(4,3)=SIP
            X(5,3)=COP
            X(6,1)=-SIP
            X(6,2)=-COP


            CALL JZXL(H,PS1,WG)
            CALL XLZF(WG,F)
            CALL JZXL(H,W,Q)

            FMD=DSQRT(F(4)*F(4)+F(5)*F(5)+F(6)*F(6))
            XMW=FMD/WX
            IF(NRG.EQ.1)XMW=XMW+abs(F1(1)/FX)
            IF(IGK.EQ.6)THEN
            XMW=XMW/2.
            END IF
            DLXM=XMW                
C-----          XMA=XMZW+XMW
c                if(rylq(is).gt.1.0)then
            IF(RYLQ(IS).GT.FMM)THEN
                RYLJQ=RYLQ(IS)*fmm
            ELSE
                RYLJQ=FMM
      END IF
            
        IF ((IGK.EQ.1.and.iltd0.lt.1).OR.
     +        (igk.eq.3.and.iltd0.ge.1).or.
     +        (IGK.GE.5.AND.IGK.LE.9.and.igk.ne.8))THEN
                RYLJQ1=0.75*RYLJQ
                IF(NRG.EQ.1)RYLJQ1=RYLJQ
                IF(RYLJQ1.LT.1)RYLJQ1=1.
                RYLJQ=RYLJQ1
                ryljq3=ryljq
            END IF
                XMA=XMW*RYLJQ
                        iJ=0
            IF((IGK.EQ.1.and.iltd0.lt.1).OR.
     +             (igk.eq.3.and.iltd0.ge.1).or.
     +              IGK.EQ.7.OR.IGK.EQ.9)THEN
                XMAD=XMMMM+XMA
                WRITE(24,*)XMAD,is,im,iJ
                 if(iltd0.ge.1)then
                  read(25)xmad3
                  xma3=fxs*xmad+xmad3
                 end if
            ELSE if(igk.ne.100)then
                IF((IGK.EQ.3.and.iltd0.lt.1).or.
     +                 (igk.eq.2.and.iltd0.ge.1))WRITE(25)XMA
c                    IF((IGK.EQ.3.and.iltd0.lt.1).or.
c     +                 (igk.eq.2.and.iltd0.ge.1))
c     +                  WRITE(25)XMa

                if(igk.ne.2)then
                READ(24,*)XMAD
                XMMMM=XMAD*FXS
                else
                 xmad=0
                 xmmmm=0
                end if

                XMA=XMA+XMMMM
                 IF(IGK.EQ.8)THEN
                  READ(25)XMAD
                  XMA=XMA+XMAD
                 END IF
            END IF
            
                FJQ=RYLJQ

            IJ=0
            XM2A=XMMMM
            NDH(IJ)=0
            DO 50 I2=1,N

                  CALL JZXL(FI,Q,Q1)
                  CALL JZXL(PSI,F,F1)
                  CALL JZXL(X,Q1,F3)


                  PR=SIIN*RL
                  CALL SCXL(PR,Q1,F2)

                  DO 30 I=1,3
                   FP(I)=F1(I)-F2(I)
                   J=I+3
                   FP(J)=F1(J)+F3(J)
30                     CONTINUE


                  FMD=DSQRT(FP(4)*FP(4)+FP(5)*FP(5)+FP(6)*FP(6))
                  XMW=FMD/WX
                  IF(NRG.EQ.1)XMW=XMW+abs(FP(1)/FX)
                  
                  IF(IGK.EQ.6)THEN
                  XMW=XMW/2.
                  END IF
c                        IF(I2.EQ.N.AND.RYLQ(IM).GT.FMM)THEN
                  if(i2.eq.n.and.rylq(im).gt.1.0)then
                    RYLJQ=RYLQ(IM)*fmm
                  ELSE
                    RYLJQ=FMM
                  END IF
        IF ((IGK.EQ.1.and.iltd0.lt.1).OR.
     +        (igk.eq.3.and.iltd0.ge.1).or.
     +        (IGK.GE.5.AND.IGK.LE.9.and.igk.ne.8))THEN
                    RYLJQ1=RYLJQ*0.75
                    IF(NRG.EQ.1)RYLJQ1=RYLJQ
                    IF(RYLJQ1.LT.1)RYLJQ1=1.
                    RYLJQ=RYLJQ1
                    ryljq3=ryljq
                  END IF

                    XM=XMW*RYLJQ

                  IF((IGK.EQ.1.and.iltd0.lt.1).OR.
     +                     (igk.eq.3.and.iltd0.ge.1).or.
     +                      IGK.EQ.7.OR.IGK.EQ.9)THEN
                    XMAD=XMMMM+XM
                    WRITE(24,*)XMAD,is,im,i2

                    if(iltd0.ge.1)then
                       read(25)xmad3
                       xm3=fxs*xmad+xmad3
                    end if

                  ELSE if(igk.ne.100)then
                     IF((IGK.EQ.3.and.iltd0.lt.1).or.
     +                        (igk.eq.2.and.iltd0.ge.1))
     +                         WRITE(25)XM

                     if(igk.ne.2)then
                     READ(24,*)XMAD
                     XMMMM=XMAD*FXS
                     else
                      xmad=0
                      xmmmm=0
                     end if
                     XM=XM+XMMMM
                     IF(IGK.EQ.8)THEN
                      READ(25)XMAD
                      XM=XM+XMAD
                     END IF
                  END IF

                  IF(XM.GT.XMA.and.iltd0.lt.1)THEN
                        DLXM=XMW
                        xm2a=XMMMM
                        XMA=XM
                        IJ=I2
                        NDH(IJ)=I2
                        FJQ=RYLJQ
                  else if(xm3.gt.xma3.and.iltd0.ge.1)then
                        DLXM3=XMad3
                        xm2a3=XMad*fxs
                        XMA3=XM3
                        IJ3=I2
c                                NDH(IJ)=I2
                        FJQ3=RYLJQ
                  END IF
                  CALL XLCC(Q1,Q)

                  CALL XLCC(FP,F)
50              CONTINUE
            

      ICD=60
      IF((IGK.EQ.1.and.iltd0.lt.1).OR.
     +     (igk.eq.3.and.iltd0.ge.1).or.IGK.EQ.7.OR.IGK.EQ.9)THEN
          XMAD= XMMMM+XMA
          if(iltd0.ge.1)xmad3=xma3
      ELSE
          XMAD=XMA
      END IF

       YLXS=XS10(IGK)
       ICD=63
       IF((IGK.EQ.3.and.iltd0.lt.1).OR.
     +      (igk.eq.2.and.iltd0.ge.1).or.IGK.EQ.8)THEN
            XM0T=1.2*(XMT+XM0)
       ELSE IF(IGK.EQ.9)THEN
            if(ylxs.ge.1)then
            XM0T=0.9*XMT
            else
            xm0t=xmt
            end if
       ELSE
            XM0T=XMT*1.
            if(iltd0.ge.1.and.igk.eq.3)xm0t3=1.2*(xmt+xm0)*fxs
       END IF
            XM0T=XM0T*YLXS
            if(iltd0.ge.1.and.igk.eq.3)xm0t=xm0t/ylxs
       IF(XMAD.GT.XM0T)THEN
          AJL=' ****'
       ELSE
          AJL=' -ok-'
       END IF
            XMM=XM2A
            if(igk.ne.2)then
            XMAT=XMAD-XMM
            else
            xmat=xmad
            end if
            XMATSC=XMAT

            if(iltd0.ge.1)then
            xmm3=xm2a3
            xmat3=xmad3-xmm3
            end if
         if(kzdw.ne.1)then
         xmad=xmad*9.807
         xm0t=xm0t*9.807
         xmat=xmat*9.807
         xmm = xmm*9.807
         if(iltd0.ge.1.and.igk.eq.3)then
         xmad3=xmad3*9.807
         xm0t3=xm0t3*9.807
         xmat3=xmat3*9.807
         xmm3 = xmm3*9.807
         end if
         end if
         dw1=dw*10
         s1=s*10
       if(xmad.gt.99999)xmad=99999
         if(igk.ne.2.and.igk.ne.100)then
         if(iprn.eq.1.and.igk.lt.99)
     +  WRITE(23,201)ICD,IGK,IS,IM,DW1,S1,FJQ,YLXS,XMAD,XM0T
      WRITE(18,202)ICD,IGK,IS,IM,ij,FJQ,YLXS,xmm,xmat,XMAD,XM0T,
     +               tj,t0,pj,dw1,s1
         if(iltd0.ge.1.and.igk.eq.3)then
         igk3=2
      WRITE(11,202)ICD,IGK3,IS,IM,ij3,FJQ3,YLXS,
     +               xmm3,xmat3,XMAD3,XM0T3,
     +               tj,t0,pj,dw1,s1
         end if
      if(iprn1.ne.0)
     +   WRITE(1,161)IS,IM,IJ,FJQ,YLXS,XMM,xmat,XMAD,XM0T,AJL
         end if
201     format(2i5,2i10,6f15.3)
202     FORMAT(2I5,3I10,11F15.3)
161     FORMAT(5X,3I10,6F15.3,2X,A6)
210     FORMAT(I7,I7,5F9.3,F7.3)
220     FORMAT(I7,I7,4F9.3,F7.3)
      RETURN
      END     
