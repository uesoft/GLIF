C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    作者:       李  智                                     *
C*    程序名:                                               *
C*               DATAIN                                    *
C*    功能:       输入数据  　                               *
C*                                                        *
C*    被调用程序:                                           *
C*                VNEW11                                   *
C*    调用程序:              　                              *
C*                CPRINT                                   *
C*                MAT0                                    *
C*                ERROR                                   *
C*    读取文件:               　                             *
c*            通道号      内容  　                           *
c*            11         原始数据                 　　       *
c*            18         原始材料数               　　       *
c*    写入文件:                                       　     *
c*            通道号      内容                           　  *
c*            1          输出分析数据                        *
c*            23         供数据处理程序用的输出分析数据         *
c*            13         内部原始数据                        *
c*            14         水压实验用的内部原始数据(9工况)        *
c*            21         超温超压用的内部原始数据(7工况)        *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    修改或增加                                         　  *
c*    修增者:  李智                                       　 *
c*    修增日期: 1995.06.01                                 　 *
c*    修增内容:                                           　 *
c*                                                         *
C*              RKWJ(IST)=PI*RSTRMB*RSTRMB*RSTSNB/1000.             *
C*     改为                                                *
C*              RKWJ(IST)=PI*RSTRMB*RSTRMB*RSTSB/1000.    *
C*                                                         *
C*     CCXS=(1+RSTR2/RSTR)*RSTSN/RSTSR                    *
C*     改为                                                *
C*     CCXS=(1+RSTR2/RSTR)*RSTSN/RSTR                     *
C*                                                        *
c*     WY(NWY,1)=WY(NWY,2)/10.
c*     WY(NWY,1)=WY(NWY,3)/10.
C*     改为                                                *
c*     WY(NWY,2)=WY(NWY,2)/10.
c*     WY(NWY,3)=WY(NWY,3)/10.
c*                                                         *
c*    修增者:                                       　     *
c*    修增日期:                                 　          *
c*    修增内容:                                           　 *
c*           窄行输出                                       *
c*           冷态吊零                                       *
c*    修增者:                                       　     *
c*    修增日期:                                 　          *
c*    修增内容:                                           　 *
c*                                                         *
c*                                                         *
c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      SUBROUTINE DATAIN(ISH,IMH,LX,ITLX,NCW1,NJG1,NFZ1)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      common/prin/iprn,iprn1,isz,iltd0,izh
      COMMON/GK/IGK,IFJ5,IFJ6,IFJ7,IFJ8,IFJ9,IFJ10
      COMMON/XS/XS10(10)
	common/jssz/isz9
      COMMON/CWSJ/NCW,LCW(20,2),NJG,LJG(20,2)
      COMMON/NXHXS/FXHXS,IZBXZ,NNDS,NNDB,NRG
      common/scnd/isc,iscnd(50),iscgk(50),iscis(50,10),iscim(50,50),
     +f1(50,50,6),f2(50,50,6)
      COMMON/SRST/ISTDH(20000),RYLQ(20000),RKWJ(20000)
C        COMMON/SPSP/NSP,SPP(10000)
      COMMON/SCDS/NFZ,NDD,JD(50),DD(50,6),D8(50,6),D6(50,6)
C       COMMON/SCST/NST,IST(50),YLJQ(50)
      COMMON/SCHZ/NJH,JHZ(300),IHZ(300),HZ(300,6),
     +njh5,IHZ5(300),HZ5(300,6)
      COMMON/LLGD/NLL,LL(50),RL(50,3)
      COMMON/LLLL/INLL,LLL(50),RRL(50)
      COMMON/KZKG/ISCDW,NTH,KTH,THXS,isrdw
      COMMON/TIM/LTIM1,LTIM2,ABC,AA1,AA2
      COMMON/YYGD/NWY,IWY(50),JWY(50),WY(50,6)
      COMMON/HDGD/NHD,IHD(400),HD(400)
      COMMON/IZDJ/IZD(10),ZDFC

      DIMENSION IMH(1000),ISH(1000)
      DIMENSION WXR(1000),WYR(1000),WZR(1000)
      DIMENSION XR(1000),YR(1000),ZR(1000),pr(1000),tr(1000),ar(1000)
      DIMENSION LX(1000),ITLX(1000)
      DIMENSION YZB(20,3),IZB(20),DK(6,6)
      dimension HIGHT(30),WINDF(30)
      CHARACTER LTIM1*9,ABC*15,LTIM2*10,AA1*12,AA2*12
      CHARACTER DAT1*17,DAT2*17,WJM*20

      LOGICAL LQ,LZBC
      INCLUDE 'VNEW6.FOR'
      DATA WXR,WYR,WZR/3000*0/
      DATA NFZ,NDD,NST,NWG,NLL,NWY,I1,NJH,NQ/9*0/
      DATA ISN,IMN,ISQ,IMQ,IZB/24*0/
      DATA S1,DW,XAD,YAD,ZAD,X,Y,Z,XL,PJ,XM0,XMT,ALF,E0,ET
     1       ,S,GW1,GW2,RC,SI,RK/21*0./
      DATA IZD/10*0/


      do 3333 i=1,20000
         istdh(i)=0
         RYLQ(I)=0
         RKWJ(I)=0
3333  continue      
      PI=3.1415926535
      DAT1='10CRMO910'
      DAT2=' '
      ZDFC=20.
      GJXS=100.
      iprn=0
      isz=0
      isz9=0
      izh=0
      itj=-999
      wr=0
      TJ=200
      T0=20
      NC=0
      NI=0
      NWG=0
      NXW=0
      NTH=0
      NGD=0
      NSD=0
      NST=0
      NGJ=0
      NWY=0
      NJH=0
      njh5=0
      NHD=0
      IH=0
      NF=0
      NCW=0
      NJG=0
      NGZ=0
      NFDS=0
      LJDYS=0
      MM=10000
      N10=0
      IEW=2
      FHZJM=0
      FHZXS=0
      GDHS=100
      FHZBGXS=0
      G1WX=0
      G1W=0
      G1WZ=0
      XX=0
      YY=0
      ZZ=0
      ISJK=0
      NZG=0
      NCNC=1
      IMAT=0
      INLL=0
      ITM=2
      LZBC=.FALSE.
      LQ=.FALSE.
      isc=0
      ik=0
      izg=0
      sa=0
c**************************
      do 342 i=1,50
       JHZ(i)=0
       IHZ(i)=0
       IHZ5(i)=0
      do 342 j=1,6
       HZ(i,j)=0.
       HZ5(i,j)=0.
342   continue      
c**************************
      WRITE(1,6666)AA1,AA2

      READ(11,*,ERR=600)ISRDW,ISCDW,IZBXZ
      READ(11,*,ERR=600)NTH,KTH,THXS
      READ(11,*,ERR=600)NRG,NNDS,NNDB
      READ(11,*,ERR=600)FXHXS,XS10(5),XS10(6)
      READ(11,*,ERR=600)nc
      if(nc.eq.1)then
       izh=1
      else if(nc.eq.0)then
       izh=0
      else
       izh=0
       BACKSPACE 11
      end if
      
      if(isrdw.eq.9.and.iscdw.eq.9.and.izbxz.eq.9)then
          isrdw=1
          iscdw=0
          izbxz=2
          nth=3
          kth=4
          thxs=0.35
          nrg=2
          nnds=5
          nndb=6
          fxhxs=1
          xs10(5)=1.2
          xs10(6)=1.2
      end if
      XS10(1)=1.0
      XS10(3)=fxhxs
      XS10(7)=1.0
      XS10(8)=FXHXS
      XS10(9)=0.9
            
      NI=1
1000    CONTINUE
      IF((NC.LE.4.AND.NC.NE.-1).OR.NC.EQ.10.)NQ=NC

      IF(NC.NE.-1)NQQ=NC
      READ(11,*,END=27250,ERR=700)NC
      BACKSPACE 11
      if(nc.eq.48)then
       read(11,*)nc,izh
      end if

       IH=IH+1

      ik=ik+1
      IF(Ik.EQ.1.OR.Ik.ge.30)then
       ik=1
       if(izh.eq.0)then
      CALL CPRINT(5)
      write(1,777)
       end if
      END IF  
      IF(IH.EQ.1)THEN
      WRITE(1,9991)ISRDW,ISCDW,IZBXZ
      WRITE(1,9990)NTH,KTH,THXS
      WRITE(1,9991)NRG,NNDS,NNDB
      WRITE(1,8881)FXHXS,XS10(5),XS10(6)
      END IF

      IF(NC.EQ.90)THEN
            READ(11,*,ERR=600)NC,T0,TJ,PJ
            WRITE(1,9990)IH,NC,T0,TJ,PJ
             IF(IMAT.EQ.1)THEN
              CALL MAT0(DAT1,S,T0,TJ,XM0,XMT,ALF,E0,ET)
               IF(ISRDW.EQ.1)THEN
                 ET=ET/98.07
                 E0=E0/98.07
                 XM0=XM0/9.807
                 XMT=XMT/9.807
               END IF
              WRITE(1,9990)IH,NC,T0,TJ,XM0,XMT,ALF,E0,ET
             END IF
      ELSE IF(NC.EQ.70)THEN
            IMAT=0
            READ(11,*,ERR=600)NC,XM0,XMT,ALF,E0,ET
            WRITE(1,9990)IH,NC,XM0,XMT,ALF,E0,ET
      ELSE IF(NC.EQ.99)THEN
            IFJ9=9
            IFJ10=10
            READ(11,*,ERR=600)NC,T0SY,TJSY,PJSY
            WRITE(1,9990)IH,NC,T0SY,TJSY,PJSY
             IF(IMAT.EQ.1)THEN
           CALL MAT0(DAT1,S,T0SY,TJSY,XM0SY,XMTSY,ALFSY,E0SY,ETSY)
               IF(ISRDW.EQ.1.AND.MDW.EQ.99)THEN
                 ETSY=ETSY/98.07
                 E0SY=E0SY/98.07
                 XM0SY=XM0SY/9.807
                 XMTSY=XMTSY/9.807
               ELSE IF(MDW.NE.1.AND.ISRDW.EQ.1)THEN
                 ETSY=ETSY/98.07
                 E0SY=E0SY/98.07
                 XM0SY=XM0SY/9.807
                 XMTSY=XMTSY/9.807
               ELSE IF(MDW.EQ.1.AND.ISRDW.NE.1)THEN
                 ETSY=ETSY*98.07
                 E0SY=E0SY*98.07
                 XM0SY=XM0SY*9.807
                 XMTSY=XMTSY*9.807
               END IF
           WRITE(1,9990)IH,NC,T0SY,TJSY,XM0SY,XMTSY,ALFSY,E0SY,ETSY
             END IF
      ELSE IF(NC.EQ.79)THEN
            IMAT=0
            READ(11,*,ERR=600)NC,XM0SY,XMTSY,ALFSY,E0SY,ETSY
            WRITE(1,9990)IH,NC,XM0SY,XMTSY,ALFSY,E0SY,ETSY
      ELSE IF(NC.EQ.97)THEN
            IFJ8=8
            IFJ7=7
            READ(11,*,ERR=600)NC,T0CY,TJCY,PJCY
            WRITE(1,9990)IH,NC,T0CY,TJCY,PJCY
             IF(IMAT.EQ.1)THEN
            CALL MAT0(DAT1,S,T0,TJCY,XM0CY,XMTCY,ALFCY,E0CY,ETCY)
               IF(ISRDW.EQ.1.AND.MDW.EQ.99)THEN
                 ETCY=ETCY/98.07
                 E0CY=E0CY/98.07
                 XM0CY=XM0CY/9.807
                 XMTCY=XMTCY/9.807
               ELSE IF(MDW.NE.1.AND.ISRDW.EQ.1)THEN
                 ETCY=ETCY/98.07
                 E0CY=E0CY/98.07
                 XM0CY=XM0CY/9.807
                 XMTCY=XMTCY/9.807
               ELSE IF(MDW.EQ.1.AND.ISRDW.NE.1)THEN
                 ETCY=ETCY*98.07
                 E0CY=E0CY*98.07
                 XM0CY=XM0CY*9.807
                 XMTCY=XMTCY*9.807
               END IF

          WRITE(1,9990)IH,NC,T0CY,TJCY,XM0CY,XMTCY,ALFCY,E0CY,ETCY
             END IF
      ELSE IF(NC.EQ.77)THEN
            IMAT=0
            READ(11,*,ERR=600)NC,XM0CY,XMTCY,ALFCY,E0CY,ETCY
            WRITE(1,9990)IH,NC,XM0CY,XMTCY,ALFCY,E0CY,ETCY
C==============
      ELSE IF(NC.EQ.60)THEN
            DWQ=DW2
            SQ=S2
            GW1Q=GW1
            READ(11,*,ERR=600)NC,DW5,S5,GW5
            WRITE(1,9990)IH,NC,DW5,S5,GW5
        if(dw5.lt.50.or.dw5.gt.3000.or.s5.gt.200)call error(nc,ih)
            DW55=DW5/10
            S55=S5/10
C============== 
      ELSE IF(NC.EQ.61)THEN
            READ(11,*,ERR=600)NC,DW,S,GW1
            WRITE(1,9990)IH,NC,DW,S,GW1
            if(dw.lt.50.or.dw.gt.3000.or.s.gt.200)call error(nc,ih)
             IF(IMAT.EQ.1.AND.
     +              (DAT1.EQ.'10CRMO910'.OR.DAT1.EQ.'ST45.8'))THEN
              CALL MAT0(DAT1,S,T0,TJ,XM0,XMT,ALF,E0,ET)
               IF(ISRDW.EQ.1)THEN
                 ET=ET/98.07
                 E0=E0/98.07
                 XM0=XM0/9.807
                 XMT=XMT/9.807
               END IF
              WRITE(1,9990)IH,NC,T0,TJ,XM0,XMT,ALF,E0,ET
             END IF
      ELSE IF(NC.EQ.62)THEN
            READ(11,*)NC,WR,WSN,WB,WS
            WRITE(1,9990)IH,NC,WR,WSN,WB,WS
                       WS=3.1415926535*WS/180
      ELSE IF(NC.EQ.71)THEN
            IMAT=1
            READ(11,6662,ERR=600)NC,DAT1
            WRITE(1,6661)IH,NC,DAT1
              MDW=99
              OPEN(UNIT=18,FILE=DAT1,STATUS='OLD',ERR=9807)
              READ(18,*)DAT1
              READ(18,*)MDW
              READ(18,*)IS
              READ(18,*)((RMATS(I,J),J=1,2),I=1,IS)
              READ(18,*)IE
              READ(18,*)((RMATE(I,J),J=1,2),I=1,IE)
              READ(18,*)IA
              READ(18,*)((RMATA(I,J),J=1,2),I=1,IA)
9807              CONTINUE
              CLOSE(UNIT=18)
              CALL MAT0(DAT1,S,T0,TJ,XM0,XMT,ALF,E0,ET)

               IF(ISRDW.EQ.1.AND.MDW.EQ.99)THEN
                 ET=ET/98.07
                 E0=E0/98.07
                 XM0=XM0/9.807
                 XMT=XMT/9.807
               ELSE IF(MDW.NE.1.AND.ISRDW.EQ.1)THEN
                 ET=ET/98.07
                 E0=E0/98.07
                 XM0=XM0/9.807
                 XMT=XMT/9.807
               ELSE IF(MDW.EQ.1.AND.ISRDW.NE.1)THEN
                 ET=ET*98.07
                 E0=E0*98.07
                 XM0=XM0*9.807
                 XMT=XMT*9.807
               END IF
                 WRITE(1,9990)IH,NC,T0,TJ,XM0,XMT,ALF,E0,ET
      ELSE IF(NC.EQ.12)THEN
            READ(11,*,ERR=600)NC,RHDJQ
            WRITE(1,9990)IH,NC,RHDJQ
      ELSE IF(NC.EQ.18)THEN
            READ(11,*,ERR=600)NC,NTH
            WRITE(1,9991)IH,NC,NTH
      ELSE IF(NC.EQ.-1)THEN
            READ(11,*,ERR=600)NC,ITM
            WRITE(1,9991)IH,NC,ITM
      ELSE IF(NC.EQ.-2)THEN
            READ(11,*,ERR=600)NC,Izg
            WRITE(1,9991)IH,NC,Izg
      ELSE IF(NC.EQ.40)THEN
            READ(11,*)nc,(IZD(I),I=5,10)
            WRITE(1,9999)IH,NC,(IZD(I),I=5,10)
      ELSE IF(NC.EQ.41)THEN
            READ(11,*)nc,ZDFC
            WRITE(1,9990)IH,NC,ZDFC
      ELSE IF(NC.EQ.42)THEN
            READ(11,*)nc,GJXS
            WRITE(1,9990)IH,NC,GJXS
      else if(nc.eq.43)then
            read(11,*)nc,isz
            write(1,9991)ih,nc,isz
      ELSE IF(NC.EQ.81)THEN
            READ(11,*)NC,XX,YY,ZZ
            WRITE(1,9990)IH,NC,XX,YY,ZZ
      ELSE IF(NC.EQ.810)THEN
            izg=1
            READ(11,*)NC,iX1,iY1,iZ1,ax,ay,az
            rx=ix1
            ry=iy1
            rz=iz1
            WRITE(1,9990)IH,NC,rX,rY,rZ,ax,ay,az
            ax=ax*3.1415926535/180
            ay=ay*3.1415926535/180
            az=az*3.1415926535/180
      ELSE IF(NC.EQ.82)THEN
            READ(11,*)NC,DZXS
            WRITE(1,9990)IH,NC,DZXS
            IEW=0
            IFJ6=6
      ELSE IF(NC.EQ.83)THEN
            ifj6=6
            IEW=1
            READ(11,*)NC,FHZJM,FHZXS
            WRITE(1,9990)IH,NC,FHZJM,FHZXS
      ELSE IF(NC.EQ.100)THEN
            READ(11,*)NC,XXX,YYY,ZZZ
            WRITE(1,9991)IH,NC,nc,XXX,YYY,ZZZ
            XAD = XXX
            YAD = YYY
            ZAD = ZZZ
      else if(nc.eq.999)then
         read(11,*)nc
         write(1,9990)ih,nc
         itj=nc
      else if(nc.eq.-999)then
         read(11,*)nc
         write(1,9990)ih,nc
         itj=nc
      ELSE IF(NC.EQ.84)THEN
            READ(11,*)NC,FYP
            WRITE(1,9990)IH,NC,FYP
      ELSE IF(NC.EQ.85)THEN
            READ(11,*)NC,NREC
            DO 878 I=1,NREC
            READ(11,*)HIGHT(I),WINDF(I)
878             CONTINUE
            
            WRITE(1,9991)IH,NC,NREC
            DO 1001 I=1,NREC
             WRITE(1,9990)IH,NC,HIGHT(I),WINDF(I)
1001            CONTINUE
      ELSE IF(NC.EQ.10)THEN
            NF=NF+1
            
            READ(11,*)NC,IS1,IM1,LX5

            WRITE(1,9998)IH,NC,IS1,IM1,LX5

            IF(NF.EQ.1)IFZSH=IS1                    

      ELSE IF(NC.LE.7.AND.NC.GE.0)THEN
            NFZ=NFZ+1
            LX1=100
            IMN=NFZ+10000
            KMN=NZG+10000
            ISN=IMN-1


            IF(NQ.EQ.10)ISN=IS1
      I3=0
      I2=0
      I4=0
      READ(11,*)I1
      READ(11,*)I1
      ixhnc=0
549     continue
      if(i1.gt.10)then
        ixhnc=ixhnc+1
        read(11,*)i1
        goto 549
      else
	  if(ixhnc.eq.0)goto 609
        do 608 i3=1,ixhnc
        backspace 11
608       continue
609       continue
      end if
        BACKSPACE 11
        BACKSPACE 11
            IF(I1.EQ.10)IMN=IM1
            IF(I1.EQ.10)LX1=LX5

           LQ=.FALSE.
           IF(NC.GE.1.AND.NC.LE.6)THEN
            IF(NC.EQ.1)THEN
                  if(izg.eq.0)then
                    READ(11,*,ERR=600)NC,ISW,IMW,X,Y,Z
                    WRITE(1,7771)IH,NC,isn,imn,ISW,IMW,X,Y,Z
                  else
                    READ(11,*,ERR=600)NC,ISW,IMW,rrl2
                    WRITE(1,7771)IH,NC,isn,imn,ISW,IMW,rrl2
                  end if
            ELSE IF(NC.EQ.3)THEN
                  if(izg.eq.0)then
                  READ(11,*,ERR=600)NC,ISW,IMW,X,Y,Z,DW3,S3,GW3
                  WRITE(1,7771)IH,NC,isn,imn,ISW,IMW,X,Y,Z,DW3,S3,GW3
                  else
                  READ(11,*,ERR=600)NC,ISW,IMW,rrl2,DW3,S3,GW3
                  WRITE(1,7771)IH,NC,isn,imn,ISW,IMW,rrl2,DW3,S3,GW3
                  end if
            ELSE IF(NC.EQ.2)THEN
                  if(izg.eq.0)then
                  READ(11,*,ERR=600)NC,ISW,IMW,X,Y,Z,DW3,S3,GW3
                  WRITE(1,7771)IH,NC,isn,imn,ISW,IMW,X,Y,Z,DW3,S3,GW3
                  else
                  READ(11,*,ERR=600)NC,ISW,IMW,rrl2,DW3,S3,GW3
                  WRITE(1,7771)IH,NC,isn,imn,ISW,IMW,rrl2,DW3,S3,GW3
                  end if
            ELSE IF(NC.EQ.5)THEN
            if(izg.eq.0)then
            READ(11,*,ERR=600)NC,ISW,IMW,X,Y,Z,D1,S1,AJD,DW3,S3,GZ
         WRITE(1,7771)IH,NC,isn,imn,ISW,IMW,X,Y,Z,D1,S1,AJD,DW3,S3,GZ
            else
         READ(11,*,ERR=600)NC,ISW,IMW,rrl2,D1,S1,AJD,DW3,S3,GZ
         WRITE(1,7771)IH,NC,isn,imn,ISW,IMW,rrl2,D1,S1,AJD,DW3,S3,GZ
            end if
             if(d1.lt.50.or.dw3.lt.50.or.s1.gt.200.or.s3.gt.200.or.
     +            d1.gt.3000.or.dw3.gt.3000)call error(nc,ih)
              if(dw3.gt.d1)then
                 dw13=dw3
                 s13=s3
                 dw3=d1
                 s3=s1
                 d1=dw13
                 s1=s3
              end if
              RYLJQ=0.2+0.01*AJD*((DW3/S3)**(1./2.))
              IF (RYLJQ.GT.2.0)THEN
                   RYLJQ=2.
              ELSE IF(RYLJQ.LT.1.0)THEN
                   RYLJQ=1.
              END IF
              RYLQ(ISN)=RYLJQ
              RYLQ(IMN)=RYLJQ
            ELSE IF(NC.EQ.4)THEN
                  NGJ=NGJ+1
               if(izg.eq.0)then
                 READ(11,*,ERR=600)NC,ISW,IMW,X,Y,Z,GZ,GA
                 WRITE(1,7771)IH,NC,isn,imn,ISW,IMW,X,Y,Z,GZ,GA
               else
                 READ(11,*,ERR=600)NC,ISW,IMW,rrl2,GZ,GA
                 WRITE(1,7771)IH,NC,isn,imn,ISW,IMW,rrl2,GZ,GA
               end if
            ELSE IF(NC.EQ.6)THEN
               if(izg.eq.0)then
                 READ(11,*,ERR=600)NC,ISW,IMW,X,Y,z,GZ
                 WRITE(1,7771)IH,NC,isn,imn,ISW,IMW,X,Y,Z,GZ
               else
                 READ(11,*,ERR=600)NC,ISW,IMW,rrl2,GZ
                 WRITE(1,7771)IH,NC,isn,imn,ISW,IMW,rrl2,GZ
               end if
            END IF
                  if(izg.ne.0)then
                    x=ix1*rrl2*dcos(ax)
                    y=iy1*rrl2*dcos(ay)
                    z=iz1*rrl2*dcos(az)
                  end if
            XL=DSQRT(X**2+Y**2+Z**2)
            if(xl.gt.20)call error(nc,ih)
            NZG=NZG+1
            XXL=X/XL
            YXL=Y/XL
            ZXL=Z/XL
            YSL=XL
            if(iprn.eq.1.and.itj.eq.999)then
             n79=79
             if(nc.eq.1)write(23,201)n79,nc,nc,nc,dw,s,tj,pj,xl,xl
             if(nc.eq.5)write(23,201)n79,nc,nc,nc,d1,s1,tj,pj,dw3,s3
             if(nc.eq.6)write(23,201)n79,nc,nc,nc,dw,s,tj,pj,xl,xl
            end if
            IF(XL.EQ.0.0)CALL ERROR(110,IH)
            RK=1.
            IF(NQ.EQ.0.AND.ITM.EQ.2)THEN
                  X=X*(1.-HL/XL)
                  Y=Y*(1.-HL/XL)
                  Z=Z*(1.-HL/XL)
                  XL=XL-HL
                  IF(abs(XL).LE.0.0000001)CALL ERROR(110,IH)
            END IF
            IF(NC.EQ.6)THEN
            READ(11,*)((DK(I,J),J=1,6),I=1,6)
            WRITE(1,5741)((DK(I,J),J=1,6),I=1,6)
               IF(ISRDW.EQ.1)THEN
                 XYXS=10.
                 XJXS=1./100.
               ELSE
                 XYXS=10./9.807
                 XJXS=1./9.807/100.
               END IF
               DO 320 I=1,3
               DO 320 J=1,3
                  DK(I,J)=XYXS*DK(I,J)
                  II=I+3
                  JJ=J+3
                  DK(II,JJ)=XJXS*DK(II,JJ)
                  DK(II,J)=XYXS*DK(II,J)
320               CONTINUE      
            END IF                  
           ELSE
		   
            NWG=NWG+1

            IF(ITM.EQ.3)THEN
                  READ(11,*,ERR=600)NC,X,Y,Z,RC,SI,LX2
                  SI=PI*SI/180
                  XL=DSQRT(X*X+Y*Y+Z*Z)
            ELSE

            if(izg.eq.0)then
               READ(11,*,ERR=600)NC,ISW,IMW,RC,SI,LX2
               WRITE(1,7770)IH,NC,isn,imn,ISW,IMW,RC,SI,LX2
            else
         READ(11,*,ERR=600)NC,ISW,IMW,RC,SI,LX2,ix1,iy1,iz1,ax,ay,az
           WRITE(1,7772)IH,NC,ISW,IMW,RC,SI,LX2,ix1,iy1,iz1,ax,ay,az
            ax=ax*3.1415926535/180
            ay=ay*3.1415926535/180
            az=az*3.1415926535/180

            end if

            if(rc.gt.5.or.rc.lt.0.05.or.si.gt.95.or.si.lt.5)
     +                 call error(nc,ih)
            if(iprn.eq.1.and.itj.eq.999)then
             nw=1
             n79=79
             rlw=3.1415926535*si*rc/180
             if(itm.eq.2)then
              SI1=PI*SI/180
              rlw=rlw-2*RC*DTAN(SI1/2)
              write(23,201)n79,nw,nw,nw,dw,s,tj,pj,rlw,rlw
             else
              if((rc*1000/dw).gt.3)
     +            write(23,201)n79,nw,nw,nw,dw,s,tj,pj,rlw,rlw
             end if
              write(23,201)n79,nc,nc,nc,dw5,s5,tj,pj,si,rc
            end if
            SI=PI*SI/180
            IF(NQQ.EQ.-1)GOTO 1111
            HL=RC*DTAN(SI/2)
            IF(HL.EQ.0.0)CALL ERROR(100,IH)
            if(izg.eq.0)then
             READ(11,*,ERR=600)K1,K1,K1,X1,Y1,Z1
            else
             READ(11,*,ERR=600)K1,K1,K1,rrl1
                    x1=ix1*rrl1*dcos(ax)
                    y1=iy1*rrl1*dcos(ay)
                    z1=iz1*rrl1*dcos(az)
            end if
            BACKSPACE(11)
            YL=DSQRT(X1**2+Y1**2+Z1**2)
            IF(YL.EQ.0.0)CALL ERROR(110,IH)
            X=(X/XL+X1/YL)*HL
            Y=(Y/XL+Y1/YL)*HL
            Z=(Z/XL+Z1/YL)*HL
            XL=DSQRT(X**2+Y**2+Z**2)
            IF(XL.LE.0.0000001)CALL ERROR(110,IH)
            IF(ITM.EQ.2)THEN
                  QX=XQ*(1.-HL/QL)
                  QY=YQ*(1.-HL/QL)
                  QZ=ZQ*(1.-HL/QL)
                  QL=QL-HL
                  IF(ABS(QL).LE.0.0001)CALL ERROR(110,IH)
            END IF
1111            CONTINUE
C---------ATAN MEANS COT
            IF(WR.LT.0.0001.AND.LX2.LE.2)THEN
               RF=((DW5-S5)/2)**2
               RAN=1000.0*S5*RC/RF
            ELSE IF(LX2.LE.2)THEN
               RF=WR**2
               RAN=1000.0*WSN*RC/RF
            ELSE IF(WB.GT.0.0001.AND.LX2.GE.3)THEN
             RF=WR**2
             WBB=WR*(1.+DTAN(WS))
             IF(WB.LT.WBB)THEN
                 RAN=WB*WSN*DATAN(WS)/(2*RF)
                 IF(RC.LT.0.0001)RC=(WB*DATAN(WS))/2./1000.
             ELSE
                 RAN=WSN*DATAN(WS)/(WR)
                 IF(RC.LT.0.0001)RC=WR*(1+DATAN(WS))/2./1000.
             END IF
            ELSE
                 CALL ERROR(160,IH)
            END IF
                 IF(RAN.EQ.0.0)CALL ERROR(160,IH)

            RK=1.65/RAN

            IF(RK.LT.1.0)RK=1.

            WYJQ=0.9/(RAN**(2./3.))
            IF(WYJQ.LT.1.0)WYJQ=1.0
             END IF   
      
           END IF
            LQ=.FALSE.
            IF(XL.EQ.0)CALL ERROR(110,IH)

            IF(NC.NE.0.AND.NQ.NE.0.AND.XL.NE.0.0.AND.QL.NE.0.0)     
     1          COSINE=(X*XQ+Y*YQ+Z*ZQ)/(YSL*QL)                
            IF(NC.EQ.0.OR.NQ.EQ.0)GOTO 340
           IF(ISN.NE.IMQ.OR.COSINE.LT.0.998)THEN
            IF(NST.EQ.0.OR.NFZ.EQ.1)GOTO 340
                  IF(ISTDH(ISN).GE.1)THEN
                        LQ=.TRUE.
                        GOTO 340
                  END IF
              IF(ISN.NE.IMQ)CALL ERROR(300,IH)
              IF(COSINE.LT.0.998.AND.NC.NE.4)CALL ERROR(111,IH)
           END IF
340             CONTINUE
           ISH(NFZ)=ISN
           IMH(NFZ)=IMN
           LX(NFZ)=LX1
           ITLX(NFZ)=NTH

           DW2=DW/10
           S2=S/10
           IF(NC.EQ.2.OR.NC.EQ.3.OR.NC.EQ.5)DW2=DW3/10
           IF(NC.EQ.2.OR.NC.EQ.3.OR.NC.EQ.5)S2=S3/10
           IX=1
           GW2=PI*((DW-2*S)/2)**2*10.
           G1W=GW1
           G1WQ=GW1
           G2W=GW2
           G5W=GW5
           G5WQ=GW5
           G2W=GW2
           IF(NC.EQ.4.OR.NC.EQ.5.OR.NC.EQ.6)G1W=GZ
           IF(NC.EQ.2.OR.NC.EQ.3)G1W=GW3
      IF(Y.GT.0.00000001.OR.Y.LT.0.0000001)THEN
           IF(.NOT.LQ)THEN
            IF(IZBXZ.EQ.1)THEN
            YADAD=XAD+X/2
            ELSE IF(IZBXZ.EQ.2)THEN
            YADAD=YAD+Y/2
            ELSE IF(IZBXZ.EQ.3)THEN
            YADAD=ZAD+Z/2
            END IF
           ELSE
            DO 3910 IV=1,NFZ
                  IF(IMH(IV).EQ.ISN)GOTO 3920
3910            CONTINUE
            CALL ERROR(300,IH)
3920            CONTINUE
            IF(IZBXZ.EQ.1)THEN
            YADAD=XR(IV)+X/2
            ELSE IF(IZBXZ.EQ.2)THEN
            YADAD=YR(IV)+Y/2
            ELSE IF(IZBXZ.EQ.3)THEN
            YADAD=ZR(IV)+Z/2
            END IF
           END IF
      END IF
           IF(IEW.EQ.0)THEN
            G1WX=G1W*XX*DZXS
            G1WY=G1W*YY*DZXS
            G1WZ=G1W*ZZ*DZXS
           END IF
           IF(IEW.EQ.1)THEN
             IF(YADAD.LE.HIGHT(1))THEN
             WHP=WINDF(1)*FYP
             ELSE IF(YADAD.GT.HIGHT(NREC))THEN
             WHP=WINDF(NREC)*FYP
             ELSE
            DO 3921 IHIH=1,NREC
            JHJH=IHIH+1
            IF(YADAD.GT.HIGHT(IHIH).AND.YADAD.LE.HIGHT(JHJH))THEN
             WHP=WINDF(JHJH)*FYP
            END IF
3921            CONTINUE
             END IF
       FHZ=(FHZJM/1000.0)*FHZXS*WHP
            if(nc.ne.0)al=xl
            al=xl

            g1wx=0.
            g1wy=0.
            g1wz=0.

            if(xx.ne.0)G1WX=FHZ*XX*((y*y+z*z)**.5)/aL
            if(yy.ne.0)G1WY=FHZ*YY*((x*x+z*z)**.5)/aL
            if(zz.ne.0)G1WZ=FHZ*ZZ*((x*x+y*y)**.5)/aL

            IF(ITM.EQ.2.AND.NC.EQ.0)THEN
             if(xx.ne.0)G1WXQ=FHZQ*XX*((qy*qy+qz*qz)**.5)/QL
             if(yy.ne.0)G1WYQ=FHZQ*YY*((qx*qx+qz*qz)**.5)/QL
             if(zz.ne.0)G1WZQ=FHZQ*ZZ*((qx*qx+qy*qy)**.5)/QL
            END IF
            FHZQ=FHZ
           END IF
           IF(ITM.EQ.2.AND.NC.EQ.0)THEN
            WXR(NFZ-1)=G1WXQ
            WYR(NFZ-1)=G1WYQ
            WZR(NFZ-1)=G1WZQ

            XAD=XAD-XQ+QX
            YAD=YAD-YQ+QY
            ZAD=ZAD-ZQ+QZ
            BACKSPACE 13
            IF(NQ.EQ.4)THEN
                ALF1=GA
                ALF2=(alfsy/alf)*GA
                ALF3=(alfcy/alf)*GA
                 if(ga.le.0.000001)then
                   ALF1=ALF
                   ALF2=ALFSY
                   ALF3=ALFCY
                 end if
                ET1=ETT*GJXS
                E01=E00*GJXS
                ET2=ETSS*GJXS
                E02=E0SS*GJXS
                ET3=ETCC*GJXS
                E03=E0CC*GJXS
             ELSE
                ALF1=ALF
                ALF2=ALFSY
                ALF3=ALFCY
                ET1=ETT
                E01=E00
                ET2=ETSS
                E02=E0SS
                ET3=ETCC
                E03=E0CC
             END IF
       WRITE(13)NQ,ISQ,IMQ,LXQ,DWQ,SQ,ETT,E00,GGZQ,
     +          G1WXQ,G1WYQ,G1WZQ,QL,RC,
     1          SI,QX,QY,QZ,RKQ,G2W,TJ,T0,ALF1,SA,WYJQ,PJJ,XM00,XMTT
             IF(IFJ9.NE.0)THEN
             BACKSPACE 14
          WRITE(14)ET2,E02,TJSY,T0SY,ALF2,PJSS,XM0SS,XMTSS,GGZQ
                isz9=1
             END IF
             IF(IFJ7.NE.0)THEN
             BACKSPACE 21
          WRITE(21)ET3,E03,TJCY,T0CY,ALF3,PJCC,XM0CC,XMTCC,GGZQ
             END IF

            WXR(NFZ-1)=G1WXQ
            WYR(NFZ-1)=G1WYQ
            WZR(NFZ-1)=G1WZQ        

           END IF

           IF(RHDJQ.GT.RYLQ(ISN).AND.ISW.eq.1)THEN
             RSYJQ=RHDJQ
             RYLQ(ISN)=RHDJQ
           ELSE IF(RYLQ(ISN).GE.1)THEN
             RSYJQ=RYLQ(ISN)
           ELSE
             RSYJQ=1.
           END IF

           IF(RHDJQ.GT.RYLQ(IMN).AND.IMW.eq.1)THEN
             RMYJQ=RHDJQ
             RYLQ(IMN)=RHDJQ
           ELSE IF(RYLQ(IMN).ge.1)THEN
             RMYJQ=RYLQ(IMN)
           ELSE
             RMYJQ=1.
           END IF
           IF(ISRDW.NE.1)THEN
            ETT=ET/98.07
            E00=E0/98.07
            PJJ=PJ*100/9.807
            XM00=XM0/9.807
            XMTT=XMT/9.807
            IF(IFJ9.NE.0)THEN
            ETSS=ETSY/98.07
            E0SS=E0SY/98.07
            PJSS=PJSY*100/9.807
            XM0SS=XM0SY/9.807
            XMTSS=XMTSY/9.807
            END IF
            IF(IFJ8.NE.0)THEN
            ETCC=ETCY/98.07
            E0CC=E0CY/98.07
            PJCC=PJCY*100/9.807
            XM0CC=XM0CY/9.807
            XMTCC=XMTCY/9.807
            END IF
           ELSE
            ETT=ET
            E00=E0
            PJJ=PJ
            XM00=XM0
            XMTT=XMT
            IF(IFJ9.NE.0)THEN
            ETSS=ETSY
            E0SS=E0SY
            PJSS=PJSY
            XM0SS=XM0SY
            XMTSS=XMTSY
            END IF
            IF(IFJ8.NE.0)THEN
            ETCC=ETCY
            E0CC=E0CY
            PJCC=PJCY
            XM0CC=XM0CY
            XMTCC=XMTCY
            END IF
           END IF
              IF(NC.EQ.0)GGZ=G5W
              IF(NC.NE.0)GGZ=G1W
           IF(NC.EQ.0)THEN
           WRITE(13)NC,ISN,IMN,LX1,DW55,S55,ETT,E00,GGZ,
     +           G1WX,G1WY,G1WZ,
     1           XL,RC,SI,X,Y,
     1           Z,RK,G2W,TJ,T0,ALF,SA,WYJQ,PJJ,XM00,XMTT
             IF(IFJ9.NE.0)THEN
          WRITE(14)ETSS,E0SS,TJSY,T0SY,ALFSY,PJSS,XM0SS,XMTSS,GGZ
                isz9=1
             END IF
             IF(IFJ7.NE.0)THEN
          WRITE(21)ETCC,E0CC,TJCY,T0CY,ALFCY,PJCC,XM0CC,XMTCC,GGZ
             END IF
           ELSE
            IF(NC.EQ.4)THEN
                ALF1=GA
                ALF2=GA
                ALF3=GA
                 if(ga.le.0.000001)then
                   ALF1=ALF
                   ALF2=ALFSY
                   ALF3=ALFCY
                 end if
                ET1=ETT*GJXS
                E01=E00*GJXS
                ET2=ETSS*GJXS
                E02=E0SS*GJXS
                ET3=ETCC*GJXS
                E03=E0CC*GJXS
             ELSE
                ALF1=ALF
                ALF2=ALFSY
                ALF3=ALFCY
                ET1=ETT
                E01=E00
                ET2=ETSS
                E02=E0SS
                ET3=ETCC
                E03=E0CC
             END IF
             if(isw.eq.2.or.imw.eq.2)then
                dw2=dw55
                s2=s55
                ggz=g5w
             end if
           WRITE(13)NC,ISN,IMN,LX1,DW2,S2,ET1,E01,GGZ,
     +           G1WX,G1WY,G1WZ,
     1           XL,RC,SI,X,Y,
     1           Z,RK,G2W,TJ,T0,ALF1,SA,WYJQ,PJJ,XM00,XMTT
             IF(IFJ9.NE.0)THEN
        WRITE(14)ET2,E02,TJSY,T0SY,ALF2,PJSS,XM0SS,XMTSS,GGZ
                isz9=1
             END IF
             IF(IFJ7.NE.0)THEN
        WRITE(21)ET3,E03,TJCY,T0CY,ALF3,PJCC,XM0CC,XMTCC,GGZ
             END IF
           READ(11,*)NH
           BACKSPACE 11
           END IF

            IF(NC.EQ.6)WRITE(13)DK
      
            WXR(NFZ)=G1WX
            WYR(NFZ)=G1WY
            WZR(NFZ)=G1WZ
           DWQ=DW2
           SQ=S2
           G1WXQ=G1WX
           G1WYQ=G1WY
           G1WZQ=G1WZ
           GGZQ=GGZ
           NQ=NC

           ISQ=ISN    
           IMQ=IMN
           LXQ=LX1
           XQ=X
           YQ=Y
           ZQ=Z
           QL=XL
           IXQ=IX
           RKQ=RK
C______________________________________________________________


C________________________ZUEBIO JIAOHE___________________________
           IF(.NOT.LQ)THEN
            XAD=XAD+X
            YAD=YAD+Y
            ZAD=ZAD+Z
           ELSE
            DO 391 IV=1,NFZ
                  IF(IMH(IV).EQ.ISN)GOTO 392
391             CONTINUE
            CALL ERROR(1250,IH)
392             XAD=XR(IV)+X
            YAD=YR(IV)+Y
            ZAD=ZR(IV)+Z
      
           END IF
           XR(NFZ)=XAD
           YR(NFZ)=YAD
           ZR(NFZ)=ZAD
           pr(nfz)=pjj
           ar(nfz)=alf
           tr(nfz)=tj
      ELSE IF(NC.EQ.215)THEN
            IFJ5=5
            NJH5=NJH5+1
            IF(NJH5.GT.300)CALL ERROR(215,IH)
       READ(11,*,ERR=600)NC,IDH,
     +  HZ5(NJH5,1),HZ5(NJH5,2),HZ5(NJH5,3),
     +  HZ5(NJH5,4),HZ5(NJH5,5),HZ5(NJH5,6)
            WRITE(1,9981)IH,NC,IDH,(HZ5(NJH5,I),I=1,6)
            IF(ISRDW.NE.1)THEN
                  DO 1414 I=1,6
1414                    HZ5(NJH5,I)=HZ5(NJH5,I)/9.8
            END IF
            IHZ5(NJH5)=IDH
      elSE IF(NC.EQ.210)THEN

       NJH=NJH+1
       IF(NJH.GT.300)CALL ERROR(210,IH)
       READ(11,*,ERR=600)NC,IDH,IGK1,
     + HZ(NJH,1),HZ(NJH,2),HZ(NJH,3),HZ(NJH,4),HZ(NJH,5),HZ(NJH,6)
       WRITE(1,9982)IH,NC,IDH,IGK1,(HZ(NJH,I),I=1,6)
       if(ISRDW.NE.1)THEN
      do 1515 I=1,6
1515                    HZ(NJH,I)=HZ(NJH,I)/9.8
       END IF
c      do 1616 I=1,6
c      if(hz(njh,i).eq.0.)HZ(NJH,I)=0.01
c1616  continue
       IF(IGK1.EQ.5)IFJ5=5
       IF(IGK1.EQ.6)IFJ6=6
       IF(IGK1.EQ.7)IFJ7=7
       IF(IGK1.EQ.8)IFJ8=8
       IF(IGK1.EQ.9)IFJ9=9
       IF(IGK1.EQ.10)IFJ10=10
       JHZ(NJH)=IDH
       IHZ(NJH)=IGK1
      ELSE IF(NC.EQ.220)THEN
            NWY=NWY+1
            IF(NWY.GT.50)CALL ERROR(220,IH)
            READ(11,*,ERR=600)NC,IDH,IGK1,(WY(NWY,I),I=1,6)
            WRITE(1,9992)IH,NC,IDH,IGK1,(WY(NWY,I),I=1,6)
                        WY(NWY,1)=WY(NWY,1)/10.
                        WY(NWY,2)=WY(NWY,2)/10.
                        WY(NWY,3)=WY(NWY,3)/10.
                  IF(IGK1.EQ.3)THEN
                        WY(NWY,1)=-WY(NWY,1)
                        WY(NWY,2)=-WY(NWY,2)
                        WY(NWY,3)=-WY(NWY,3)
                        WY(NWY,4)=-WY(NWY,4)
                        WY(NWY,5)=-WY(NWY,5)
                        WY(NWY,6)=-WY(NWY,6)
                  END IF
            JWY(NWY)=IDH
            IWY(NWY)=IGK1
      ELSE IF(NC.EQ.230)THEN
            NDD=NDD+1
            IF(NDD.GT.40)CALL ERROR(230,IH)
            READ(11,*,ERR=600)NC,IDH,(DD(NDD,I),I=1,6)
            WRITE(1,9991)IH,NC,IDH,(DD(NDD,I),I=1,6)
       if(iprn.eq.1)WRITE(23,201)NC,NC,IDH,IDH,(DD(NDD,I),I=1,6)
      if(iprn.eq.2)WRITE(23,4385)NC,IDH,IDH,
     +                           (DD(NDD,I),I=1,6),' -',' -'
      if(iprn.eq.3)WRITE(50,5002)NC,NC,IDH,IDH,
     +                           (DD(NDD,I),I=1,6)
5002  FORMAT(4I12,6F12.3)     
4385  format(i5,i7,i7,6f15.3,1x,a13,1x,a13)
            JD(NDD)=IDH
            DD(NDD,1)=DD(NDD,1)/10.
            DD(NDD,2)=DD(NDD,2)/10.
            DD(NDD,3)=DD(NDD,3)/10.
201             FORMAT(2I5,2I10,6F15.3)
      ELSE IF(NC.EQ.237)THEN
            N38=N38+1
            IF(N38.GT.40)CALL ERROR(237,IH)
            READ(11,*,ERR=600)NC,III,(D8(N38,I),I=1,6)
            WRITE(1,9991)IH,NC,III,(D8(N38,I),I=1,6)
       if(iprn.eq.1)WRITE(23,201)NC,NC,III,III,(D8(N38,I),I=1,6)
            D8(N38,1)=D8(N38,1)/10.
            D8(N38,2)=D8(N38,2)/10.
            D8(N38,3)=D8(N38,3)/10.
      ELSE IF(NC.EQ.236)THEN
            N36=N36+1
            IF(N38.GT.40)CALL ERROR(236,IH)
            READ(11,*,ERR=600)NC,III,(D6(N36,I),I=1,6)
            WRITE(1,9991)IH,NC,III,(D6(N36,I),I=1,6)
       if(iprn.eq.1)WRITE(23,201)NC,NC,III,III,(D6(N36,I),I=1,6)
            D6(N36,1)=D6(N36,1)/10.
            D6(N36,2)=D6(N36,2)/10.
            D6(N36,3)=D6(N36,3)/10.
      ELSE IF(NC.EQ.240)THEN
            NLL=NLL+1
            IF(NLL.GT.50)CALL ERROR(240,IH)
            READ(11,*)NC,IDH,(RL(NLL,I),I=1,3)
            WRITE(1,9991)IH,NC,IDH,(RL(NLL,I),I=1,3)
            LL(NLL)=IDH
            RL(NLL,1)=RL(NLL,1)/10.
            RL(NLL,2)=RL(NLL,2)/10.
            RL(NLL,3)=RL(NLL,3)/10.
      ELSE IF(NC.EQ.241)THEN
            INLL=INLL+1
            IF(NLL.GT.50)CALL ERROR(241,IH)
            READ(11,*)NC,IDH,RRL(INLL)
            WRITE(1,9991)IH,NC,IDH,RRL(INLL)

            RRL(INLL)=-RRL(INLL)/10
            LLL(INLL)=IDH
      ELSE IF(NC.EQ.250)THEN
            NZB=NZB+1
            IF(NZB.GT.20)CALL ERROR(250,IH)
            READ(11,*,ERR=600)NC,IDH,(YZB(NZB,I),I=1,3)
            WRITE(1,9991)IH,NC,IDH,(YZB(NZB,I),I=1,3)
            IZB(NZB)=IDH
      ELSE IF(NC.EQ.260)THEN
            NHD=NHD+1
            IF(NHD.GT.400)CALL ERROR(260,IH)
            READ(11,*,ERR=600)NC,IHD(NHD),HD(NHD)
            WRITE(1,9981)IH,NC,IHD(NHD),HD(NHD)
            IF(ISRDW.NE.1)HD(NHD)=HD(NHD)/9.807
            HD(NHD)=-HD(NHD)                
      ELSE IF(NC.EQ.290)THEN
            isc=isc+1
            IF(isc.GT.50)CALL ERROR(260,IH)
            READ(11,*,ERR=600)NC,iscnd(isc)
            WRITE(1,9981)IH,NC,iscnd(isc)
C        ELSE IF(NC.EQ.280)THEN
C-----SPRING PRESS
C                NTP=NTP+1
C                IF(NTP.GT.400)CALL ERROR(70,IH)
C                READ(11,*)NC,ISP1,SPP1
C                WRITE(11,9991)IH,NC,ISP1,SPP1
C
C                SPP(ISP1)=SPP1
      else if(nc.eq.45)then
            read(11,*,err=600)nc,iprn1
            write(1,9991)ih,nc,iprn1
      else if(nc.eq.46)then
            read(11,*,err=600)nc,iltd0
            write(1,9991)ih,nc,iltd0
      else if(nc.eq.47)then
            read(11,*,err=600)nc,izh
            write(1,9991)ih,nc,izh
      else if(nc.eq.44)then
       iprn=1
       read(11,6662)nc,wjm
       write(1,9993)ih,nc,wjm
       open(23,file=wjm,status='UNKNOWN')
       close(unit=23,status='delete')
       open(23,file=wjm,status='new')
       write(23,201)itm,isrdw,iscdw,izbxz,
     +              fxhxs,fxhxs,fxhxs,fxhxs,fxhxs,fxhxs
      
      else if(nc.eq.49)then
             iprn=2
             read(11,6662)nc,wjm
             write(1,9993)ih,nc,wjm
c             write(1,*)'  ',nc,'     ',wjm
       open(23,file=wjm,status='UNKNOWN')
       close(unit=23,status='delete')
             open(23,file=wjm,status='new')
             ngd=29
             fzbxz=izbxz
             write(23,4385)ngd,isrdw,iscdw,
     +                     FXHXS,XS10(5),XS10(6),fzbxz,f,f,' -',' -'
C             fXHXS=izbxz
C             write(23,4385)ngd,isrdw,iscdw,
C     +               fxhxs,fxhxs,fxhxs,fxhxs,fxhxs,fxhxs,' -',' -'
      else if(nc.eq.50)then
             iprn=3
             read(11,6662)nc,wjm
             write(1,9993)ih,nc,wjm
       open(50,file=wjm,status='UNKNOWN')
       close(unit=50,status='delete')
             open(50,file=wjm,status='new')
             ngd=29
             fzbxz=izbxz
             write(50,5001)ngd,isrdw,iscdw,
     +                     FXHXS,XS10(5),XS10(6),fzbxz,f,f,f
5001  format(3I12,7f12.3)     
      ELSE IF(NC.EQ.300)THEN
            NST=NST+1
            READ(11,*,ERR=600)NC,IST,RYLJQ
            WRITE(1,9991)IH,NC,IST,RYLJQ
            RYLQ(IST)=RYLJQ
            ISTDH(IST)=1
      ELSE IF(NC.EQ.390)THEN
         READ(11,*,ERR=600)
     +  NC,IST,RSTRMH,RSTSNH,RSTRMB,RSTSNB,RSTRP,RSTSB
         WRITE(1,9991)
     +  IH,NC,IST,RSTRMH,RSTSNH,RSTRMB,RSTSNB,RSTRP,RSTSB
          RYLJQ=1.5*((RSTRMH/RSTSNH)**(2./3.))*
     +                 ((RSTRMB/RSTRMH)**(1./2.))*
     +                 (RSTSNB/RSTSNH)*(RSTRMB/RSTRP)
            RYLQ(IST)=RYLJQ
            RKWJ(IST)=PI*RSTRMB*RSTRMB*RSTSB/1000.
            ISTDH(IST)=2

      ELSE IF(NC.GE.310.AND.NC.LE.380)THEN
      
C___________________SURU SANTONG_____________________

           NST=NST+1
           IF(NST.GT.50)CALL ERROR(310,IH)
           IF(NC.EQ.310)THEN
            READ(11,*,ERR=600)NC,IST,RSTR,RSTSN,RSTRMB,RSTSNB
            WRITE(1,9991)IH,NC,IST,RSTR,RSTSN,RSTRMB,RSTSNB
            CCXS=RSTSN/RSTR
           ELSE IF(NC.EQ.320)THEN
            READ(11,*,ERR=600)NC,IST,RSTR,RSTSN,RSTRMB,RSTSNB
            WRITE(1,9991)IH,NC,IST,RSTR,RSTSN,RSTRMB,RSTSNB
            CCXS=(4.4*RSTSN)/RSTR
           ELSE IF(NC.EQ.330)THEN
            READ(11,*,ERR=600)NC,IST,RSTR,RSTSN,RSTRMB,RSTSNB
            WRITE(1,9991)IH,NC,IST,RSTR,RSTSN,RSTRMB,RSTSNB
            CCXS=(3.25*RSTSN)/RSTR
           ELSE IF(NC.EQ.340)THEN
            READ(11,*,ERR=600)NC,IST,RSTR,RSTSN,RSTRMB,RSTSNB
            WRITE(1,9991)IH,NC,IST,RSTR,RSTSN,RSTRMB,RSTSNB
            CCXS=(3.25*RSTSN)/RSTR
           ELSE IF(NC.EQ.350)THEN
            READ(11,*,ERR=600)NC,IST,RSTR,RSTSN,RSTRMB,RSTSNB,RSTSR
            WRITE(1,9991)IH,NC,IST,RSTR,RSTSN,RSTRMB,RSTSNB,RSTSR
            CCXS=((RSTSN+(RSTSR/2))**(5./2.))/
     +               (RSTR*(RSTSN**(3./2.)))
           ELSE IF(NC.EQ.360)THEN
            READ(11,*,ERR=600)NC,IST,RSTR,RSTSN,RSTRMB,RSTSNB,RSTR2
            WRITE(1,9991)IH,NC,IST,RSTR,RSTSN,RSTRMB,RSTSNB,RSTR2
            CCXS=(1+RSTR2/RSTR)*RSTSN/RSTR
           END IF
            ISTDH(IST)=3
            RYLJQ=0.9/(CCXS**(2./3.))
            IF(RYLJQ.LT.1.)RYLJQ=1.0
            RYLQ(IST)=RYLJQ
            IF(RSTRMB.LT.RSTR)THEN
               ISTDH(IST)=2
               IF(RSTSN.LE.(RSTSNB*RYLJQ))THEN
                  SB3=RSTSN
               ELSE
                  SB3=RSTSNB*RYLJQ
               END IF
               RKWJ(IST)=PI*RSTRMB*RSTRMB*SB3/1000.
            END IF

      ELSE
            read(11,*)nc
            CALL ERROR(1100,IH)
      END IF

      IF(NFZ.GT.1000)CALL ERROR(1000,IH)
      GOTO 1000       

600     CALL ERROR(1600,IH)
      WRITE(1,*)IH
      GOTO 1000
700     IH=IH+1
      CALL ERROR(1700,IH)
      WRITE(1,*)IH
      GOTO 1000


27250   CONTINUE
      
      IF(NZB.NE.0)THEN
            DO 394 J=1,NFZ
                  DO 393 I=1,NZB
                        IF(IMH(J).EQ.IZB(I))THEN
                              XX=ABS(XR(J)-YZB(I,1))
                              YY=ABS(YR(J)-YZB(I,2))
                              ZZ=ABS(ZR(J)-YZB(I,3))
            IF(XX.GT.0.01.OR.YY.GT.0.01.OR.ZZ.GT.0.01)THEN
                              CALL ERROR(250,IH)
                              LZBC=.TRUE.
                       WRITE(1,*)'         ',
     +'                         COORDINATION WARNING'
                        WRITE(1,6767)IMH(J),XR(J),YR(J),ZR(J)
                        WRITE(1,7676)YZB(I,1),YZB(I,2),YZB(I,3)
                            END IF
                        END IF
393                     CONTINUE
394             CONTINUE
      IF(LZBC.or.iprn.eq.1)THEN
            n77=77
            r7=0
            ik=0
            DO 367 J=1,NFZ
            ik=ik+1
            if(lzbc)then
      IF(ik.eq.1.or.ik.ge.30)then
                  ik=1
                  CALL CPRINT(5)
                  WRITE(1,368)
               END IF
            WRITE(1,6767)IMH(J),XR(J),YR(J),ZR(J)
            end if
            if(j.ne.1)then
              if(xr(j-1).ne.xr(j).and.izbxz.eq.1)jj=jj+1
              if(yr(j-1).ne.yr(j).and.izbxz.eq.2)jj=jj+1
              if(zr(j-1).ne.zr(j).and.izbxz.eq.3)jj=jj+1
            else
              jj=1
            end if
      if(iprn.eq.1)
     +    WRITE(23,201)n77,n77,IMH(J),jj,
     +                 XR(J),YR(J),ZR(J),tr(j),pr(j),ar(j)
367             CONTINUE
368       format(//,35X,'POINT COODINATE',
     +           //,20X,'END POINT',7X,'X',12X,'Y',12X,'Z')
      END IF

      END IF
            
      if(njg.gt.0.or.ncw.gt.0)then
      call cprint(5)
      write(1,*)' '
      write(1,*)' '
      write(1,*)' '
      end if
      IF(NJG.GT.0)THEN
            WRITE(1,333)NJG
      write(1,334)'Line number','Warning kind'
334   format(14x,a11,31x,a12)     

            WRITE(1,311)((LJG(I,J),J=1,2),I=1,NJG)          
      END IF
      IF(NCW.GT.0)THEN
            WRITE(1,444)NCW
      write(1,334)'Line number','Error kind'
            WRITE(1,311)((LCW(I,J),J=1,2),I=1,NCW)
      END IF

      IF(IEW.ne.2)THEN
            ik=0
            DO 1785 j=1,NFZ
            ik=ik+1
       iF(ik.eq.1.or.ik.ge.30)then
                  ik=1
                  CALL CPRINT(5)
            if(iew.eq.0)
     +    WRITE(1,*)'                            EARTHQUAKE FORCE'
            if(iew.eq.1)
     +          WRITE(1,*)'                               wind FORCE'
            WRITE(1,*)'                    start       end    ',
     +' Fx         Fy        Fz       Yend'
            END IF
                 IF(WXR(J).NE.0.0.OR.WZR(J).NE.0.0)
     +         WRITE(1,6777)ish(j),IMH(J),WXR(J),WYR(J),WZR(J),YR(J)
6777           format(16x,i10,i10,4f10.3)

1785            CONTINUE
      END IF

      WRITE(1,9994)NLL,NDD,NST,NFZ
      WRITE(1,9996)NGJ,NWG




7786    FORMAT(/,25X,'TABLE')
7784    FORMAT(1X,14X,'STRAINT PIPE LENGTH')
7787    FORMAT(15X,I2,'----','DW=',F9.3,2X,'S=',F9.3,2X,'GW=',F9.3)
7785    FORMAT(//,15X,'NUMBER',3X,'STRAINT PIPE LENGTH',3X,
     1  'NUMBER OF ELBOW',3X,'NUMBER OF BENDS')
7788    FORMAT(16X,I2,10X,F8.2,2(15X,I3))
      NCW1=NCW
      NJG1=NJG
6666    FORMAT(//////////,
     +  20X,'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$',/,
     +  20X,'$                                                 $',/,
     +  20X,'$                  GLIF---V/3.1                   $',/,
     +  20X,'$      STRESS ANALYSIS OF ',A12,' PIPING     $',/,
     +  20X,'$                    ',A12,'                 $',/,
     +  20X,'$                                                 $',/,
     +  20X,'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$',
     +  /////////////////////,35X,'CHECKED BY     _________',
     +   ///,32X,              'CALCULATED BY     _________',///,
     +   25X,'NORTHEAST ELECTRIC POWER DESIGN INSTITUTE',
     +          /,38X,'CHANG        CHUN'/,
     +          26X,A8,6X,A9,/)
777     FORMAT(/,30X,'DATA OF PIPE',/)
9990    FORMAT(' +',2I5,10F10.3)
9991    FORMAT(' +',2I5,i5,9F10.3)
9992    FORMAT(' +',2I5,2I5,9F10.3)
9993    format(' +',2i5,2x,a20)
9981    FORMAT(' +',2I5,I5,6F10.1)
9982    FORMAT(' +',2I5,2I5,6F10.1)
9999    FORMAT(' +',2I5,10I6)
9998    format(' +',2i5,2i6,i5)
8881    format(' +',10f10.3)
7770    FORMAT(' +',2I5,'(',i5,i6,')',
     +           2I2,26X,F10.3,f7.3,I4,3i3,3f7.3)
7772    FORMAT(' +',2I5,9X,2I2,5X,2F8.3,I3,3i3,3f8.3)
7771    FORMAT(' +',2I5,'(',i5,i6,')',2I2,9F10.3)
6661    FORMAT(' +',2I5,2X,A17)
6662    FORMAT(I2,1X,A17)
7777    FORMAT(1X,A9,/,A9)
7775    FORMAT(16X,'MAXMUM NUMBER OF SPRING:',I2,
     +          5X,' WEIGHT OF WATER:',
     2          5X,I2,' BEND TREAD:',I2,6X,' SPRING VERSION:',I2)
7780    FORMAT(I5,4X,I4,3(3X,F9.3))
7783    FORMAT(I5,4X,I4,4X,'DW=',F9.3,4X,'S=',F9.3,5X,'GW=',F10.3)

6767    FORMAT(16X,I10,3(3X,F10.3))
7676    FORMAT(16X,10X,3(3X,F10.3))     
9875    FORMAT(I5,4X,I4,5X,F9.3,3X,F9.3,3X,F9.3)
1919    FORMAT(I5,4X,I4,2(4X,I4),6X,I4)
99911   FORMAT(16X,F9.3)
8891    FORMAT(I5,4X,I4,5X,F9.3,2(2X,F9.3))
8888    FORMAT(I5,4X,I4,5X,F9.3,2(2X,F9.3),1X,F10.2)
8892    FORMAT(I5,4X,I4,5X,F9.3,2X,F9.3,5X,I4)
8883    FORMAT(28X,3F9.3)
8894    FORMAT(I5,4X,I4,19X,2(3X,F9.3),3X,I4)
9985    FORMAT(I5,4X,I4,3X,I4,4X,I4,6(F9.3))
9977    FORMAT(I5,4X,I4,4X,I4,6(2X,F9.3))
9994    FORMAT(10X,'THIS PIPING STRUCTURE CONTAINS',
     1          I3,'  COLD SPRINGS,',/,
     2          I3,' ANCHORS,',/,9X,I3,' THREEWAYS' ,
     3' AND ',I3,' PIPE ELEMENTS')
9996    FORMAT(10X,'THERE ARE ',I3,' RIGID ELEMENTS AND',
     +I3,' BEND ELEMENTS IN THIS ELEMENTS')
9995    FORMAT(//,20X,'ERR OF COODINATE')
9555    FORMAT(//,11X,'ERR OF COODINATE')
9997    FORMAT(I5,15X,2(3X,F9.3))
5741    FORMAT(16X,F9.3,2X,F9.3,2X,F9.3,2X,F9.3,2X,F9.3,2X,F9.3)
333     FORMAT(20X,'WARNING','    NW=',I6)
311     FORMAT(17X,I5,37X,I5)
444     FORMAT(12X,'ERRORS ','    NE=',I6)
5550    FORMAT(I5,4X,I2,5X,5(4X,F7.3),3X,I2)
      NFZ1=NFZ
      
      RETURN

      END
