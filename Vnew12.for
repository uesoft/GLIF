c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    作者:       李  智                                   *
c*    程序名:                                              *
c*                VNEW11                                   *
c*    功能:                                                *
c*                主程序                                   *
c*    被调用程序:                                          *
c*                                                         *
c*    调用程序:                                            *
c*                DATAIN                                   *
c*                SANS                                     *
c*                TSTRESS                                  *
c*                CPRINT                                   *
c*                GETTIM(SYS)                              *
c*                GETDAT(SYS)                              *
c*    读取文件:                                            *
c*            通道号      内容                             *
c*            19          代替ENCODE函数作文本处理         *
c*    写入文件:                                            *
c*            通道号      内容                             *
c*            19          代替ENCODE函数作文本处理         *
c*            1           输出分析报告                     *
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
      PROGRAM VNEW11
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      CHARACTER DAT1*10,IA*1

      CHARACTER LTIM1*9,ABC*15,AA1*12,AA2*12

      COMMON/KZKG/ISCXZ,ISCDW,IQS,NTH
      COMMON/CAD/ICAD
      COMMON/TIM/LTIM1,DAT1,ABC,AA1,AA2
      COMMON/HDGD/NHD,IHD(50),HD(50)
C       COMMON/SCDS/NFZ
C        COMMON/SCDS/NFZ,NDD,JD(50),DD(50,6),D8(50,6),D6(50,6)
      common/prin/iprn
      common/scnd/isc,iscnd(10),iscgk(10),iscis(10,10),iscim(10,10),
     +f1(10,10,6),f2(10,10,6)
      common/gk/igk,ifj5,ifj6,ifj7,ifj8,ifj9,ifj10,ifj11,ifj12
      DIMENSION ISH(1000),IMH(1000)
      DIMENSION LX(1000),ITLX(1000)

c      INCLUDE   'FLIB.FI'
c      INCLUDE   'FLIB.FD'

      INTEGER*2 tmpday, tmpmonth, tmpyear
      INTEGER*2 tmphour, tmpminute, tmpsecond, tmphund



c       open(unit=9,file='src',status='old',ERR=987)
c       read(9,654,err=987)abc
c       read(9,654,err=987)abc
c       read(9,654,err=987)abc
c       read(9,654,err=987)AA1
c       read(9,654,err=987)AA1
c654     format(a5)
c       CLOSE(UNIT=9,STATUS='DELETE')


c++++++++++++++++++++++++++++++++++++++++++
c++++++++++++++++++++++++++++++++++++++++++
c       open(unit=9,file='c:\dos\country.sys',status='old',ERR=987)
c       read(9,654,err=987)abc
c654     format(a15)
c       CLOSE(UNIT=9)
c        if(abc(3:3).ne.'A')goto 987
c++++++++++++++++++++++++++++++++++++++++++
c++++++++++++++++++++++++++++++++++++++++++


c      open(unit=9,file='c:\dos\doshelp.hlp',status='old',ERR=987)
c      read(9,654,err=987)abc
c      read(9,654,err=987)abc
c      read(9,654,err=987)abc
c      read(9,654,err=987)abc
c      read(9,654,err=987)abc
c      read(9,654,err=987)abc
c      read(9,654,err=987)abc
c      read(9,654,err=987)abc
c      CLOSE(UNIT=9)
c     
c      if(abc(2:2).ne.' ')goto 987
c      if(abc(3:3).ne.'N')goto 987
c      if(abc(4:4).ne.'e')goto 987
c      if(abc(5:5).ne.'w')goto 987
c      if(abc(6:6).ne.' ')goto 987
c      if(abc(7:7).ne.' ')goto 987


      tmphund = 0
C
C     Show current date and time.
C
      call GETDAT(tmpyear, tmpmonth, tmpday)
      CALL GETTIM(tmphour, tmpminute, tmpsecond, tmphund)
      tmpyear=tmpyear-1900
981        format(i2,'/',i2,'/',i2)
980        format(a9)
        close(19,status='delete')
        open(unit=19,file='encode',status='new')
        write(19,981)tmpyear,tmpmonth,tmpday
        backspace 19
        read(19,980)dat1
        close(19,status='delete')

           close(19,status='delete')
           open(unit=19,file='encode',status='new')
           write(19,981)tmphour,tmpminute,tmpsecond
           backspace 19
           read(19,980)ltim1
           close(19,status='delete')


c       if(abc(1:1).ne.'e')goto 987
c        if(abc.ne.'eicnm')goto 987
C       WRITE(*,99)
C99      FORMAT(' PLEASE TYPE YOUR SERIES NUMBER:___'$)
C       READ(*,333)ABC
C       IF(ABC(1:1).NE.'S'.AND.ABC(1:1).NE.'s')GOTO 987
C       IF(ABC(2:2).NE.'D'.AND.ABC(2:2).NE.'d')GOTO 987
C       IF(ABC(3:7).NE.'EPDI'.AND.ABC(3:7).NE.'epdi')GOTO 987
c       if(abc(4:4).ne.'n')goto 987

      OPEN(UNIT=4,FILE='FORT04',STATUS='UNKNOWN')
c       if(abc(5:5).ne.'m')goto 987
      OPEN(UNIT=7,FILE='FORT07',STATUS='UNKNOWN')
c       if(abc(2:2).ne.'i')goto 987
      OPEN(UNIT=8,FILE='FORT08',STATUS='UNKNOWN')
c       if(abc(3:3).ne.'c')goto 987
      OPEN(UNIT=9,FILE='FORT09',STATUS='UNKNOWN')
      OPEN(UNIT=12,FILE='FORT12',STATUS='UNKNOWN')
      OPEN(UNIT=13,FILE='FORT13',STATUS='UNKNOWN')
      OPEN(UNIT=14,FILE='FORT14',STATUS='UNKNOWN')
      OPEN(UNIT=15,FILE='FORT15',STATUS='UNKNOWN')
      OPEN(UNIT=20,FILE='FORT20',STATUS='UNKNOWN')
      OPEN(UNIT=21,FILE='FORT21',STATUS='UNKNOWN')
      OPEN(UNIT=23,FILE='FORT23',STATUS='UNKNOWN')
      open(unit=31,file='fort31',status='UNKNOWN')
      open(unit=32,file='fort32',status='UNKNOWN')
      open(unit=33,file='fort33',status='UNKNOWN')
      OPEN(UNIT=25,FILE='FORT25',STATUS='UNKNOWN')
      OPEN(UNIT=24,FILE='FORT24',STATUS='UNKNOWN')
      OPEN(UNIT=18,FILE='FORT18',STATUS='UNKNOWN')
      CLOSE(UNIT=20,STATUS='DELETE')
c       close(unit=24)
      close(unit=24,STATUS='DELETE')
      CLOSE(UNIT=18,STATUS='DELETE')
      CLOSE(UNIT=15,STATUS='DELETE')
      CLOSE(UNIT=14,STATUS='DELETE')
      CLOSE(UNIT=13,STATUS='DELETE')
      CLOSE(UNIT=12,STATUS='DELETE')
      CLOSE(UNIT=9,STATUS='DELETE')
      CLOSE(UNIT=8,STATUS='DELETE')
      CLOSE(UNIT=7,STATUS='DELETE')
      CLOSE(UNIT=4,STATUS='DELETE')
      CLOSE(UNIT=31,STATUS='DELETE')
      CLOSE(UNIT=32,STATUS='DELETE')
      CLOSE(UNIT=33,STATUS='DELETE')
      CLOSE(UNIT=25,STATUS='DELETE')
      close(unit=21,STATUS='DELETE')
      close(unit=23,status='delete')


c        open(1,file='cright.db',form='unformatted',status='old',
c     +  err=9990)
c        read(1)i1
c        read(1)i2
c        if(i2.gt.5555)then
c          WRITE(*,*)'             Dont use any report in engineering'
c          write(*,*)'               Maybe this program have errors'
c          WRITE(*,*)'                CALL 0431-5642361-8240 LIZHI'
c          stop
c        end if
c        i2=i2+1
c        backspace (1)
c        write(1)i2

      WRITE(*,100)
100     FORMAT(' PLEASE TYPE INPUT FILE NAME:___'$)
      READ(*,333)AA1
      WRITE(*,110)
110     FORMAT(' PLEASE TYPE OUTPUT FILE NAME:___'$)
      READ(*,333)AA2
      OPEN(UNIT=1,FILE=aa2,STATUS='UNKNOWN')
      CLOSE(UNIT=1,STATUS='DELETE')
      OPEN(UNIT=1,FILE=AA2,STATUS='NEW')
      OPEN(UNIT=4,FILE='FORT04',FORM='UNFORMATTED',STATUS='NEW')
      OPEN(UNIT=7,FILE='FORT07',FORM='UNFORMATTED',STATUS='NEW')
      OPEN(UNIT=8,FILE='FORT08',FORM='UNFORMATTED',STATUS='NEW')
      OPEN(UNIT=9,FILE='FORT09',FORM='UNFORMATTED',STATUS='NEW')
      OPEN(UNIT=11,FILE=AA1,STATUS='OLD')
      OPEN(UNIT=12,FILE='FORT12',FORM='UNFORMATTED',STATUS='NEW')
      OPEN(UNIT=13,FILE='FORT13',FORM='UNFORMATTED',STATUS='NEW')
      OPEN(UNIT=14,FILE='FORT14',FORM='UNFORMATTED',STATUS='NEW')
      OPEN(UNIT=15,FILE='FORT15',FORM='UNFORMATTED',STATUS='NEW')
      OPEN(UNIT=20,FILE='FORT20',STATUS='NEW')
      OPEN(UNIT=21,FILE='FORT21',FORM='UNFORMATTED',STATUS='NEW')
c        open(unit=31,file='fort31',form='unformatted',status='new')
c        open(unit=32,file='fort32',form='unformatted',status='new')
      open(unit=33,file='fort33',form='UNFORMATTED',status='NEW')
      OPEN(UNIT=25,FILE='FORT25',FORM='UNFORMATTED',STATUS='NEW')
      OPEN(UNIT=24,FILE='FORT24',STATUS='NEW')
      NCW=0
      NJG=0
C       CALL DATE(DAT1)
C       CALL TIME(LTIM1)
      WRITE(*,*)'                               GOOD LUCK TO YOU'

      WRITE(*,1000)
      CALL DATAIN(ISH,IMH,LX,ITLX,NCW,NJG,NFZ)
      CLOSE(UNIT=11)
      close(unit=18)
      OPEN(UNIT=18,FILE='FORT18',STATUS='NEW')

      IF(NCW.GT.0)WRITE(*,*)'          STOP               TO CHECK
     1ORIGINAL DATA '
      IF(NCW.GT.0)GOTO 10
      IF(NJG.GT.0)WRITE(*,101)
101
     +FORMAT('                              CONTINUE-------------Y/N'$)
      IF(NJG.GT.0)READ(*,222)IA
      IF(IA.EQ.'N'.OR.IA.EQ.'n')GOTO 10

      IF(NJG.EQ.0)WRITE(*,*)'                              ',
     +  'NO ERRORS IN DATA FILE'
      IF(NJG.GE.0)
     +WRITE(*,*)'                               CONTINUE CALCULATION'
      CALL SANS(NFZ,ISH,IMH,LX,ITLX)

10      continue
      iscgk(1)=1
      iscgk(2)=2
      iscgk(3)=3
      iscgk(4)=4
      iscgk(5)=ifj5
      iscgk(6)=ifj6
      iscgk(7)=ifj7
      iscgk(8)=ifj8
      iscgk(9)=ifj9
      iscgk(10)=ifj10

      if(isc.ne.0)then
          do 555 i=1,10
          if(iscnd(i).ne.0)then
            call cprint(1)
            write(1,5551)iscnd(i)
            write(1,5554)
          end if
          do 554 j=1,10
          if(iscnd(i).ne.0)then
            if(iscgk(j).ne.0)then
      	if(iscgk(j).ne.1.and.iscgk(j).ne.9)then
      	  if(iscgk(j).eq.3)then
            fxs=-1
      	  else
            fxs=1
      	  end if
      	  f1(i,j,1)=f1(i,j,1)*fxs+f1(i,1,1)
      	  f1(i,j,2)=f1(i,j,2)*fxs+f1(i,1,2)
      	  f1(i,j,3)=f1(i,j,3)*fxs+f1(i,1,3)
      	  f1(i,j,4)=f1(i,j,4)*fxs+f1(i,1,4)
      	  f1(i,j,5)=f1(i,j,5)*fxs+f1(i,1,5)
      	  f1(i,j,6)=f1(i,j,6)*fxs+f1(i,1,6)
      	endif
      	write(1,5552)iscgk(j),
     +          f1(i,j,1),f1(i,j,2),f1(i,j,3),
     +          f1(i,j,4),f1(i,j,5),f1(i,j,6),
     +          iscis(i,j),iscim(i,j)
            end if
          end if
554         continue
          if(iscnd(i).ne.0)then
          write(1,5553)
          end if
          do 553 j=1,10
          if(iscnd(i).ne.0)then
            if(iscgk(j).ne.0)then
      	write(1,5552)iscgk(j),
     +          f2(i,j,1),f2(i,j,2),f2(i,j,3),
     +          f2(i,j,4),f2(i,j,5),f2(i,j,6)
            end if
          end if
553         continue
555         continue
5551    format(//,56x,'Output point number: ',i5)
5552    format(/,i10,6f15.3,2i10,/)
5553    format(/,6x,'Case',10x,'Dx   ',10x,'Dy   ',10x,'Dz   ',
     +                     10x,'Sx   ',10x,'Sy   ',10x,'Sz   ')
5554    format(/,6x,'Case',10x,'Fx   ',10x,'Fy   ',10x,'Fz   ',
     +                     10x,'Mx   ',10x,'My   ',10x,'Mz   ',
     +                     5X,'Start',7x,'End')
      end if

C        CALL TIME(LTIM1)
c        LTIM1='----'

           close(19,status='delete')
           open(unit=19,file='encode',status='new')
           write(19,981)tmphour,tmpminute,tmpsecond
           backspace 19
           read(19,980)ltim1
           close(19,status='delete')

      ij=1000
      call tstress(ij)

      CLOSE(UNIT=20,STATUS='DELETE')
      close(unit=24)
      close(unit=24,STATUS='DELETE')
      CLOSE(UNIT=18,STATUS='DELETE')
      CLOSE(UNIT=15,STATUS='DELETE')
      CLOSE(UNIT=14,STATUS='DELETE')
      CLOSE(UNIT=13,STATUS='DELETE')
      CLOSE(UNIT=12,STATUS='DELETE')
      CLOSE(UNIT=9,STATUS='DELETE')
      CLOSE(UNIT=8,STATUS='DELETE')
      CLOSE(UNIT=7,STATUS='DELETE')
      CLOSE(UNIT=4,STATUS='DELETE')
      CLOSE(UNIT=31,STATUS='DELETE')
      CLOSE(UNIT=32,STATUS='DELETE')
      CLOSE(UNIT=33,STATUS='DELETE')
      CLOSE(UNIT=25,STATUS='DELETE')
      close(unit=11)
      close(unit=21,STATUS='DELETE')
      close(unit=23)
      CLOSE(UNIT=1)
      CLOSE(UNIT=24)
      if(iprn.eq.1)CLOSE(UNIT=23)

1000    FORMAT(////////,30X,'PIPING STRESS ANALYSIS'
     1         ,////////)
2000    FORMAT(1H1)
222     FORMAT(1A)
333     FORMAT(12A)
      write(*,*)
     +'                           -----End of the program-----'
      goto 9991
9990    continue
      write(*,*)'                Sorry you have not copyright'
9991    continue

c        STOP
      goto 986
987     continue
      write(*,*)'Sorry you are not legal user   !!!'
      write(*,*)'Do not belive the result anymore!!!'
      write(*,*)'Call (0431)5642361-8203 Lizhi'
986     continue        
      END


c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c*    作者:       李  智                                   *
c*    程序名:                                              *
c*                TSTRESS                                  *
c*    功能:                                                *
c*                对计算出的应力进行整理输出               *
c*    被调用程序:                                          *
c*                VNEW11                                   *
c*    调用程序:                                            *
c*                CPRINT                                   *
c*    读取文件:                                            *
c*            通道号      内容                             *
c*            18          单元应力                         *
c*    写入文件:                                            *
c*            通道号      内容                             *
c*            23          数据库文件(61,62)                *
c*            1           输出分析报告                     *
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
      subroutine tstress(ij)
      common/prin/iprn
      dimension ICD1(4000),IGK1(4000),
     +             IS1(4000),IM1(4000),mij1(4000)
      dimension FJQ1(4000),YLXS1(4000)
      dimension xmm1(4000),xmat1(4000),XMAD1(4000),XMT1(4000)
      dimension tj1(4000),t01(4000),pj1(4000),dw1(4000),s1(4000)
      dimension igz(800),IGK(800),IS(800),IM(800),mij(800),
     +FJQ(800),YLXS(800),
     +xmm(800),xmat(800),xmad(800),xmt(800),ok(800)
      COMMON/KZKG/ISCXZ,ISCDW,IQS,NTH
      character adw*12,ok*6

      if(iscxz.eq.1)then
         adw='[kgf/mm**2]'
      else
         adw='[MPa]'
      end if
      ij=0
      call cprint(1)
      write(1,5559)
      write(1,5557)adw
      WRITE(1,5558)
5559    format(//,29x,'TOTAL MAX STRESS TABLE',/)
5558    FORMAT('   No ',1x,'CASE',4X,'START',
     +7X,'END',3X,'IJ',5X,'i',
     +5X,'f/k',7x,'STRESS-C',5X,'STRESS-A',/)
5557    format(70x,a12,/)
5551    continue
c         open(unit=18,file='fort18',status='old')
       rewind(18)
       nfz=0
       do 1000 i=1,10000
       nfz=nfz+1
       read(18,5550,end=1001)ICD1(i),IGK1(i),IS1(i),IM1(i),
     +mij1(i),FJQ1(i),YLXS1(i),
     +xmm1(i),xmat1(i),XMAD1(i),XMT1(i),
     +tj1(i),t01(i),pj1(i),dw1(i),s1(i)
1000     continue
1001     continue
       if(nfz.gt.4000)then
            write(*,*)'Sorry'
            write(*,*)'The cases or elements is too many'
            write(*,*)'Release some cases or elements'
       end if
       k=0
       ijk=0
       do 2001 i=1,nfz
         nc1=icd1(i)
       if(icd1(i).ne.0)then
         if(i.ge.2)then
            if(igk1(i).ne.igk1(i-1))k=0
         end if
         if(nc1.eq.65)then
            kk=555
         else
            k=k+1
         end if

         ijk=ijk+1  
       do 2000 j=1,nfz
       if(icd1(j).ne.0)then
             nc2=icd1(j)
          if(dw1(i).eq.dw1(j).and.s1(i).eq.s1(j).and.
     +         tj1(i).eq.tj1(j).and.t01(i).eq.t01(j).and.
     +         pj1(i).eq.pj1(j).and.xmt1(i).eq.xmt1(j).and.
     +        igk1(i).eq.igk1(j))then
             icd1(j)=0
              

             if((xmad1(j).gt.xmad(ijk)).and.
     +            (xmad1(j).lt.xmt1(j)))then
      	  if(nc2.eq.65)then
      	    igz(ijk)=kk
      	  else
      	    igz(ijk)=k
      	  end if
      	  IGK(ijk)=igk1(j)
      	  IS(ijk)=is1(j)
      	  IM(ijk)=im1(j)
      	  mij(ijk)=mij1(j)
      	  FJQ(ijk)=fjq1(j)
      	  YLXS(ijk)=ylxs1(j)
      	  xmm(ijk)=xmm1(j)
      	  xmat(ijk)=xmat1(j)
      	  xmad(ijk)=XMAD1(j)
      	  xmt(ijk)=XMT1(j)
      	  ok(ijk)='-ok-'
            else if(xmad1(j).ge.xmt1(j))then
      	  if(nc2.eq.65)then
      	   igz(ijk+1)=kk
      	  else
      	   igz(ijk+1)=k
      	  end if
      	  IGK(ijk+1)=igk(ijk)
      	  IS(ijk+1)=is(ijk)
      	  IM(ijk+1)=im(ijk)
      	  mij(ijk+1)=mij(ijk)
      	  FJQ(ijk+1)=fjq(ijk)
      	  YLXS(ijk+1)=ylxs(ijk)
      	  xmm(ijk+1)=xmm(ijk)
      	  xmat(ijk+1)=xmat(ijk)
      	  xmad(ijk+1)=XMAD(ijk)
      	  xmt(ijk+1)=XMT(ijk)
      	  ok(ijk+1)=ok(ijk)

      	  if(nc2.eq.65)then
      	   igz(ijk)=kk
      	  else
      	   igz(ijk)=k
      	  end if
      	  IGK(ijk)=igk1(j)
      	  IS(ijk)=is1(j)
      	  IM(ijk)=im1(j)
      	  mij(ijk)=mij1(j)
      	  FJQ(ijk)=fjq1(j)
      	  YLXS(ijk)=ylxs1(j)
      	  xmm(ijk)=xmm1(j)
      	  xmat(ijk)=xmat1(j)
      	  xmad(ijk)=XMAD1(j)
      	  xmt(ijk)=XMT1(j)
      	  ok(ijk)='****'

      	  ijk=ijk+1
             end if
          end if
       end if
2000     continue
         icd1(i)=0

       end if
2001     continue
       if(ijk.gt.800)then
            write(*,*)'**** Sorry    ****'
            write(*,*)'**** The element stress is illegal too many'
            write(*,*)'**** check data file carefully'
            write(1,*)'****    Sorry    ****'
            write(1,*)'**** The element stress is illegal too many'
            write(1,*)'**** Check data file carefully'
       end if
       do 3000 i=1,ijk
       if(igz(i).gt.0.and.igk(i).gt.0)then
      	 write(1,5552)Igz(i),IGK(i),IS(i),IM(i),
     +                        mij(i),FJQ(i),YLXS(i),
     +                        XMAD(i),XMT(i),ok(i)
      	 if(iprn.eq.2)then
      	  ngd1=61
      	  write(23,4385)ngd1,IS(i),IM(i),
     +                         FJQ(i),YLXS(i),
     +         xmm(i),xmat(i),XMAD(i),XMT(i),ok(i),ok(i)
      	  ngd1=62
      	  rmij=mij(i)
      	  write(23,4385)ngd1,Igz(i),IGK(i),
     +           rmij,rmij,rmij,rmij,rmij,rmij,ok(i),ok(i)
4385         format(i5,i7,i7,6f15.3,1x,a13,1x,a13)
      	 else if(iprn.eq.1)then
      	  ngd1=61
      	  write(23,201)ngd1,ngd1,IS(i),IM(i),
     +                         FJQ(i),YLXS(i),
     +         xmm(i),xmat(i),XMAD(i),XMT(i)
      	  ngd1=62
      	  rmij=mij(i)
      	  write(23,201)ngd1,ngd1,Igz(i),IGK(i),
     +           rmij,rmij,rmij,rmij,rmij,rmij
201     FORMAT(2I5,2I10,6F15.3)
        ELSE IF(IPRN.EQ.3)THEN
		   ngd1=61
         write(50,5003)ngd1,IGK(I),IS(i),IM(i),mij(i),
     +          FJQ(i),YLXS(i),xmm(i),xmat(i),XMAD(i)                
     
5003  FORMAT(5I12,5F12.3)     
		   ngd1=62
		   rmij=mij(i)
          write(50,5003)ngd1,Igz(i),Igz(i),Igz(i),Igz(i),
     +           XMT(i),XMT(i),XMT(i),XMT(i),XMT(i)
      	 end if
       end if

3000     continue


c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
5550     FORMAT(2I5,3I10,11F15.3)
      WRITE(1,5555)
      WRITE(1,5556)

5556    FORMAT('
     +')
5555    FORMAT(/,28x,'------END OF THE REPORT------')
5552    format(2i5,2I10,i5,f8.3,f7.3,f13.3,f13.3,2x,a4)
      RETURN
      END

       
