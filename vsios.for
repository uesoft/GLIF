C     Ver 11.2000.
$debug
$Title:'Load Calculation for Variable Disk Spring Supports'
      program diskvsio
      real load_deviation,episilen
      logical bool,bool1,bool2
      common /k/bool
      bool=.true.
      episilen=0.25
  15  write(*,*)'请输入工作载荷 P2、热位移Trace'
      read(*,*)p2,ttrace
      if (ttrace.le.1e-3) then
        itrace=1
        ttrace=abs(ttrace)
      else
        itrace=0
      end if
      inConstant=1
      p1=0.0
      call variable(p2,itrace,ttrace,episilen,bool1,bool2,p1,load_deviat
     $ion,ik,jj,inConstant)
      if (bool) then
        write(*,*)'是否存盘（0 - 存盘; 1 - 不存盘）'
        read(*,*)iprint
      end if
      if (iprint.eq.0) call print(inConstant,itrace,bool1,bool2,p1,p2,tt
     $race,load_deviation,ik,jj)
      if (iprint.eq.1) call display(inConstant,itrace,bool1,bool2,p1,p2,
     $ttrace,load_deviation,ik,jj)
      write(*,*)'是否还要计算? （0 - 计算 ; 1 - 结束）'
      read(*,*)iContinue
      if (iContinue.eq.0) goto 15
      stop
      end
c      
      subroutine display(iconstant,itrace,bool1,bool2,p1,p2,trace,load_d
     $eviation,ik,jj)
      real load_deviation
      logical bool1,bool2     
      if (itrace.eq.0) then
	write(*,*)'热位移方向向上'
      else
	write(*,*)'热位移方向向下'
      endif
      if (iconstant.eq.0) then
	  write(*,25)
	  write(*,28)p2,trace
      else
 	  if (bool2) then
	    write(*,65)p2,trace
	    write(*,55)
	    return
	  endif
        if (bool1) then
	    write(*,50)
        else
	    write(*,30)p1,p2
 	    write(*,35)trace
          write(*,40)100.*load_deviation
	    write(*,45)ik-1,jj
	  endif
      endif
  25  format(1x,'选用恒力碟簧支吊架')      
  28  format(1x,'工作载荷 p2=',f12.2,1x,'热位移 Trace=',f7.2)
  30  format(1x,'安装载荷 p1=',f12.2,1x,'工作载荷 p2=',f12.2)
  35  format(1x,'热位移 Trace=',f7.2)
  40  format(1x,'载荷偏差度 load_deviation=',f8.3,'%')
  45  format(1x,'选用的可变碟簧支吊架的型号为:VDS',i3,'-',i3/)
  50  format(1x,'载荷超出范围')
  55  format(1x,'建议选用可变弹簧支吊架')
  65  format(1x,'工作载荷 p2=',f12.2,'〈碟簧支吊架的最小载荷 1500 N',2x,
     $'位移Trace=',f7.2)
      return
      end
c
      subroutine print(iconstant,itrace,bool1,bool2,p1,p2,trace,load_dev
     $iation,ik,jj)
      real load_deviation
      logical bool1,bool2,bool
      CHARACTER*12 FNAME
      common /k/bool
      if (bool) then
        WRITE(*,'(A)')'  请输入文件名(*.dat):'
        READ(*,'(A)')FNAME
        OPEN(6,FILE=FNAME)
        bool=.false.
      else
        open (6,file=FNAME,access='append',status='old')
      endif
c      open (6,file='prn')
      if (itrace.eq.0) then
	write(6,*)'热位移方向向上'
      else
	write(6,*)'热位移方向向下'
      endif
      if (iconstant.eq.0) then
	  write(6,25)
	  write(6,28)p2,trace
      else
 	  if (bool2) then
	    write(6,65)p2,trace
	    write(6,55)
	    return
	  endif
        if (bool1) then
	    write(6,50)
        else
	    write(6,30)p1,p2
 	    write(6,35)trace
          write(6,40)100.*load_deviation
	    write(6,45)ik-1,jj
	  endif
      endif
      close(6)
  25  format(1x,'选用恒力碟簧支吊架')
  28  format(1x,'工作载荷 p2=',f12.2,1x,'热位移 Trace=',f7.2)
  30  format(1x,'安装载荷 p1=',f12.2,1x,'工作载荷 p2=',f12.2)
  35  format(1x,'热位移 Trace=',f7.2)
  40  format(1x,'载荷偏差度 load_deviation=',f8.3,'%')
  45  format(1x,'选用的可变碟簧支吊架的型号为:VDS',i3,'-',i3/)
  50  format(1x,'载荷超出范围')
  55  format(1x,'建议选用可变弹簧支吊架')
  65  format(1x,'工作载荷 p2=',f12.2,'〈碟簧支吊架的最小载荷 1500 N',2x,
     $'位移Trace=',f7.2)
      return
      end
c
      subroutine variable(p2,itrace,trace,eps,bool1,bool2,p1,load_deviat
     $ion,ik,jj,inConstant)
      real load_deviation
      logical bool1,bool2
      dimension htrace(5),httrace(5),hhtrace(6,100),fi0(25),fi2(25)
      dimension pk(200)
      common /a7/htrace,httrace,hhtrace
      common /a6/n
      data fi0/.2959,.3259,.3061,.3124,.314,.3267,.3237,.3158,.329,.3345
     $,.3331,.3398,.3237,.3071,.3001,.3101,.3124,.30825,.3103,.30155,.29
     $934,.30355,.3028,.3031,.30695/
      data fi2/.629,.7209,.6593,.6788,.6835,.7235,.7141,.6893,.7311,.748
     $7,.7441,.7662,.7141,.7273,.6971,.7407,.7516,.7324,.74172,.70308,.6
     $9382,.7117,.70842,.70972,.72666/
      n=45
	bool1=.false.
	bool2=.false.
      call load_judge(p2,itrace,ik,bool1,bool2)
	if (bool1) return
	if (bool2) return
      call find_loadp(fi0,fi2,ik,pk)
      call find_pd(p2,pk,nk) 
      call hot_trace
      jj=0
  20  jj=jj+1
 	p1=0.0
	if (jj.lt.5) then
        if (itrace.eq.0) then
c         if ((hhtrace(jj,nk)+trace).gt.hhtrace(jj,n+1)) goto 20
          nh=nk+int(trace/htrace(jj)+0.5)
          if (nh.gt.n+1) goto 20
        else
c         if ((hhtrace(jj,nk)-trace).le.1e-3) goto 20
          nh=nk-int(trace/htrace(jj)+0.5)
          if (nh.le.0) goto 20
        endif
      end if
	if (nh.le.0) then
	  inConstant=0
	else
	  p1=pk(nh)
	endif
      call insert(p2,itrace,pk,nk,jj,trace,p1)
      load_deviation=abs((p2-p1)/p2)
      if ((load_deviation-eps).gt.1e-03) then
        if (jj.lt.5) then
	    goto 20
	  else
          inConstant=0
	  endif
      endif
      return
      end
c
      subroutine load_judge(p2,itrace,ik,bool1,bool2)
      dimension p(25),p1(25)
      logical bool1,bool2
      data p/2500.,3150.,4000.,5000.,6300.,8000.,10000.,12500.,16000.,
     $20000.,25000.,31500.,40000.,50000.,63000.,80000.,100000.,125000.,
     $160000.,200000.,250000.,315000.,400000.,500000.,630000./
      do 1 i=1,25
        p1(i)=0.6*p(i)
   1  continue
      if (p2.gt.p(25)) then
	bool1=.true.
	return	
      endif
      if (p2.lt.0.6*p(1)) then
	bool2=.true.
	return
      endif
      if (itrace.eq.0) then
        ii=25
   2    if (p2.ge.p1(ii)) goto 4
        ii=ii-1
        goto 2
   4    ik=ii
      end if
      if (itrace.eq.1) then
        ii=1
   5    if (p2.le.p(ii)) goto 8
        ii=ii+1
        goto 5
   8    ik=ii
      end if
      return
      end
c
      subroutine hot_trace
      dimension htrace(5),httrace(5),hhtrace(6,100)
      common /a7/htrace,httrace,hhtrace/a6/n
      data httrace/22.5,45.,90.,135.,180./
      do 20 i=1,5
        htrace(i)=httrace(i)/n
        do 20 j=1,n+1
          hhtrace(i,j)=httrace(i)/n*(j-1)
   20 continue
      return
      end
c
      subroutine find_loadp(fi1,fi2,ii,ph)
      dimension fi1(25),fi2(25),fi(25,181),pp(25,181),t(25)
      dimension d1(25),d2(25),ph(200),h(25)
      common /a1/fk/a2/ck4,hk
      common /a5/d1,d2,t,h/a6/n
      data  d1/40.,45.,50.,56.,63.,71.,80.,90.,100.,112.,125.,140.,160.,
     $180.,200.,225.,250.,280.,315.,355.,400.,450.,500.,560.,630./
      data  d2/20.4,22.4,25.4,28.5,31.,36.,41.,46.,51.,57.,64.,72.,82.,9
     $2.,102.,112.,127.,142.,162.,182.,202.,224.,254.,285.,310./
      data  t/1.4,1.55,1.75,1.95,2.2,2.45,2.75,3.1,3.45,3.85,4.3,4.8,5.5
     $,6.15,6.9,7.75,8.6,9.65,10.85,12.25,13.8,15.5,17.30,19.35,21.8/
      data  h/1.4,1.55,1.75,1.95,2.2,2.45,2.75,3.1,3.45,3.85,4.3,4.8,5.5
     $,6.15,6.9,7.75,8.6,9.65,10.85,12.25,13.8,15.5,17.30,19.35,21.8/
      do 10 i=1,25
	fk=h(i)/t(i)
	call coe2(t(i),h(i),ck1)
	h0=t(i)+h(i)
	call coe1(ck1,h0,t(i),ck4)
	t1=ck1*t(i)               
	h1=h0-t1 
	c=d1(i)/d2(i) 
        do 20 j=1,n+1
          fi(i,j)=0.0
          fi(i,j)=fi1(i)+(fi2(i)-fi1(i))/n*(j-1) 
          ph(j)=0.0
	  call rload(fi(i,j),h(i),d1(i),c,t1,h1,ck4,pp(i,j))
          ph(j)=pp(ii,j)
  20    continue
  10  continue
      return
      end
c
      subroutine find_pd(p,ph,nk)
      dimension ph(200)
      common /a6/n
      i=1
  1   if (i.gt.n+1) then
	write(*,5)
      else
        if (ph(i).le.1e-03) goto 8
        if ((p-ph(i))/ph(i).le.1e-03) goto 8
        i=i+1
        goto 1  
      end if
   5  format(10x,'The load is beyond the Range!'/)
   8  nk=i
      return
      end
c                      
      subroutine insert(y2,itrace,ph,nk,jj,trace,y1)
      dimension ph(200) 
      dimension htrace(5),httrace(5),hhtrace(6,100)
      common /a6/n/a7/htrace,httrace,hhtrace
      if (nk.eq.1) then
        x0=1.0+(y2-ph(nk))/(ph(nk+1)-ph(nk))
      else
        x0=(y2-ph(nk-1))/(ph(nk)-ph(nk-1))
      end if
      x1=trace/htrace(jj)-int(trace/htrace(jj)+0.5)
      if (itrace.eq.0) then
        if ((nk+int(trace/htrace(jj)+0.5)).gt.n) return
        kk=nk+int(trace/htrace(jj)+0.5)
      else
        if ((nk-int(trace/htrace(jj)+0.5)).le.0) return
        kk=nk-int(trace/htrace(jj)+0.5)
      endif
      if (kk.eq.1) then
        y1=ph(1)
      else
        y1=ph(kk-1)+(x0+x1)*(ph(kk)-ph(kk-1))
      end if
      return
      end
c
      subroutine coe(mu,e,c,alpha)
      real mu
      c1=alog(c)
      e1=4.*e/(1-mu*mu)
      alpha=3.1415927*((c+1.)/(c-1.)-2./c1)/(1.-1./c)**2.*e1
      return
      end
c
      subroutine rload(fi,h,d,c,t1,h1,ck4,p)
      call coe(.3,206000.,c,alpha)
      fk=h1/t1
      ff=t1*t1*t1*h1*alpha/(d*d)
      p=ck4*ck4*fi*h/h1*(ck4*ck4*(1.-fi*h/h1)*(1.-.5*fi*h/h1)*fk*fk+1.)*
     $ff
      return
      end
c
      subroutine coe1(ck1,h0,t,ck4)
      c1=ck1*ck1/((0.25*h0/t-ck1+.75)*(0.625*h0/t-ck1+0.375))
      c2=c1/(ck1*ck1*ck1)*(.15625*(h0/t-1.)*(h0/t-1.)+1.)
      ck4=sqrt(sqrt((c1/2.)*(c1/2.)+c2)-c1/2.)
      return
      end      
c
      subroutine coe2(t,h,ck1)
      integer group
      if (t.le.1.25) then
	group=1
      else 
	if ((t.gt.1.25).and.(t.le.6.0)) then
	  group=2
	else
	  if (t.gt.6.0) then
	    group=3
	  endif
	endif
      endif
      fk=h/t
      h0=t+h
      if (group.eq.3) then
	if (fk.le.1.) then
	  ck1=.94
	else
	  ck1=.95
	endif
      else
	ck1=1.
      endif                            
      return
      end
