program ex1030

 real:: x(30),y(30),xsec,Z,sec, a, b, X_mean, Y_mean, XX_sum, XY_sum,t,sdv,R,M,P,O,sdvv,yy(30),errsum,err,err2
 integer:: min, i, n
 x=0
 y=0
 open(1,file='ppfile.txt',status='old')
 read(1,*)
 do i=1,30
 read(1,'(5x,f6.1,9x,i3,f6.2)',end=99)x(i),min,sec
 y(i)=min*60.0+sec-2409.23
 end do
99 close(1)

!n=i-1
!X_mean=sum(x)/n
!Y_mean=sum(y)/n
!XX_sum=0.0
!XY_sum=0.0
!do i = 1, n
!XX_sum = XX_sum + x(i)**2
!XY_sum = XY_sum + x(i)*y(i)
!end do
!t = (30*XX_sum)-(sum(x)**2)
!a = (XX_sum*sum(y)-sum(x)*XY_sum)/t
!b = (30*XY_sum-sum(x)*sum(y))/t
!m=0
!p=0
!O=0
!sdv=0
!yy=0
!do i=1,n
!M=M+(x(i)-X_mean)*(y(i)-Y_mean)
!p=p+(x(i)-X_mean)**2
!O=O+(y(i)-Y_mean)**2
!sdv=sdv+(y(i)-(a*x(i)+b))**2
!end do 
!R=M/(sqrt(p)*sqrt(o))
!sdvv=(sdv/29.0)**0.5

!do i=1,30
!yy(i) = (b*x(i))+a
!enddo 

 istat=PGOPEN('hw2.ps/vcps')  !PostScript
 if(istat<=0)stop 'ERR opening for PS file!'  
  
 call pgslw(4)
 call pgenv(0.0,120.0,3.0,25.0,0,1) !just,axis
 call pglab('Xi','Yi','sdv')
 call pgslw(2)

 do i=1,30
 call XYFIT(x,y,30,cept,slop,rms,r_cor,sdv)
 call pgsci(30)
 call pgline(30,x,(slop*x)+cept)
 call pgsci(2)
 call pgpt(30,x(i),y(i),4)
 enddo
 print*, cept, slop, sdv

 call pgend
 end


! fitting line using X,Y
subroutine xyfit(x,y,ndata,cept,slop,rms,r_cor,sdv)
real*4 x(ndata),y(ndata)

    xsum  =0.0
    ysum  =0.0
    xxsum =0.0
    yysum =0.0
    xysum =0.0
    sum   =0.0
    errsum=0.0

    do i=1,ndata
      xp  =x(i)
      yp  =y(i)
      sum =sum+1.0
      xsum=xsum+xp
      ysum=ysum+yp
      xxsum=xxsum+xp*xp
      yysum=yysum+yp*yp
      xysum=xysum+xp*yp
    enddo

    r_cor=(sum*xysum-xsum*ysum)/sqrt( (sum*xxsum-xsum*xsum)*(sum*yysum-ysum*ysum) )

    xavg=xsum/sum
    yavg=ysum/sum
    su2 =xxsum-xsum*xavg
    sv2 =yysum-ysum*yavg
    suv =xysum-xsum*yavg
    sdx =sqrt( su2/(sum-1.0) )
    sdy =sqrt( sv2/(sum-1.0) )
    r2  =suv/sqrt(su2*sv2)
    suv2=sv2-su2
    b2  =( suv2+sqrt( suv2*suv2+4.0*suv*suv ) )/(2.0*suv)
    b1  =yavg-b2*xavg
    sdb2=b2*sqrt( (1.-r2*r2)/sum )/r2
    part1=(sdy-sdx*b2)**2.0
    part2=2.*sdx*sdy+(b2*(1.+r2)*xavg**2.0)/r2**2.
    sdb1=sqrt( ( part1+(1.-r2)*b2*part2 )/sum )

    cept=b1 !-  a     Y=aX+b
    slop=b2 !-  b

    do i=1,ndata
      err =b2*x(i)+b1-y(i)
      err2=err*err
      errsum=errsum+err2
    enddo

    sdv=sqrt(errsum/float(ndata-1))

    rms=sqrt(abs(errsum))/float(ndata)

RETURN
END SUBROUTINE xyfit


