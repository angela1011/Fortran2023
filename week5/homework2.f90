program ex1016

 real:: x(30),y(30),xsec,Z,sec, a, b, X_mean, Y_mean, XX_sum, XY_sum,t,sdv,R,M,P,O,sdvv,yy(30)
 integer:: min, i, n
 x=0
 y=0
 open(1,file='ppfile.txt',status='old')
 read(1,*)
 do i=1,30
 read(1,'(5x,f6.1,9x,i3,f6.2)',end=99)x(i),min,sec
 y(i)=min*60.0+sec-2409.23
 !print*,x, y
 end do
99 close(1)

n=i-1
X_mean=sum(x)/n
Y_mean=sum(y)/n
XX_sum=0.0
XY_sum=0.0
do i = 1, n
XX_sum = XX_sum + x(i)**2
XY_sum = XY_sum + x(i)*y(i)
end do
t = (30*XX_sum)-(sum(x)**2)
a = (XX_sum*sum(y)-sum(x)*XY_sum)/t
b = (30*XY_sum-sum(x)*sum(y))/t
m=0
p=0
O=0
sdv=0
yy=0
do i=1,n
M=M+(x(i)-X_mean)*(y(i)-Y_mean)
p=p+(x(i)-X_mean)**2
O=O+(y(i)-Y_mean)**2
sdv=sdv+(y(i)-(a*x(i)+b))**2
end do 
R=M/(sqrt(p)*sqrt(o))
sdvv=(sdv/29.0)**0.5
print*, a,b, sdvv,R
do i=1,30
yy(i) = (b*x(i))+a
!print*, yy
enddo 

!pgv=0.920*pd+1642

 istat=PGOPEN('hw2.ps/vcps')  !PostScript
 if(istat<=0)stop 'ERR opening for PS file!'  
  
 call pgslw(4)
 call pgenv(0.0,120.0,3.0,25.0,0,1) !just,axis
 call pglab('Xi','Yi','sdv')
 call pgslw(2)
 !call pgmove(0.001,0.015)
 !call pgdraw(1.0,100.0)
 !call pgmove(0.001,0.09)
 !call pgdraw(5.0,100.0)
 !do i=1,2
 !call pgsls(i)
 !call pgmove(0.001,0.01*i)
 !call pgdraw(10.0,100.0,*i)
 !end do
 !call pgsci(30)
 !call pgrect(2.,6.,2.,4.)
 call pgsci(30)
 !call pgcirc(4.,3.,2.0)
 call pgline(30,x,yy)
 call pgsci(2)
 call pgpt(30,x,y,4)
 call pgend

end 

