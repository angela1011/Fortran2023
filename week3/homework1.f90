program ex0925
 integer:: min, i, n
 real::X(999),Y(999),sec, a, b, X_mean, Y_mean, XX_sum, XY_sum,t,sdv,R,M,P,O,sdvv,xa,ya
x=0
y=0
 Open(1,file='ppfile.txt',status='old')
 i=1
 read(1,*)
22 read(1,'(6x,F5.1,10x,i2,x,F5.2)',end=23) X(i), min, sec 
 y(i)=min+sec/60
 i=i+1
!print*, X,Y
goto 22
23 close(1)
! write(*,*) X, Y

n=i-1
X_mean=sum(x)/n
Y_mean=sum(y)/n
XX_sum=0.0
XY_sum=0.0
do i = 1, n

XX_sum = XX_sum + x(i)**2
XY_sum = XY_sum + x(i)*y(i)
end do
t = 30.0*(XX_sum)-(sum(x)**2)
a = (XX_sum*sum(y)-sum(x)*XY_sum)/t
b = (30*XY_sum-sum(x)*sum(y))/t
print*, t,a, b
m=0
p=0
O=0
sdv=0
do i=1,n
M=M+(x(i)-X_mean)*(y(i)-Y_mean)
p=p+(x(i)-X_mean)**2
O=O+(y(i)-Y_mean)**2
sdv=sdv+(y(i)-(a*x(i)+b))**2
end do 
R=M/(sqrt(p)*sqrt(o))
sdvv=(sdv/29.0)**0.5
print*, sdvv

end
