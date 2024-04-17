program hk3_1
 real:: x(35),y(35),xsec,Z,sec, a, b, X_mean, Y_mean, XX_sum, XY_sum,t,sdv,R,M,P,O,sdvv
 integer:: min, i, n
 x=0
 y=0
 open(1,file='ppfile.txt',status='old')
 read(1,*)
 do i=1,35
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
t = (XX_sum)-(sum(x)**2)
a = (XX_sum*sum(y)-sum(x)*XY_sum)/t
b = (XY_sum-sum(x)*sum(y))/t
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
print*, a,b, sdvv,R

end