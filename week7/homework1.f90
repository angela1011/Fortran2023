program ex1113
real:: t(30),SX(30),la,lo,h,st_x(210),sxx(30),syy(30)
real:: SY(30),SZ(30), ST(30),G(30,4),GT(4,30),C(4,4),m(4),d(30),x0,y0,z0,t0,st_y(210),st_z(210)
integer:: i,j,lat,lon,sta_in(1),k
Character(len=4):: sta(210)
Character(len=4):: sta0(30)
x0 = 121.3
y0 = 23.5
z0 = 25
t0 = 20 

open(2,file='nsta.dat ',status='old')
do k=1,210
 read(2,'(a4,i2,F5.2,1x,i3,F5.2,1x,F6.1)',end=300) sta(k),lat,la,lon,lo,h
 st_x(k)=lon+lo/60.0
 st_y(k)=lat+la/60.0
 st_z(k)=h/(-1000)
end do
300 close(2)


open(1,file='ppfile.txt',status='old')
read(1,*)
do i=1,30
read(1,'(1x,a4,19x,f5.2)',end=33) sta0(i),t(i)
do k=1,210
 if (sta(k)==sta0(i))then
 sx(i) = st_x(k)
 sy(i) = st_y(k)
 sz(i) = st_z(k)
end if
end do
end do
33 close(1)

do i=1,30
 call delaz(23.5,121.3,sy(i),sx(i),sxx(i),syy(i),delta) 
 !print*, sxx,syy
enddo

call delaz(23.5,120.5,24.5,121.5,dx,dy,delta) 


!m(1,1) = 0
!m(2,1) = 0
!m(3,1) = 0
!m(4,1) = 0

do i=1,100
 do j=1,30
 g(j,1)=sxx(j)-x0
 g(j,2)=syy(j)-y0
 g(j,3)=sz(j)-z0
 g(j,4)=-1.0*6.5*6.5*(t(j)-t0)
 d(j)=0.5*((sxx(j)-x0)**2+(syy(j)-y0)**2+(sz(j)-z0)**2-6.5*6.5*(t(j)-t0)**2)
 enddo

GT = TRANSPOSE(G)
C = matmul(GT,G)
call MATRIXINV(C,4) 
M = matmul(matmul(C,GT),d)


X0 = X0+m(1)
Y0 = Y0+m(2)
Z0 = Z0+m(3)
T0 = T0+m(4)

if(z0.lt.0.0)z0=abs(z0)
if((abs(m1))+(abs(m2))+(abs(m3))+(abs(m4)) .lt. 0.001)exit
enddo
print*, m(1),m(2),m(3),m(4)
print*,x0,y0,z0,t0
print*, (m(1)/dx)+121.3, (m(2)/dy)+23.5
end
