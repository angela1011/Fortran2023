program hw121
integer :: z
real :: a,b,elon(49917),elat(49917),c,d,e,f,g,h,l,m,dep(49917),mag(49917),ma,de
real :: ex(44864),ed(44864),edep(49917),p,di,dis(49917),xx(49917),x,y

Open(1,file='1999.LIS',status='old')
 do i=1,49917
  Read(1,'(18x,f2.0,f5.2,f3.0,f5.2,f6.2,f4.2)',err=98)c,d,g,h,de,ma
	e=d*0.01/0.6	   
	f=c+e
	l=h*0.01/0.6	   
	m=l+g
  elon(i)=m
	elat(i)=f
	dep(i)=de
	mag(i)=ma
 end do
98 close(1)

call delaz(23.0,120.0,25.0,122.0,dx,dy,delta)
x=dx
y=dy

do i=1,49917
	call delaz(23.0,120.0,elat(i),elon(i),dx,dy,delta)
	p=(x*dx+y*dy)/((x**2+y**2)**0.5)
	di=sqrt(delta**2-p**2)
	dis(i)=di
	xx(i)=p
end do
z=0

j=1
do i=1,49917
if (dis(i)<50.0)then
 ex(j)=xx(i)
 ed(j)=-dep(i)
 j=j+1
 end if
 end do 

 istat=PGOPEN('hw1127_1.ps/cps') !PostScript
 if(istat<=0)stop 'ERR opening for PS file!'	
  call pgslw(4)
  call pgsch(2.0)
  call pgscf(3)
  call pgenv(0.0,delta,-150.0,0.0,0,0) !just,axis
  call pglab('x(km)','depth(km)','spatial')
	call pgpt(44864,ex,ed,4)
	call pgsci(2)
	call pgend
end
