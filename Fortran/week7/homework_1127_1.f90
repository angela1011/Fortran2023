program ex1127
 integer::Lat,Lon,n
 Real::C,J,X(49917),Y(49917),AB,B,R,A,xx,yy,xxx(49917),yyy(49917),D(49917)

Open(1,file='1999.lis',status='old')
n=0
 do i=1,49917
 read(1,'(18x,i2,F5.2,i3,F5.2,F6.2)',end=23) Lat, C, lon, J,d(i)
  y(i)= Lat+C/60.0
  x(i)= lon+J/60.0
  call delaz(23,120,25,122,dx,dy,delta) 
  AB = (x(i)-120)*(122-x(i))+(y(i)-23)*(25-y(i))
  A = ((x(i)-120)**2 + (y(i)-23)**2)**1/2
  r = (AB/B)
  B = ((122-120)**2+(25-23)**2)**1/2
  xx = 120+2*(r/b)
  yy = 23+2*(r/b)
  call delaz(xx,yy,x(i),y(i),dx,dy,delta) 
  if(delta <= 50.0)then
    xxx(i)=x(i)
    yyy(i)=y(i)
    n=n+1
  endif
 enddo
23 close(1)

 istat=PGOPEN('hw1127.ps/vcps')  !PostScript
 if(istat<=0)stop 'ERR opening for PS file!'
 call pgslw(4)
 call pgsch(2.0)
 call pgscf(3)
 call pgenv(0,delta,maxval(d),0.0,0,1) !just,axis
 call pglab('X(km)','Depth(km)','Profile')
 call pgslw(2)
 call pgsci(30)
 do i=1,n
 call pgcirc(xxx(i),(-1.)*d(i),0.01)
 enddo

! call pgslw(4)
! call pgsch(2.0)
! call pgscf(3)
! call pgenv(119.0,123.0,20.0,26.0,0,1) !just,axis
! call pglab('Longitude(E)','Latitude(N)','49917 events')
! call pgslw(2)
! call pgsci(30)
! do i=1,n
! call pgcirc(xxx(i),yyy(i),0.01)
! enddo

 call pgend
end 