
program ex1016
 integer::Lat,Lon
 Real::N,J,M(49917),X(49917),Y(49917),xx(1482),yy(1482),D(49917),xr,xg,xb
 Open(1,file='1999.lis',status='old')
 do i=1,49917
 read(1,'(18x,i2,F5.2,i3,F5.2,F6.2,F4.2)',end=23) Lat, N, lon, J, d(i),M(i)
 y(i)= Lat+N/60.0
 x(i)= lon+J/60.0
enddo
23 close(1)

!print*, maxloc(d), maxval(d),minloc(d), minval(d)

Open(2,file='Taiwan.txt',status='old')
do i=1,1482
read(2,'(F7.3,F6.3)',end=13) xx(i), yy(i)
enddo
13 close(2)


 istat=PGOPEN('hw1.ps/vcps')  !PostScript
 if(istat<=0)stop 'ERR opening for PS file!'

 call pgslw(4)
 call pgsch(2.0)
 call pgscf(3)
 call pgenv(119.0,123.0,21.0,26.0,0,1) !just,axis
 call pglab('Longitude(E)','Latitude(N)','49917 events')
 call pgslw(2)
 call pgsci(2)
 call pgline(1482,xx,yy)
! call pgsci(30)

do i=1,49917
if(d(i) < 20 )then
 call pgscr(20,1.00, 0.00, 0.00)
 call pgsci(20)
 call pgsch(m(i)*0.1)
 call pgpt(1,x(i),y(i),4)
  else if(50>=d(i) .and. d(i)>=20)then
 call pgscr(50,0.00, 1.00, 0.00)
 call pgsci(50)
 call pgsch(m(i)*0.1)
 call pgpt(1,x(i),y(i),4)
  else
    call pgscr(500,0.00, 0.00, 1.00)
    call pgsci(500)
    call pgsch(m(i)*0.1)
    call pgpt(1,x(i),y(i),4)
endif
enddo


! call pgsci(30)


!if(d(i).le.((maxval(d) - minval(d)/3)))then
!      xr=1.0
!      xg=d(i)- minval(d)/((maxval(d) - minval(d))/3)
!      xb=0.0
!endif
!  call pgscr(30,xr,xg,xb)
!  call pgslw(1)

!if(d(i) .le. 2*(maxval(d) - minval(d)/3))then
!       xr=1.0 - d(i)- minval(d)/2*((maxval(d) - minval(d))/3)
!       xg=1.0
!       xb= d(i)- minval(d)/2*((maxval(d) - minval(d))/3)
!endif
!  call pgscr(30,xr,xg,xb)
!  call pgslw(1)
!if(d(i) .le. (maxval(d) - minval(d)))then
!       xr=0.0
!       xg=1.0 - d(i)- minval(d)/2*((maxval(d) - minval(d))/3)
!       xb=1.0
!endif
!  call pgscr(30,xr,xg,xb)
!  call pgslw(1)

 call pgend

end
