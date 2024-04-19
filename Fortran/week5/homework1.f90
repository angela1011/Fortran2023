program ex1016
 integer::Lat,Lon
 Real::N,J,M(49917),X(49917),Y(49917),xx(1482),yy(1482),D(49917),xr(49917),xg(49917),xb(49917),dmin,dmax,dfz,df
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
 call pgsci(30)
 call pgline(1482,xx,yy)

!do i=1,49917
!if(d(i) < 20 )then
! call pgscr(20,1.00, 0.00, 0.00)
! call pgsci(20)
! call pgsch(m*0.1)
! call pgpt(49917,x,y,4)
!else if(50>=d(i) .and. d(i)>=20)then
! call pgscr(50,0.00, 1.00, 0.00)
! call pgsci(50)
! call pgsch(m*0.1)
! call pgpt(49917,x,y,4)
!else 
! call pgscr(500,0.00, 0.00, 1.00)
! call pgsci(500)
! call pgsch(m*0.1)
! call pgpt(49917,x,y,4)
!endif
!enddo

dfz=(maxval(d)-minval(d))/3.0
df=maxval(d)-minval(d)
!print*, dfz,df,maxval(d),minval(d)

!if(d(i) .gt. maxval(d))then
!  d=dmax
!else if(d(i) .lt. minval(d))then
!  d=dmin
!endif

do i=1,49917
if(d(i) .le. (minval(d)+dfz))then
    xr=1.00
    xg=(d(i)-minval(d))/dfz
    xb=0.00
    call pgscr(30,xr,xg,xb)
    call pgsci(30)
    !call pgsch(m*0.1) 
    !call pgpt(49917,x,y,4)
    call pgcirc(x(i),y(i),m(i)*0.007)
else if(d(i) .le. (minval(d)+2.0*dfz))then
    xr=1.00-((d(i)-minval(d)-dfz)/dfz)
    xg=1.00
    xb=-((d(i)-minval(d)-dfz)/dfz)
    call pgscr(30,xr,xg,xb)
    call pgsci(30)
    !call pgsch(m*0.1) 
    !call pgpt(49917,x,y,4)
    call pgcirc(x(i),y(i),m(i)*0.007)
else
    xr=0.00
    xg=1.00-((d(i)-minval(d)-2*dfz)/dfz)
    xb=1.00
    call pgscr(30,xr,xg,xb)
    call pgsci(30)
    !call pgsch(m*0.1) 
    !call pgpt(49917,x,y,4)
    call pgcirc(x(i),y(i),m(i)*0.007)
    
endif
enddo
 call pgend
end
