program ex1023
Real::zmin,zmax,xdis,xdep,vp,z(4003),x(4003),y(4003),dfz,Lon(4003),Lat(4003),Vpp,rainbow,dx, xr,xg,xb
Open(1,file='Vp_prof.dat',status='old')
read(1,*)
do i=1,4003
read(1,'(F10.4,F10.4,F10.4,F10.4,F10.4,F10.4)',end=23) xdis,xdep,vp,Vpp,Lon(i),Lat(i)
  z(i)=vp
  x(i)=xdis
  y(i)=-1.*xdep
enddo
23 close(1)

  dfz=(maxval(z)-minval(z))/3.0
  zmax=maxval(z)
  zmin=minval(z)
 
 istat=PGOPEN('hw3.ps/vcps')  !PostScript
 if(istat<=0)stop 'ERR opening for PS file!'
 call pgenv(0.0,200.0,-150.0,0.0,0,1) !just,axis
 call pglab('Distance(km)','Depth(km)','Profile')
 call pgslw(2)

!-- for vp
dx=2
do i=1,4003
    if(z(i).le.(zmin+dfz))then
      xr=1.00
      xg=(z(i)-zmin)/dfz
      xb=0.00
    else if(z(i).le.(zmin+2.*dfz))then
       xr=1.0-(z(i)-zmin-dfz)/dfz
       xg=1.00
       xb=(z(i)-zmin-dfz)/dfz
    else
       xr=0.00
       xg=1.00-(z(i)-zmin-2.*dfz)/dfz
       xb=1.00
    endif
  call pgscr(30,xr,xg,xb)
  call pgsci(30)
  call pgrect(x(i)-dx,x(i)+dx,y(i)-dx,y(i)+dx)
  enddo
do i=1,10
rainbow=zmin+i*((zmax-zmin)/12)
    if(rainbow .le.(zmin+dfz))then
      xr=1.00
      xg=(rainbow-zmin)/dfz
      xb=0.00
    else if(rainbow.le.(zmin+2.*dfz))then
       xr=1.0-(rainbow-zmin-dfz)/dfz
       xg=1.00
       xb=(rainbow-zmin-dfz)/dfz
    else
       xr=0.00
       xg=1.00-(rainbow-zmin-2.*dfz)/dfz
       xb=1.00
    endif
    print*, xr,xg,xb
  call pgscr(30,xr,xg,xb)
  call pgsci(30)
  call pgrect(50.+5.*(i-1),50.+5.+5.*(i-1),-140.,-140.+5.)
 print*, rainbow
enddo

  call pgsci(30)
  call pgtext(55.0,-130.0,'Vp(km/sec)')
  call pgslw(6)
  call pgsci(30)
  call pgtext(50.0,-145.0,'1')
  call pgslw(6)
  call pgsci(30)
  call pgtext(55.0,-145.0,'2')
  call pgslw(6)
  call pgsci(30)
  call pgtext(60.0,-145.0,'3')
  call pgslw(6)
  call pgsci(30)
  call pgtext(65.0,-145.0,'4')
  call pgslw(6)
  call pgsci(30)
  call pgtext(70.0,-145.0,'5')
  call pgslw(6)
  call pgsci(30)
  call pgtext(75.0,-145.0,'6')
  call pgslw(6)
  call pgsci(30)
  call pgtext(80.0,-145.0,'7')
  call pgslw(6)
  call pgsci(30)
  call pgtext(85.0,-145.0,'8')
  call pgslw(6)
  call pgsci(30)
  call pgtext(90.0,-145.0,'9')
  call pgslw(6)
  call pgsci(30)
  call pgtext(95.0,-145.0,'10')


  call pgend
end 
