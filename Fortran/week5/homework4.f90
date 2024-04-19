program ex1023
Real::zmin,zmax,xdis,xdep,vp,z(4003),x(4003),y(4003),dfz,Lon(4003),Lat(4003),Vpp,rainbow,dx, xr,xg,xb
Open(1,file='Vp_prof.dat',status='old')
read(1,*)
do i=1,4003
read(1,'(F10.4,F10.4,F10.4,F10.4,F10.4,F10.4)',end=23) xdis,xdep,vp,Vpp,Lon(i),Lat(i)
  z(i)=vpp
  x(i)=xdis
  y(i)=-1.*xdep
enddo
23 close(1)

  dfz=(maxval(z)-minval(z))/2.0
  zmax=maxval(z)
  zmin=minval(z)
 
 istat=PGOPEN('hw4.ps/vcps')  !PostScript
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
      xb=(z(i)-zmin)/dfz
    else
       xr=1.00-(z(i)-zmin-dfz)/dfz
       xg=1.00-(z(i)-zmin-dfz)/dfz
       xb=1.00
    endif
  call pgscr(30,xr,xg,xb)
  call pgsci(30)
  call pgrect(x(i)-dx,x(i)+dx,y(i)-dx,y(i)+dx)
  enddo

do i=1,10
rainbow=zmin+i*((maxval(z)-minval(z))/12)
    if(rainbow.le.(zmin+dfz))then
      xr=1.00
      xg=(rainbow-zmin)/dfz
      xb=(rainbow-zmin)/dfz
    else
       xr=1.00-(rainbow-zmin-dfz)/dfz
       xg=1.00-(rainbow-zmin-dfz)/dfz
       xb=1.00
    endif
  call pgscr(30,xr,xg,xb)
  call pgsci(30)
  call pgrect(50.+5.*(i-1),50.+5.+5.*(i-1),-140.,-140.+5.)
enddo

  call pgsci(30)
  call pgtext(55.0,-130.0,'Perturbation')
  call pgslw(6)
  call pgsci(30)
  call pgtext(40.0,-145.0,'-10%')
  call pgslw(6)
  call pgsci(30)
  call pgtext(75.0,-145.0,'0')
  call pgslw(6)
  call pgsci(30)
  call pgtext(95.0,-145.0,'10%')

call pgend
end 
