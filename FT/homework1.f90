program ex1127
real :: x(272833),y(272833),Z(262144)
real :: zza(272833),aa(272833),fxi(272833),fx(272833),f(131072),df
integer :: Sec,N

Open(1,file='molly_toy.txt',status='old')
read(1,*)
do i=1,272832
 read(1, *) Sec, N
  x(i) = sec
  y(i) = n
end do
close(1)

 istat=PGOPEN('hw1.ps/vcps')  !PostScript
 if(istat<=0)stop 'ERR opening for PS file!'

 call pgsubp(1,3)
 call pgsci(1)
 call pgslw(4)
 call pgsch(2.0)
 call pgscf(3)
 call pgenv(0.0,272832.0,-5500.0,5500.0,0,1) !just,axis
 call pglab('Time (sec)','Amplitude','ori')
 call pgslw(2)
 call pgsci(2)
 call pgline(272832,x,y)

 call pgend
end

