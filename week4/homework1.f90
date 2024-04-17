program ex1002
 integer::pgopen
 real Sec(4600),Z(4600),N(4600),E(4600), X,y,j,k
 x=0
 y=0
 j=0
 k=0
 Open(1,file='seisdata.txt',status='old')

 do i=1,4600
  read(1,'(4F12.3)', end=99) Sec(i), Z(i), N(i), E(i)
  
  x= sec(i)
  y= Z(i)
  j= N(i)
  k= E(i)
 end do

 99 close(1)
 
 print*, maxloc(Z), maxval(Z), maxval(N), maxval(E)

 istat=PGOPEN('hw1.ps/vcps')  !PostScript
 if(istat<=0)stop 'ERR opening for PS file!'

 call pgsubp(1,3)
 call pgsci(1)
 call pgslw(4)
 call pgsch(2.0)
 call pgscf(3)
 call pgenv(0.0,46.0,-1.0,13.0,0,1) !just,axis
 call pglab('Time (sec)','Amplitude','Plot Z')
 call pgslw(2)
 call pgsci(2)
 call pgline(4600,Sec,Z)
 call pgsci(30)
 call pgmove(20.230,0.0)
 call pgdraw(20.230,13.0)
 call pgsci(30)
 call PGtext(20.,10.,'peak value')

 call pgsci(1)
 call pgslw(4)
 call pgsch(2.0)
 call pgscf(3)
 call pgenv(0.0,46.0,-1.0,13.0,0,1) !just,axis
 call pglab('Time (sec)','Amplitude','Plot N')
 call pgslw(2)
 call pgsci(2)
 call pgline(4600,Sec,N)
 call pgsci(30)
 call pgmove(20.790,0.0)
 call pgdraw(20.790,13.0)
 call pgsci(30)
 call PGtext(20.,10.,'peak value')
 
 call pgsci(1)
 call pgslw(4)
 call pgsch(2.0)
 call pgscf(3)
 call pgenv(0.0,46.0,-1.0,13.0,0,1) !just,axis
 call pglab('Time (sec)','Amplitude','Plot E')
 call pgslw(2)
 call pgsci(2)
 call pgline(4600,Sec,E)  
 call pgsci(30)
 call pgmove(19.810,0.0)
 call pgdraw(19.810,13.0)
 call pgsci(30)
 call PGtext(20.,10.,'peak value')
 call pgend
 
end
