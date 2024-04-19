program ex1120
 Type :: Header
	character*4  Code
	real*8 orgintime
	integer*2 ncom
	integer*4 ndata
	real dt
	character*4 blk
  End type Header 
Type (header) :: wh

real, allocatable :: wd(:,:)
real, allocatable :: Sec(:)
open(8,file='seismicdata.bin', status='old', form='unformatted', access='stream')
Read(8)wh
Allocate (wd(wh%ncom,wh%ndata))
Allocate (Sec(wh%ndata))
Read(8, end=99)wd
99 close(8) 
do i=1,wh%ndata
sec(i)=(i-1)*(wh%dt)
enddo

!print*, wh%ncom,wh%ndata, wh, 
print*, wd

 istat=PGOPEN('hw2.ps/vcps')  !PostScript
 if(istat<=0)stop 'ERR opening for PS file!'

 call pgsubp(1,3)
 call pgsci(1)
 call pgslw(4)
 call pgsch(2.0)
 call pgscf(3)
 call pgenv(0.0,46.0,-3.0,13.0,0,1) !just,axis
 call pglab('Time (sec)','Amplitude','Plot Z')
 call pgslw(2)
 call pgsci(2)
 call pgline(wh%ndata,Sec,wd(1,:))

 call pgsci(1)
 call pgslw(4)
 call pgsch(2.0)
 call pgscf(3)
 call pgenv(0.0,46.0,-1.0,15.0,0,1) !just,axis
 call pglab('Time (sec)','Amplitude','Plot N')
 call pgslw(2)
 call pgsci(2)
 call pgline(4600,Sec,wd(2,:))
 
 call pgsci(1)
 call pgslw(4)
 call pgsch(2.0)
 call pgscf(3)
 call pgenv(0.0,46.0,0.0,15.0,0,1) !just,axis
 call pglab('Time (sec)','Amplitude','Plot E')
 call pgslw(2)
 call pgsci(2)
 call pgline(4600,Sec,wd(3,:))  

call pgend
end
