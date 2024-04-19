program ex0925
 real X(10),Y(10)
 do i=1,10
 Open(1,file='XiYi.list',status='old')
 read(*,'(2x,F5.1,13x,F7.4)',end=23) X(i), Y(i)
 write(*,*) X(i), Y(i)
 !print*,X(i), Y(i)
 23 close(1)
 end do
end 
