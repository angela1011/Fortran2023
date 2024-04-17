! This is homework1
Program test1
 Real*4 x1,y1,x2,y2,a,b
 print*, 'testing' 
 read(*,*)x1,y1,x2,y2
 a=(y1-y2)/(x1-x2)
 b=-x1*(y1-y2)/(x1-x2)+y1
 print*,"Y = a X + b"
 print*,'a=',a,'b=',b
end program test1

