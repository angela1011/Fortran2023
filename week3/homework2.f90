program ex0925
!integer::A(3,6)= reshape( (/3,3/),(/2,3,6,1,4,9,2,6,1/) ) 
integer::(3,6)

do i=1,6
A(1,i)=A(1,i)/2
A(2,i)=A(2,i)/2
A(3,i)=A(3,i)/2
end do

do i=1,6
A(1,i)=A(1,i)/2
A(2,i)=A(2,i)/2
A(3,i)=A(3,i)/2
end do
do i=1,6
A(1,i)=A(1,i)/2
A(2,i)=A(2,i)/2
A(3,i)=A(3,i)/2
end do
do i=1,6
A(1,i)=A(1,i)/2
A(2,i)=A(2,i)/2
A(3,i)=A(3,i)/2
end do
do i=1,6
A(1,i)=A(1,i)/2
A(2,i)=A(2,i)/2
A(3,i)=A(3,i)/2
end do
do i=1,6
A(1,i)=A(1,i)/2
A(2,i)=A(2,i)/2
A(3,i)=A(3,i)/2
end do
print*, A
end
