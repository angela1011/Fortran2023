program ex1106
real:: X, Y, Z, D(5), Xi(5), Yi(5), Zi(5), Di(5), DX, DY, DZ, GT(3,5), GTG(3,3), e, et,G(5,3),m(3)
 open(1,file='input.txt',status='old')
 do i=1,5
 read(1,'(f4.1,x,f4.1,x,f3.1,x,f4.2)',end=99) Xi(i),Yi(i), Zi(i), Di(i)
 end do
99 close(1)
x = 20
y = 20
z = 20

m(1) = 0
m(2) = 0
m(3) = 0

do n=1,100
do i=1,5
 g(i,1) = (Xi(i)-X)
 g(i,2) = (Yi(i)-Y)
 g(i,3) = (Zi(i)-Z)
 d(i) = 0.5 * (Di(i)*Di(i) - (Xi(i)-X)**2 - (Yi(i)-Y)**2 - (Zi(i)-Z)**2) 
enddo

GT = TRANSPOSE(G)
GTG = matmul(GT,G)

call MATRIXINV(GTG,3) 

m= matmul(matmul(GTG,GT),d)

if(maxval(m) < 0.00001)then
exit
endif

 X=X-m(1)
 Y=Y-m(2)
 Z=Z-m(3)

enddo
print*, m(1),m(2),m(3)
print*, X,Y,Z
end