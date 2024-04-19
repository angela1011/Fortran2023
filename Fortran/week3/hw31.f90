program hw31
    real :: m,n,l,x(30),y(30),s,sx,sy,sxx,sxy,del,a,b,r,e,averx,avery,f,g,h,sig,var
	Open(1,file='ppfile.txt',status='old')	
	Read(1,'(7x,f3.1)',err=99)
	
	
	do i=1,30
	   Read(1,'(6x,f5.1,13x,f5.2)',err=99)m,n
	   l=n-69.23
	   
	   x(i)=m
	   y(i)=l
	   sx=sx+m
	   sy=sy+l
	   sxx=sxx+(m)**2.0
	   syy=syy+(l)**2.0
	   sxy=sxy+m*l

	end do
	99 close(1)
	!print*,x,y
	!print*,sx,sy,sxx,syy,sxy
	s=30.0
	del=s*sxx-sx**2.0
	a=(sxx*sy-sx*sxy)/del
	b=(s*sxy-sx*sy)/del
	print*,del,a,b
	averx=sx/30.0
	avery=sy/30.0
	h=0.0
	do i=1,30
	   e=e+(x(i)-averx)*(y(i)-avery)
	   f=f+(x(i)-averx)**2.0
	   g=g+(y(i)-avery)**2.0
	   h=h+(y(i)-(b*(x(i))+a))**2	   
	end do
	!print*,h
	r=e/(sqrt(f)*sqrt(g))	
	var=(h/29.0)
	sig=var**0.5
	print*,r,var,sig,a,b,sxy
	
end program hw31