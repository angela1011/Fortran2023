PROGRAM extest
  real :: x(524288), y(524288)
  integer :: Sec, N, i, istat
  real :: fxi(524288),fx(524288),f(262144),df,Z(524288)

  Open(1, file='molly_cry_out.txt', status='old')
  read(1, *)  ! Skip the first line if it contains headers
  do i = 1, 524288
    read(1, *) Sec, N
    x(i) = real(Sec)  ! Convert Sec to real
    y(i) = real(N)    ! Convert N to real
  end do
  close(1)

  istat = PGOPEN('test_cryout.ps/vcps')  ! PostScript
  if (istat <= 0) stop 'Error opening PS file!'

  call pgsubp(1, 3)
  call pgsci(1)
  call pgslw(4)
  call pgsch(2.0)
  call pgscf(3)
  call pgenv(0.0, 524288.0, -10000.0, 10000.0, 0, 1) ! Adjusted axis range
  call pglab('Time (sec)', 'Amplitude', 'Original Waveform')
  call pgslw(2)
  call pgsci(8)
  call pgline(524288, x, y)
  
  call FFT(y, x, 524288, 19)
  fx=y/524288.0
  fxi=x/524288.0
  df=1/(524288*0.0001) 
  
  do i=1,262144
   Z(i)=(((fx(i))**2 + (fxi(i))**2))**1/2
   f(i)=i*df
  end do

  call pgsci(1)
  call pgslw(4)
  call pgsch(2.0)
  call pgscf(3)
  call pgenv(0.0,5000.0,0.0,1000.0,0,1) !just,axis
  call pglab('frequency(Hz)','Amplitude','Fourier Transform')
  call pgslw(2)
  call pgsci(11)
  call pgline(262144,f,z) 

  call pgend
  end

SUBROUTINE FFT(XREAL,XIMAG,N,NU)
	DIMENSION XREAL(N),XIMAG(N)
	N2=N/2
	NU1=NU-1
	K=0
	DO 100 L=1,NU
102	DO 101 I=1,N2
	P=IBITR(K/2**NU1,NU)
	ARG=6.283185*P/FLOAT(N)
	C=COS(ARG)
	S=SIN(ARG)
	K1=K+1
	K1N2=K1+N2
	TREAL=XREAL(K1N2)*C+XIMAG(K1N2)*S
	TIMAG=XIMAG(K1N2)*C-XREAL(K1N2)*S
	XREAL(K1N2)=XREAL(K1)-TREAL
	XIMAG(K1N2)=XIMAG(K1)-TIMAG
	XREAL(K1)=XREAL(K1)+TREAL
	XIMAG(K1)=XIMAG(K1)+TIMAG
101	K=K+1
	K=K+N2
	IF(K.LT.N) GOTO 102
	K=0
	NU1=NU1-1
100	N2=N2/2
	DO 103 K=1,N
	I=IBITR(K-1,NU)+1
	IF(I.LE.K) GOTO 103
	TREAL=XREAL(K)
	TIMAG=XIMAG(K)
	XREAL(K)=XREAL(I)
	XIMAG(K)=XIMAG(I)
	XREAL(I)=TREAL
	XIMAG(I)=TIMAG
103	CONTINUE
	RETURN
	END
	FUNCTION IBITR(J,NU)
	J1=J
	IBITR=0
	DO 200 I=1,NU
	J2=J1/2
	IBITR=IBITR*2+(J1-2*J2)
200	J1=J2
	RETURN
	END