Program ex0918
 Character::I(3), J(3)
 Integer::k = 0, l = 0
 read(*,*) I
 11 read(*,*) J
 !I(1)/=I(2) .and. I(1)/=I(3) .and. I(2)/=I(3)
 !J(1)/=J(2) .and. J(1)/=J(3) .and. J(2)/=J(3)
 if ((I(1)==J(1)) .or. (I(2)==J(2)) .or. (I(3)==J(3))) then
 k=k+1 
 goto 77
 end if 

 if (I(1)==J(2) .or. I(1)==J(3) .or. I(2)==J(1) .or. I(2)==J(3) .or. I(3)==J(1) .or. I(3)==J(2))then
 l=l+1
 goto 66
 end if

 77 write(*,*)k, 'A'
 66 write(*,*)l, 'B'
 if (k/=3) then
 goto 11
 end if
end