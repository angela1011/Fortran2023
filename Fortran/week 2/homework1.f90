program ex0918
 Character::I(3), J(3)
 Integer::k = 0, l = 0
 !read(*,*) I(1),I(2),I(3),J(1),J(2),J(3)
 read(*,*) I, J
 if (I(1)==J(1)) then
 k=k+1
 end if 
 if (I(2)==J(2))then
 k=k+1
 end if
 if (I(3)==J(3))then
 k=k+1
 end if
 if (I(1)==J(2))then
 l=l+1
 end if
 if (I(1)==J(3))then
 l=l+1
 end if
 if (I(2)==J(1))then
 l=l+1
 end if
 if (I(2)==J(3))then
 l=l+1
 end if
 if (I(3)==J(1))then
 l=l+1
 end if
 if (I(3)==J(2))then
 l=l+1
 end if
 write(*,*)k, 'A'
 write(*,*)l, 'B'
 
end
