program matrix_inverse
  real(8) :: matrix(3, 3), inverse(3, 3)
  real(8) :: det

  ! Input the 3x3 matrix
  print *, "Enter the elements of the 3x3 matrix:"
  do i = 1, 3
     read *, matrix(i, :)
  end do

  ! Calculate the determinant of the matrix
  det = matrix(1,1)*(matrix(2,2)*matrix(3,3) - matrix(2,3)*matrix(3,2)) &
      - matrix(1,2)*(matrix(2,1)*matrix(3,3) - matrix(2,3)*matrix(3,1)) &
      + matrix(1,3)*(matrix(2,1)*matrix(3,2) - matrix(2,2)*matrix(3,1))

  ! Check if the matrix is invertible
  if (det == 0.0d0) then
     print *, "The matrix is singular and does not have an inverse."
  else
     ! Calculate the inverse of the matrix using Gauss-Jordan elimination
     inverse(1,1) = (matrix(2,2)*matrix(3,3) - matrix(2,3)*matrix(3,2)) / det
     inverse(1,2) = -(matrix(1,2)*matrix(3,3) - matrix(1,3)*matrix(3,2)) / det
     inverse(1,3) = (matrix(1,2)*matrix(2,3) - matrix(1,3)*matrix(2,2)) / det
     inverse(2,1) = -(matrix(2,1)*matrix(3,3) - matrix(2,3)*matrix(3,1)) / det
     inverse(2,2) = (matrix(1,1)*matrix(3,3) - matrix(1,3)*matrix(3,1)) / det
     inverse(2,3) = -(matrix(1,1)*matrix(2,3) - matrix(1,3)*matrix(2,1)) / det
     inverse(3,1) = (matrix(2,1)*matrix(3,2) - matrix(2,2)*matrix(3,1)) / det
     inverse(3,2) = -(matrix(1,1)*matrix(3,2) - matrix(1,2)*matrix(3,1)) / det
     inverse(3,3) = (matrix(1,1)*matrix(2,2) - matrix(1,2)*matrix(2,1)) / det

     ! Output the inverse matrix
     print *, "The inverse of the matrix is:"
     do i = 1, 3
        print *, inverse(i, :)
     end do
  end if

end program matrix_inverse
