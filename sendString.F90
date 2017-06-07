module mathOps

implicit none

integer:: log_error
attributes(managed) log_error

contains
  attributes(global) subroutine saxpy(x, y, a)
    implicit none
    real :: x(:), y(:)
    real, value :: a
    integer :: i, n
    n = size(x)
    i = blockDim%x * (blockIdx%x - 1) + threadIdx%x
    if (i <= n) y(i) = y(i) + a*x(i)
    call log(1)
  end subroutine saxpy

 attributes(device) subroutine log(error)
   implicit none
   integer, intent(in) ::error
   
   log_error = 3
 end subroutine log 
end module mathOps

program testSaxpy
  use mathOps
  use cudafor
  implicit none
  integer, parameter :: N = 40000
  real :: x(N), y(N), a
  real, device :: x_d(N), y_d(N)
  type(dim3) :: grid, tBlock
  integer :: istat

  tBlock = dim3(256,1,1)
  grid = dim3(ceiling(real(N)/tBlock%x),1,1)

  x = 1.0; y = 2.0; a = 2.0
  x_d = x
  y_d = y
  !call saxpy<<<grid, tBlock>>>(x_d, y_d, a)
  call saxpy<<<1,1>>>(x_d,y_d,a)
  y = y_d
  write(*,*) 'Max error: ', maxval(abs(y-4.0))

  istat = cudaDeviceSynchronize()
  write(*,*) 'my log thing', log_error  
end program testSaxpy
