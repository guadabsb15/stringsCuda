module mathOps

implicit none

integer, allocatable :: log_error
integer, allocatable :: place

attributes(managed) log_error
attributes(managed) place

contains
  attributes(global) subroutine saxpy(x, y, a)
    implicit none
    real :: x(:), y(:)
    real, value :: a
    integer :: i, n
    n = size(x)
    i = blockDim%x * (blockIdx%x - 1) + threadIdx%x
    if (i <= n) y(i) = y(i) + a*x(i)
    call log(1, i)
  end subroutine saxpy

 attributes(device) subroutine log(error, ind)
   implicit none
   integer, intent(in) ::error
   integer, intent(in) :: ind
   
   log_error = error
   place = ind
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

  allocate(log_error)
  allocate(place)
  
  tBlock = dim3(256,1,1)
  grid = dim3(ceiling(real(N)/tBlock%x),1,1)

  x = 1.0; y = 2.0; a = 2.0
  x_d = x
  y_d = y
  call saxpy<<<grid, tBlock>>>(x_d, y_d, a)
  !call saxpy<<<1,1>>>(x_d,y_d,a)
  y = y_d
  write(*,*) 'Max error: ', maxval(abs(y-4.0))

  istat = cudaDeviceSynchronize()
  if (log_error>0) write(*,*) 'called the error function', log_error
  write(*,*) 'place', place
  deallocate(log_error)
  deallocate( place)

end program testSaxpy
