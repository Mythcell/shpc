!Program to calculate self-annihilation luminosity data
program lumi

	IMPLICIT NONE
	REAL, DIMENSION(:), ALLOCATABLE :: r, h, w, l, f
	REAL :: pi
	INTEGER :: i, j, k, npart
	
	pi = 3.14159
	npart = 10000
	allocate(r(npart),h(npart),w(npart),l(npart),f(npart))
	
	!Get the data file
	call read_file(r,h,w)
	write(*,*)r(1),h(1),w(1)
	
	call calculate_luminosity(w, l)
	call calculate_flux(r, l, f)
	write(*,*)l(1),f(1)
	
	call write_lf(r, l, f)
	
	contains
	
	subroutine calculate_flux(r, l, f)
	
		IMPLICIT NONE
		REAL, DIMENSION(:), ALLOCATABLE, INTENT(IN) :: r, l
		REAL, DIMENSION(:), ALLOCATABLE :: f
		
		do i=1,npart,1
			f(i) = (1/(4*pi))*(l(i)/(r(i)*r(i)))
		end do

	end subroutine calculate_flux
	
	subroutine calculate_luminosity(w, l)
	
		IMPLICIT NONE
		REAL, DIMENSION(:), ALLOCATABLE, INTENT(IN) :: w
		REAL, DIMENSION(:), ALLOCATABLE :: l
		
		do i=1,npart,1
			l(i) = w(i)*w(i)
		end do
	
	end subroutine calculate_luminosity
	
	subroutine read_file(r, h, w)
	
		IMPLICIT NONE
		REAL, DIMENSION(:), ALLOCATABLE :: r, h, w
		
		open(unit=1,file='results.dat',action='read')
		do i=1,npart,1
			read(1,*,end=100)r(i),h(i),w(i)
		end do
100		continue
		close(1)
	
	end subroutine read_file
	
	subroutine write_lf(r, l, f)
	
		IMPLICIT NONE
		REAL, DIMENSION(:), ALLOCATABLE :: r, l, f
		
		open(unit=2,file='flux.dat',action='write')
		do i=1,npart,1
			write(2,*)r(i),l(i),f(i),ABS(l(i)/LOG(r(i)))
		end do
		close(2)
		
	end subroutine
		

end program lumi