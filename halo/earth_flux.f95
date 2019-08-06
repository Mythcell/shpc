!Program to calculate self-annihilation flux of a dark matter halo as viewed
!from the perspective of an "Earth-bound" observer located outside of the center of the halo
program earthflux

	IMPLICIT NONE
	REAL, DIMENSION(:), ALLOCATABLE :: x,y,z,r,l,f,llnr
	REAL :: massarr,pi
	INTEGER :: i, np, npart
	
	pi=3.14159
	npart = 10000
	allocate(x(npart),y(npart),z(npart),r(npart),l(npart),f(npart),llnr(npart))
	call read_file(np, massarr, x, y, z)
	write(*,*)'Massarr:',massarr
	
	open(unit=1,file='flux.dat',action='read')
	do i=1,npart,1
		read(1,*,end=200)r(i),l(i),f(i),llnr(i)
	end do
200	continue
	close(1)
	
	write(*,*)r(1),l(1)
	call get_radii(x,y,z,r)
	call calculate_flux(r,l,f)
	
	open(unit=2,file='earth_flux.dat',action='write')
	do i=1,npart,1
		write(2,*)r(i),l(i),f(i),ABS(l(i)/LOG(r(i)))
	end do
	close(2)
	
	contains
	
	subroutine get_radii(x, y, z, radii)
	
		IMPLICIT NONE
		REAL, DIMENSION(:), ALLOCATABLE, INTENT(IN) :: x, y, z
		REAL, DIMENSION(:), ALLOCATABLE :: radii
		INTEGER :: i
		
		do i = 1,npart,1
			radii(i) = get_distance((/x(i),y(i),z(i)/), (/0.1,0.0,0.0/))
		end do
	
	end subroutine get_radii
	
	subroutine calculate_flux(r, l, f)
	
		IMPLICIT NONE
		REAL, DIMENSION(:), ALLOCATABLE, INTENT(IN) :: r, l
		REAL, DIMENSION(:), ALLOCATABLE :: f
		
		do i=1,npart,1
			f(i) = (1/(4*pi))*(l(i)/(r(i)*r(i)))
		end do

	end subroutine calculate_flux
	
	subroutine read_file(np, massarr, x, y, z)
	
		IMPLICIT NONE
		REAL, DIMENSION(:), ALLOCATABLE :: x,y,z
		REAL :: massarr
		INTEGER :: np
		
		open(1,file='nfw_ics.gdt',status='old',form='unformatted')
		read(1)np,massarr

		read(1,end=100)(x(i),y(i),z(i),i=1,npart)
100		continue
		close(1)
		
	end subroutine
	
	function get_distance(p1, p2) result(dist)
	
		REAL, DIMENSION(3), INTENT(IN) :: p1, p2
		REAL :: dist, x, y, z
		
		x = p1(1) - p2(1); y = p1(2) - p2(2); z = p1(3) - p2(3)
		
		dist = SQRT(x*x + y*y + z*z)
	
	end function get_distance

end program earthflux