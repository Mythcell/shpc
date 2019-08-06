!Uses the direct-matrix method to approximate solutions to the 2D Schrödinger Equation

program ex5b

implicit none

!Interface to LAPACK's sgeev subroutine
interface
	subroutine sgeev(JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR, LDVR, WORK, LWORK, INFO)
	character :: JOBVL, JOBVR
	integer :: N, LDA, LDVL, LDVR, LWORK, INFO
	real(8) :: A(*), WR(*), WI(*), VL(*), VR(*), WORK(*)
	end subroutine sgeev
end interface

!Declare variables
real(8), allocatable, dimension(:,:) :: m_psi, m_v, m_e, m_i, leftvectors, rightvectors
real(8), allocatable, dimension(:) :: arr, eigen_real, eigen_im, work
real(8) :: delta_x, delta_y, energy
integer :: i, j, k, n_points, n_total, v_flag, info

!Enter user-input loop to choose potential
write(*,*)'Choose potential:'
do
	write(*,*)'Type 0 for (x^2+y^2), 1 for -exp(-(x^2+y^2))'
	read(*,*)v_flag
	if ((v_flag == 0) .or. (v_flag == 1)) then
		exit
	end if
end do
!Enter user-input loop to choose energy estimate
do
	write(*,*)'Enter energy estimate:'
	read(*,*)energy
	if ((energy >= 0) .and. (energy < 100)) then
		exit
	else
		write(*,*)'Energy must be between 0 and 100 inclusive'
	end if
end do

!The grid consists of 21 points separated by 1/sqrt(2)
!from -5sqrt(2) to 5sqrt(2) across x and y values.
!Hence matrix has 21**2 rows and 21**2 columns
n_points = 21; n_total = n_points**2

!Allocate arrays
allocate(m_psi(1:n_total, 1:n_total), m_v(1:n_total, 1:n_total), m_e(1:n_total, 1:n_total))
allocate(leftvectors(1:n_total, 1:n_total), rightvectors(1:n_total, 1:n_total))
allocate(eigen_real(1:n_total), eigen_im(1:n_total), work(1:n_total))

!Populate matrices
call populate_psi(m_psi)
call determine_potentials(m_v, v_flag)
call populate_energies(m_e, energy)

!Determine the final matrix
m_psi = m_psi + m_v - m_e

!Write the matrix to disk
call write_matrix(m_psi)

!Use LAPACK to calculate the eigenvalues
call sgeev('V', 'V', n_total, m_psi, n_total, eigen_real, eigen_im, &
& leftvectors, n_total, rightvectors, n_total, work, -1, info)

!Output to stdout
write(*,*)'Real eigenvalues:'
write(*,10) eigen_real
10 format("(",f12.4,",",f12.4,")")


contains

	!####################################################
	!This subroutine creates the bare psi matrix by populating
	!the diagonals with '4' and the entries above, below, to the
	!left and to the right with '-1'
	subroutine populate_psi(p_psi)
	
		real(8), allocatable, dimension(:,:), intent(inout) :: p_psi
		
		p_psi(1,1) = 4
		do i = 2, n_total
			p_psi(i,i) = 4
			p_psi(i-1,i) = -1
			p_psi(i,i-1) = -1
		end do
		
	end subroutine
	
	!####################################################
	!This subroutine creates a matrix where the leading
	!diagonal has the value of the energy
	subroutine populate_energies(pe_e, pe_energy)
	
		real(8), allocatable, dimension(:,:), intent(inout) :: pe_e
		real(8), intent(in) :: pe_energy
		
		do i = 1, n_total
			pe_e(i,i) = pe_energy
		end do
	
	end subroutine
	
	!####################################################
	!This subroutine determines the matrix of potentials
	!of the form
	!V_{1,1}	0		0		0		0		0
	!0		V_{1,2}		0		0		0		0
	!0			0	V_{1,n}		0		0		0
	!
	!0			0		0	V_{2,1}		0		0
	!
	!...		...		...		...		...		...
	!
	!0			0		0		0		0	V_{n,n}
	subroutine determine_potentials(dp_v, dp_flag)
	
		real(8), allocatable, dimension(:,:), intent(inout) :: dp_v
		integer, intent(in) :: dp_flag
		
		k = 1
		do i = 1, n_points
			do j = 1, n_points
				dp_v(k,k) = v(i,j,dp_flag)
				k = k + 1
			end do
		end do
		
	end subroutine
	
	!####################################################
	!This function determines the value of the potential
	!at the spatial coordinates given by the matrix entry
	!(i,j).
	real(8) function v(p_i, p_j, vflag) result(vval)
	
		integer, intent(in) :: p_i, p_j, vflag
		real(8) :: x, y
		
		!Map input points to positions on the grid
		x = (p_i - 1 - (n_points - 1)/2)*(1/sqrt(2.0))
		y = (p_j - 1 - (n_points - 1)/2)*(1/sqrt(2.0))
		
		!Return the potential depending on the flag value
		if (vflag == 0) then
			vval = x**2 + y**2
		else
			vval = -exp(-(x**2 + y**2))
		end if
	
	end function v

	!####################################################
	!Helper method to write a matrix to file
	subroutine write_matrix(wm_matrix)
	
		real(8), allocatable, dimension(:,:), intent(in) :: wm_matrix
		
		open(unit=1,file='matrix.dat')
		
		do i = 1, n_total
			write(1,*)(wm_matrix(i,j),j=1,n_total)
		end do
		
		close(1)
		
	end subroutine

end program ex5b
