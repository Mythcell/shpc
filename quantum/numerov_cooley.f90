!Uses the Numerov-Cooley method to approximate the wavefunctions for the
!harmonic potential V(x) = 6.4 + (1 - 1.2*exp(-(x-1.2)/2))^2

program ex5a

implicit none
real(8), allocatable, dimension(:) :: vj, pj, psij_sout, psij_sin
real(8) :: dx, energy, emax, emin, tolerance, delta_e
integer :: interval, nsteps, i, j, dirflag, match, z, n_count

!Here delta_e is initially chosen to be very large to avoid
!conflicting with the main loop.
interval = 5; dx = 0.05; tolerance = 0.0000001; delta_e = 10000

!Calculate the number of steps
nsteps = (interval)/dx + 1
!Set the matching point to be near the center of the interval
match = interval/(2*dx) + 1

!Prompt user for initial energy
input_loop: do
	write(*,*)'Enter initial energy estimate:'
	read(*,*)energy
	!Impose an arbitrary limit
	if (abs(energy) <= 100) then
		exit input_loop
	else
		write(*,*)'Energy must be between -100 and 100'
	end if
end do input_loop


!Enter main loop
do while ((abs(delta_e) > tolerance) .and. (abs(energy) <= 100))

	write(*,*)'Testing with energy:',energy

	!Determine the vj and pj arrays
	call populate_vj(vj, nsteps)
	call populate_pj(vj, pj, energy, nsteps)

	!Determine the two wavefunctions, shooting from the left and the right
	!dirflag, 1 for shooting in, 0 for shooting out
	call determine_psij(psij_sout, pj, nsteps, 0)
	call determine_psij(psij_sin, pj, nsteps, 1)

	!Scale functions to match at the match point
	call scale_wfuncs(psij_sout, psij_sin, match)

	!Normalise both functions
	call normalise_wfunc(psij_sin)
	call normalise_wfunc(psij_sout)

	!Write data to file for plotting, analysis, etc
	call write_wfuncs(psij_sout, psij_sin)
	call write_matchedfuncs(psij_sout, psij_sin)
	call write_potential(vj)

	!Count nodes
	call count_nodes(psij_sout, psij_sin, n_count)
	write(*,*)'Found nodes:',n_count

	!Determine the energy correction using Cooley formula
	call correct_energy(psij_sout, psij_sin, vj, pj, energy, delta_e)

	!Update the energy
	energy = energy + delta_e
	write(*,*)'Updated energy:',energy

	!Clear the arrays in readiness for next iteration
	call flush_all(psij_sout, psij_sin, vj, pj)

end do


contains

	!#######################################################
	!This subroutine determines the wavefunction psi.  The dirflag
	!variable indicates whether the wavefunction is shooting
	!forwards (out) or backwards (in).
	subroutine determine_psij(psij, pj, nsteps, dirflag)
	
		real(8), allocatable, dimension(:), intent(in) :: pj
		real(8), allocatable, dimension(:), intent(out) :: psij
		real(8) :: jpterm_nopsi, jpterm, jterm, jmterm, jmterm_nopsi
		integer, intent(in) :: nsteps, dirflag
		
		!Allocate space for the array
		allocate(psij(1:nsteps))
		
		if (dirflag == 1) then
			!Apply backward (inward) shooting
			psij(size(psij)-1) = 0; psij(size(psij)-2) = 0.0000001
			!Iterate backwards through the array
			do j = size(psij)-2, 2, -1
				jpterm = psij(j+1)*(1+((dx**2)/12)*pj(j+1))
				jterm = 2*psij(j)*(1-(5*(dx**2)/12)*pj(j))
				jmterm_nopsi = (1-((dx**2)/12)*pj(j-1))
				psij(j-1) = (jterm - jpterm)/jmterm_nopsi
			end do
				
		else
			!Apply forward (outward) shooting
			psij(1) = 0; psij(2) = 0.0000001
			!Iterate forwards through the array
			do j = 2, size(psij)-1
				jpterm_nopsi = 1+((dx**2)/12)*pj(j+1)
				jterm = 2*psij(j)*(1-(5*(dx**2)/12)*pj(j))
				jmterm = psij(j-1)*(1-((dx**2)/12)*pj(j-1))
				psij(j+1) = (jterm - jmterm)/jpterm_nopsi
			end do
			
		end if
	
	end subroutine
	
	!#######################################################
	!This subroutine scales the inward shooting wavefunction
	!so that it matches the outward shooting wavefunction
	!at the point specified by match
	subroutine scale_wfuncs(psij_sout, psij_sin, match)
	
		real(8), allocatable, dimension(:), intent(in) :: psij_sout
		real(8), allocatable, dimension(:), intent(inout) :: psij_sin
		real(8) :: sfactor
		integer, intent(in) :: match
		
		!Determine the scale factor and hence scale each entry
		!in the array
		sfactor = psij_sout(match)/psij_sin(match)
		do j = 1, size(psij_sin)
			psij_sin(j) = sfactor*psij_sin(j)
		end do
	
	end subroutine
	
	!#######################################################
	!This subroutine applies the Cooley formula to determine
	!the energy correction deltaE.
	subroutine correct_energy(w_out, w_in, v, p, e, de)
	
		real(8), allocatable, dimension(:), intent(in) :: w_out, w_in, v, p
		real(8), intent(in) :: e
		real(8), intent(out) :: de
		real(8) :: yplus, ymatch, yminus, veterm, norm
		
		yplus = w_in(match+1)*(1 + (1/12)*(dx**2)*p(match+1))
		ymatch = w_out(match)*(1 + (1/12)*(dx**2)*p(match))
		yminus = w_out(match-1)*(1 + (1/12)*(dx**2)*p(match-1))
		veterm = w_out(match)*(v(match)-e)
		
		!Determine the sum of squares
		do i = 1, match
			norm = norm + (w_out(i))**2
		end do
		do i = match+1, size(w_in)
			norm = norm + (w_in(i))**2
		end do
		
		!Apply the Cooley formula
		de = -0.5*((yplus - 2*ymatch + yminus)/(dx**2)) + veterm
		de = de*(w_out(match)/norm)
		write(*,*) de
	
	end subroutine
	
	!#######################################################
	!This subroutine counts the number of nodes in the 'matched'
	!wavefunction, hence determining the energy state.
	subroutine count_nodes(cn_out, cn_in, cn_nodes)
	
		real(8), allocatable, dimension(:), intent(in) :: cn_out, cn_in
		real(8) :: x
		integer, intent(out) :: cn_nodes
		
		cn_nodes = 0; x = cn_out(1)
		do i = 2, match
			if ((cn_out(i))*x < 0) then
				cn_nodes = cn_nodes + 1
			end if
			x = cn_out(i)
		end do
		do i = match+1, size(cn_in)
			if ((cn_in(i))*x < 0) then
				cn_nodes = cn_nodes + 1
			end if
			x = cn_in(i)
		end do
		
	end subroutine
		
	!#######################################################
	!This subroutine normalises the wavefunction by scaling
	!each entry with the normalisation factor.
	subroutine normalise_wfunc(arr)
	
		real(8), allocatable, dimension(:), intent(inout) :: arr
		real(8) :: norm, n_sfactor
		
		!Determine the sum of squares
		do i = 1, size(arr)
			norm = norm + (arr(i))**2
		end do
		
		!Scale each entry with the normalisation constant
		n_sfactor = 1/(sqrt(norm))
		do i = 1, size(arr)
			arr(i) = n_sfactor*arr(i)
		end do
	
	end subroutine
	
	!#######################################################
	!This subroutine deallocates all memory in each array
	subroutine flush_all(a1, a2, a3, a4)
	
		real(8), allocatable, dimension(:), intent(inout) :: a1, a2, a3, a4
		
		deallocate(a1)
		deallocate(a2)
		deallocate(a3)
		deallocate(a4)
		
	end subroutine
	
	!#######################################################
	!This subroutine populates the P_j array, where
	!			P_j = -2m/(hbar^2)(V_j - E)
	!To simplify things, m/(hbar^2) = 1
	subroutine populate_pj(vj, pj, energy, nsteps)
	
		real(8), allocatable, dimension(:), intent(in) :: vj
		real(8), allocatable, dimension(:), intent(out) :: pj
		real(8), intent(in) :: energy
		integer, intent(in) :: nsteps
		
		allocate(pj(1:nsteps))
		!pj = 0
		do i = 1, size(pj), 1
			pj(i) = -2.0*(vj(i) - energy)
		end do
	
	end subroutine

	!#######################################################
	!This subroutine populates the potential array vj
	subroutine populate_vj(vj, nsteps)

		real(8), allocatable, dimension(:), intent(out) :: vj
		integer, intent(in) :: nsteps
	
		allocate(vj(1:nsteps))
		!vj = 0
		do i = 1, size(vj), 1
			vj(i) = v(i)
		end do
	
	end subroutine
	
	!#######################################################
	!Helper method to write the two wavefunction arrays to file
	subroutine write_wfuncs(arr1, arr2)
	
		real(8), allocatable, dimension(:), intent(in) :: arr1, arr2
		
		open(unit=1,file='data.dat')
		
		do i = 1, size(arr1)
			write(1,*) i, arr1(i), arr2(i)
		end do
		
		close(1)
	
	end subroutine
	
	!#######################################################
	!Helper method to write the 'matched' wavefunction to file
	subroutine write_matchedfuncs(arr_out, arr_in)
	
		real(8), allocatable, dimension(:), intent(in) :: arr_out, arr_in
		
		open(unit=1,file='matched_data.dat')
		
		do i = 1, match
			write(1,*) i, arr_out(i)
		end do
		
		do i = match+1, size(arr_in)
			write(1,*) i, arr_in(i)
		end do
		
		close(1)
		
	end subroutine
	
	!#######################################################
	!Helper method to write the potential to file
	subroutine write_potential(v_arr)
	
		real(8), allocatable, dimension(:), intent(in) :: v_arr
		
		open(unit=1,file='potential_v.dat')
		
		do i = 1, size(v_arr)
			write(1,*) i, v_arr(i)
		end do
		
		close(1)
		
	end subroutine
	
	!#######################################################
	!This function returns the value of the potential for the
	!given spatial step.
	real(8) function v(i) result(vval)
	
		integer, intent(in) :: i
		real(8) :: v_xval
	
		!Convert the current spatial step into the actual x value.
		v_xval = 0 + i*dx
		
		!Evaluate the value of the potential at the position x
		vval = 6.4 + (1-1.2*exp(-(v_xval-1.2)*0.5))**2
	
	end function v

end program ex5a
