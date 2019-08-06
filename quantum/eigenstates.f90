!This program uses the shooting method to solve for the energy eigenvalues for
!a given energy level n in the potential V(x) = (x^2)/2
!The wavefunction is outputted into the file wfunc.dat

program eigenfind

implicit none

real(8) :: emax, emin, energy, tolerance, bound, dx, x
real(8) :: mass, hbar, psi_bound, psi_e_bound
real(8), dimension(:), allocatable :: psi, psi_e
integer :: node, nsteps, j, nnodes
!Initialise main variables
emax = 20; emin = -10; mass = 1; hbar = 1
tolerance = 0.00001; bound = 10; dx = 0.005

!Calculate number of steps
nsteps = (2*bound)/dx + 1
j = 1
x = -bound
!Initially set to -1 to avoid conflicting with third loop conditional
nnodes = -1 

!Main loop to search over energy levels n=0,1,2,3
do node = 0,3

	!Reset energy levels and bounds
	emax = 20; emin = -10

	write(*,*)"Attempting to solve for energy level:",node

	!Second loop is to loop while the energy bounds are above the tolerance
	do while (abs((emax - emin)) >= tolerance)

		!Third loop to find the correct eigenstate
		do while (nnodes /= node)

			!Bisect the energy
			energy = (emin+emax)/2
	
			!Determine the wave function
			call solve_wfunc(energy, mass, hbar, nsteps, j, dx, x, psi)
	
			!Determine the number of nodes
			!Subtract 1 to remove the false "node" at unstable boundary
			nnodes = find_nodes()-1

			!Update energy bounds
			if (nnodes < node) then
				emin = energy
			else if (nnodes > node) then
				emax = energy
			end if
	
		!Exit third loop
		end do
	
		!Now we have found the correct number of nodes, continue
		!bisecting until energy interval is within tolerance
	
		!Bisect the energy
		energy = (emin+emax)/2
	
		!Determine the wave function for energy and emin
		call solve_wfunc(energy, mass, hbar, nsteps, j, dx, x, psi)
		psi_e = psi
		call solve_wfunc(emin, mass, hbar, nsteps, j, dx, x, psi)
		!Determine values at boundary
		psi_e_bound = psi_e(size(psi_e)-1); psi_bound = psi(size(psi)-1)
	
		!Update energy bounds accordingly
		if (psi_e_bound*psi_bound > 0) then
			emin = energy
		else
			emax = energy
		end if
	
	!Exit second loop
	end do

	write(*,*)"ENERGY: ",energy

!Exit main loop
end do

contains

	!=================================================
	!This function plots the wavefunction for the given
	!parameters and writes the output to a file named
	!"wfunc.dat".
	!=================================================
	subroutine solve_wfunc(energy, mass, hbar, nsteps, j, dx, x, psi)
		
		real(8), intent(in) :: energy, mass, hbar, dx, x
		real(8), dimension(:), allocatable, intent(out) :: psi
		integer, intent(in) :: nsteps, j
		real(8) :: w_energy, w_mass, w_hbar, w_dx, w_x
		integer :: w_nsteps, w_j
		w_energy = energy; w_mass = mass; w_hbar = hbar; w_nsteps = nsteps; w_j = j; w_dx = dx; w_x = x
		
		allocate(psi(1:nsteps-2))
		!Set initial values
		psi = 0
		psi(1) = 0.00001

		open(unit=1,file="wfunc.dat")
		
		do while (w_j < (w_nsteps-2))
			!Only write the wavefunction values
			write(1,*) psi(w_j)
			psi(w_j+1) = 2*(((w_mass*(w_dx**2))/(w_hbar**2))*(v(w_x + w_j*w_dx)-w_energy)+1)*psi(w_j) - psi(w_j-1)
			w_j = w_j + 1
		end do
		
		close(1)
	
	end subroutine solve_wfunc

	!=================================================
	!This function counts the number of nodes in a
	!wavefunction by counting the number of roots.
	!It does this by reading from the wfunc.dat file
	!and looking for adjacent terms with opposing signs.
	!=================================================
	integer function find_nodes() result(n)
		
		real(8) :: fn_a, fn_b
		n = 0
		
		open(unit=2,file="wfunc.dat",action="read")
		!Get the first number
		read(2,*) fn_a
		do
			!Read subsequent numbers
			read(2,*,end=20) fn_b
			if (fn_a*fn_b < 0) then
				!Update number of roots
				n = n + 1
			end if
			!Update index
			fn_a = fn_b
		end do
		20 close(2)
		
	end function find_nodes
	
	!=================================================
	!This function returns the value of the potential V
	!for some given value n.
	!=================================================
	real(8) function v(n) result(x)
		
		real(8), intent(in) :: n
		
		x = (n**2)/2
		
	end function v

end program eigenfind
