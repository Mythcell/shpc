!This is the parallel program for SHPC4002 Astro Assignment 2 (MPI only)
!
!by Mitchell Cavanagh, 21727193
!
!compile with $ mpifort -o halo_mpionly halo_mpionly.f95
!
!run with $ mpirun -np # ./halo_mpionly
!NB: {num_cells} must be divisible by #
!
!ver 0.4a, October 24, 2018
program halo

	USE MPI

	IMPLICIT NONE
	
	!This type defines nodes for the linked list
	type Linked_List
		REAL, DIMENSION(3) :: pos
		INTEGER :: id
		TYPE(Linked_List), POINTER :: next => NULL ()
	end type Linked_List
	
	!This type defines an array of pointers to linked lists
	!(to be used for the 3D cubic mesh and 1D neighbours array)
	type Mesh_t
		TYPE(Linked_List), POINTER :: p
	end type Mesh_t
	
	!Main program variables
	REAL, DIMENSION(:), ALLOCATABLE :: x, y, z, bounds_arr, bounds_minvals, h_smooth, w_local, radii
	REAL :: massarr, mesh_bound, mesh_subsize
	INTEGER :: i, j, k, np, num_cells, npart, num_neighbours
	TYPE(Mesh_t), DIMENSION(:,:,:), ALLOCATABLE :: mesh
	TYPE(Mesh_t), DIMENSION(:), ALLOCATABLE :: neighbours
	INTEGER, DIMENSION(:), ALLOCATABLE :: domain_start, domain_end, domain
	LOGICAL, DIMENSION(:,:), ALLOCATABLE :: nbr_arrs
	LOGICAL, DIMENSION(:), ALLOCATABLE :: nbr_arr
	
	!MPI-related variables
	INTEGER, DIMENSION(MPI_STATUS_SIZE) :: mpi_status
	INTEGER :: ierr, mpi_rank, mpi_size, mpi_src, mpi_dest, mpi_tg, mpi_rootp
	
	!----------------------------------------------------------------------------
	!The number of particles to consider
	npart = 10000
	!The cubic mesh will have dimensions {num_cells} X {num_cells} X {num_cells}
	num_cells = 24
	!We wish to search for, at most, {num_neighbours} nearest neighbours
	num_neighbours = 32
	!----------------------------------------------------------------------------
	
	!Set up MPI
	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, mpi_rank, ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD, mpi_size, ierr)
	mpi_rootp = 0
	
	!Print the number of processes
	if (mpi_rank == mpi_rootp) then
		write(*,*)'This is halo (MPI), executing with',mpi_size,'processes'
		write(*,*)'Particles:',npart
	end if
	
	!Set up the position arrays
	allocate(x(npart),y(npart),z(npart),radii(npart))
	!Preallocate the 3D cubic mesh and bounds arrays
	allocate(mesh(num_cells,num_cells,num_cells))
	allocate(bounds_arr(3),bounds_minvals(3))
	allocate(neighbours(npart))
	!Initialise the custom data structures
	call init_mesh(mesh); call init_neighbours(neighbours)
	!This is to store the bounds of the domain decomposition
	allocate(domain_start(mpi_size),domain_end(mpi_size),domain(2))
	!This is to keep track of particle IDs for sending between processes
	allocate(nbr_arr(npart*(npart+1)))
	!Initialise the flat array to false
	do i=1,size(nbr_arr),1
		nbr_arr(i) = .FALSE.
	end do
	
	!ONLY THE ROOT PROCESS REQUIRES I/O - THE DATA IS BROADCAST TO THE OTHER PROCESSES
	!------------------------------------------------------------------------------------!
	if (mpi_rank == mpi_rootp) then
	
		!Read in the GADGET file (must be of name nfw_ics.gdt)
		call read_file(np, massarr, x, y, z)
		
		!Read in mock positional test data (test_data.dat)
!		call read_mock_file(x, y, z)

		!Calculate the distance between each particle and the origin
		call get_radii(x, y, z, radii)
		
	end if
	!The data is then broadcast to the rest of the worker processes
	call MPI_BCAST(x, size(x), MPI_REAL, mpi_rootp, MPI_COMM_WORLD, ierr)
	call MPI_BCAST(y, size(y), MPI_REAL, mpi_rootp, MPI_COMM_WORLD, ierr)
	call MPI_BCAST(z, size(y), MPI_REAL, mpi_rootp, MPI_COMM_WORLD, ierr)
	call MPI_BCAST(radii, size(radii), MPI_REAL, mpi_rootp, MPI_COMM_WORLD, ierr)
	!Now sync everything for the pre-processing
	call MPI_BARRIER(MPI_COMM_WORLD, ierr)
	
	!write(*,*)'Process',mpi_rank,'reports',x(1),y(1),z(1),radii(1)

	!ALL PROCESSES DO THEIR OWN PRE-PROCESSING
	!------------------------------------------------------------------------------------!
	!Determine the bounds of the mesh
	call determine_mesh_bounds(x, y, z, bounds_arr, bounds_minvals)
	!The length of the cubic mesh should be the largest of these separations
	mesh_bound = maxval(bounds_arr); mesh_subsize = mesh_bound / num_cells
		
	!Populate the mesh with the particles
	!(i.e assign each particle to the relevant cube in the mesh based on position)
	call populate_mesh(x,y,z,mesh,bounds_minvals)
	
	!(DEBUG) Determine a density estimate based on the population of the cubes in the mesh
	!This will write to a file called 'cube_density.dat'
	!call get_density_estimate(mesh)
	
	!Merge cubes with less than num_neighbours particles
	call merge_sparse_cubes(mesh)
	!call print_mesh_sizes(mesh) !for debugging purposes
	
	!Sync here to ensure all processes have finished pre-processing
	call MPI_BARRIER(MPI_COMM_WORLD, ierr)
	deallocate(bounds_arr,bounds_minvals)
	
	!ROOT PROCESS DETERMINES DOMAIN DECOMPOSITION
	!------------------------------------------------------------------------------------!
	if (mpi_rank == mpi_rootp) then
		write(*,*)'Pre-processing done... determining domain decomposition'
		call determine_decomposition(domain_start, domain_end)
		!Set domains for the root process
		domain(1) = domain_start(1); domain(2) = domain_end(1)
		
		!We are now ready to perform the neighbour checking
		!Send domains to the worker processes (start from 2 to skip root process)
		do i=2,mpi_size,1
			call MPI_SEND((/domain_start(i),domain_end(i)/), 2, MPI_INTEGER, i-1, 10, MPI_COMM_WORLD, ierr)
		end do
	else
		!All other processes receive the domain decomposition from the root
		call MPI_RECV(domain, 2, MPI_INTEGER, mpi_rootp, 10, MPI_COMM_WORLD, mpi_status, ierr)
	end if
	
	!Sync processes to begin the neighbour check
	write(*,*)mpi_rank,'has domain',(domain(i),i=1,size(domain))
	call MPI_BARRIER(MPI_COMM_WORLD, ierr)
	
	!EACH PROCESS NOW CHECKS FOR NEIGHBOURS WITHIN THEIR OWN LOCAL DOMAIN
	!------------------------------------------------------------------------------------!
	if (mpi_rank == mpi_rootp) then
		write(*,*)'Checking neighbours... (this may take some time)'
	end if
	
	call check_neighbours(mesh, neighbours, domain)
	write(*,*)'process',mpi_rank,'neighbour check complete'
	
	!Now each process translates the neighbours array into a flat array
	!write(*,*)'process',mpi_rank,'transferring neighbours'
	call translate_neighbours(neighbours, nbr_arr)
	
	!Root process stores its own neighbour array...
	if (mpi_rank == mpi_rootp) then
		!Only the root process needs to hold memory for nbr_arrs
		allocate(nbr_arrs(mpi_size,npart*(npart+1)))
		!Store own array
		do i=1,size(nbr_arr)
			nbr_arrs(1,i) = nbr_arr(i)
		end do
		do i=2,mpi_size,1
			!Store arrays received from the worker processes
			call MPI_RECV(nbr_arr, npart*npart, MPI_LOGICAL, i-1, 20, MPI_COMM_WORLD, mpi_status, ierr)
			do j=1,size(nbr_arr)
				nbr_arrs(i,j) = nbr_arr(j)
			end do
		end do
	else !...while the worker processes transfer their own neighbour arrays to the root process
		call MPI_SEND(nbr_arr, npart*npart, MPI_LOGICAL, mpi_rootp, 20, MPI_COMM_WORLD, ierr)
	end if
	
	!Sync processes for post-processing (to ensure all neighbour data has been processed by root)
	call MPI_BARRIER(MPI_COMM_WORLD, ierr)
	!These arrays are no longer needed
	deallocate(nbr_arr,domain_start,domain_end,domain)
	
	!THE ROOT PROCESS IS LEFT TO CONDUCT POST-PROCESSING AND FINAL I/O
	!------------------------------------------------------------------------------------!
	if (mpi_rank == mpi_rootp) then
		!Reset neighbour linked list
		call free_neighbours(neighbours); call init_neighbours(neighbours)
		
		!Parse all the {mpi_size} flat arrays and re-populate final neighbour list
		write(*,*)'Reconstructing neighbour list...'
		call merge_neighbour_lists(nbr_arrs, neighbours)
		!nbr_arrs no longer needed
		deallocate(nbr_arrs)
		
		!Set up arrays for h and w
		allocate(h_smooth(npart),w_local(npart))
		
		!Now calculate the local densities using the combined neighbour list
		!call print_neighbour_sizes(neighbours)
		call calculate_density(h_smooth, w_local, neighbours)

		!Finally, write everything to file
		open(unit=1,file='results.dat',action='write')
		do i=1,size(h_smooth),1
			write(1,*)radii(i),h_smooth(i),w_local(i)
		end do
		close(1)
		
		deallocate(h_smooth,w_local)
	end if
	
	!Sync all processes one last time for memory deallocation
	call MPI_BARRIER(MPI_COMM_WORLD, ierr)
	!All processes can release memory
	call free_mesh(mesh); call free_neighbours(neighbours)
	!Free all allocatable arrays
	deallocate(x,y,z,mesh,neighbours,radii)
	
	!Finally, terminate MPI
	call MPI_FINALIZE(ierr)
	
	
	!----------------------------------------------------------------------------
	contains
	!----------------------------------------------------------------------------
	
	
	!Calculate the smoothing length h for each particle, i.e the distance to
	!the furtherest neighbour out of all the nearest neighbours to that particle.
	subroutine calculate_density(h_smooth, w_local, neighbours)
	
		IMPLICIT NONE
		TYPE(Mesh_t), DIMENSION(:), ALLOCATABLE, INTENT(IN) :: neighbours
		TYPE(Linked_List), POINTER :: ll
		REAL, DIMENSION(3) :: a_pos, b_pos
		REAL, DIMENSION(:), ALLOCATABLE :: h_smooth, w_local
		REAL :: r, r_max, h, w
		INTEGER :: i, c
		
		write(*,*)'Calculating densities...'
		do i = 1,size(neighbours),1
		
			r_max = 0.0; w = 0.0; c = 0
			!First calculate the smoothing length h_smooth
			ll => neighbours(i)%p
			!Set a_pos to the coordinates of the ith particle
			a_pos = (/x(i),y(i),z(i)/)
			do while (associated(ll%next))
				ll => ll%next
				b_pos = ll%pos
				r = get_distance(a_pos, b_pos)
				r_max = MAX(r_max, r)
			end do
			h_smooth(i) = r_max; h = r_max
			
			!Now calculate the weight using the cubic spline kernel
			ll => neighbours(i)%p !rewind ll
			do while (associated(ll%next))
				ll => ll%next
				b_pos = ll%pos
				r = get_distance(a_pos, b_pos)
				w = w + w_kernel(r, h)
				c = c + 1
			end do
			w_local(i) = w
			
		end do
		write(*,*)'...done'
	
	end subroutine calculate_density
	
	
	!Merges all the flat neighbour arrays from each process to create a combined neighbour list
	subroutine merge_neighbour_lists(nbr_arrs, neighbours)
	
		IMPLICIT NONE
		LOGICAL, DIMENSION(:,:), ALLOCATABLE, INTENT(IN) :: nbr_arrs
		TYPE(Mesh_t), DIMENSION(:), ALLOCATABLE :: neighbours
		TYPE(Linked_List), POINTER :: ll
		INTEGER :: i, j, k
		
		!Loop over the neighbour mesh
		do i=1,size(neighbours),1
			ll => neighbours(i)%p
			!Now search over the nbr_arrs matrix for a valid id entry
			do j=1,mpi_size,1
				if (nbr_arrs(j,i*npart)) then
					!write(*,*)'Found',i,'via process',j-1
					!Now search for ids of neighbouring particles...
					do k=1,npart-1,1
						if (nbr_arrs(j,i*npart+k)) then
							!...add these to the neighbour list
							call ll_insert(ll, (/x(k),y(k),z(k)/), k)
						end if
					end do
					!No need to loop again if we have already found a list of neighbours
					EXIT
				end if
			end do
		end do
	
	end subroutine merge_neighbour_lists
	
	
	!Parses a (Mesh_t) neighbour list into a 1D logical flat array to be used with MPI
	subroutine translate_neighbours(neighbours, nbr_arr)
	
		IMPLICIT NONE
		TYPE(Mesh_t), DIMENSION(:), ALLOCATABLE, INTENT(IN) :: neighbours
		TYPE(Linked_List), POINTER :: ll
		LOGICAL, DIMENSION(:), ALLOCATABLE :: nbr_arr
		INTEGER :: i, j, s
		
		!Loop over each entry in the neighbour list
		s = size(neighbours)
		do i=1,s,1
			ll => neighbours(i)%p
			!i.e if the check found neighbours for i, set (i*s) to true
			if (ll_size(ll) > 0) then
				nbr_arr(i*s) = .TRUE.
			end if
			!Now, for each neighbour of i, go and set the corresponding entries in nbr_arr to true
			do while (associated(ll%next))
				ll => ll%next
				nbr_arr(i*s + ll%id) = .TRUE.
			end do
		end do
	
	end subroutine translate_neighbours
	
	
	!For every particle in each of the cubic mesh's linked lists, determine
	!the nearest neighbouring particles and append these to the relevant
	!linked list in the neighbours array
	subroutine check_neighbours(mesh, neighbours, domain)
	
		IMPLICIT NONE
		TYPE(Mesh_t), DIMENSION(:,:,:), ALLOCATABLE, INTENT(IN) :: mesh
		TYPE(Mesh_t), DIMENSION(:), ALLOCATABLE :: neighbours
		TYPE(Linked_List), POINTER :: ll, current
		REAL, DIMENSION(:), ALLOCATABLE :: distances
		REAL, DIMENSION(3) :: a_pos, b_pos
		REAL :: d, d_min, d_max, d_m
		INTEGER, DIMENSION(:), ALLOCATABLE :: id_seen
		INTEGER, DIMENSION(2), INTENT(IN) :: domain
		INTEGER :: i, j, k, l, m, c, d_num, k_start, k_end
		
		allocate(id_seen(npart))
		!write(*,*)'Checking neighbours... (this may take some time)'
		k_start = domain(1); k_end = domain(2)
		
		do k = k_start,k_end,1
			!write(*,*)'process:',mpi_rank,'k: ',k
			do j = 1,num_cells,1
				do i = 1,num_cells,1
					!write(*,*)'Checking:',i,j,k
					c = MIN(ll_size(mesh(i,j,k)%p), num_neighbours)
					!Only proceed if there are at least 2 particles in the list
					if (c.LT.2) then
						continue
					else
						
						!set ll to the first proper node in the list
						ll => mesh(i,j,k)%p%next
						!loop over all particles in the list
						do while(associated(ll))
							
							!set up the distances array
							allocate(distances(npart))
							
							!only proceed if ll has not already been checked
							!(this is to avoid the inevitable duplicate entries
							!from merging the sparse cubes)
							if (.NOT.(ANY(id_seen==ll%id))) then
						
								!Loop from the beginning
								current => mesh(i,j,k)%p%next
								a_pos = current%pos
								!Initially set this to some arbitrary value which will always be higher
								d_min = mesh_bound + 1
								!Similarly, set these to 0
								d_max = 0; d_num = 0
							
								!loop over remaining particles in the list
								do while(associated(current%next))
									!current => current%next
									!Ignore this node if it is the current one
									if ((current%id == ll%id)) then
										continue
									else
										b_pos = current%pos
										d = get_distance(a_pos, b_pos)
										distances(current%id) = d
										!write(*,*)'Adding:',current%id
										d_num = d_num + 1
										d_max = MAX(d_max, d)
									end if
									current => current%next
								end do
								
								!Add this to the array of checked particles
								id_seen(ll%id) = ll%id
								
								!confirm that c is at least equal to the number of nonzero entries
								!in the distance array (had some problems with MINLOC without this)
								c = MIN(c,d_num)
								
								!find the c nearest neighbours
								do l=1,c,1
								
									!Only consider elements greater than some threshold value
									!(so that it doesn't trigger on entries equal to 0)
									m = MINLOC(distances, DIM=1, MASK=(distances > 0.000001))
									!write(*,*) m
									
									if (m == 0) then !there were some issues with MINLOC
										continue
									else
										!search for the particle in the linked list corresponding to this id
										!and add it to the corresponding neighbours list
										current => mesh(i,j,k)%p%next
										do while (associated(current%next))
											if (current%id == m) then
												call ll_insert(neighbours(ll%id)%p, current%pos, current%id)
											end if
											current => current%next
										end do
										!now set this distance to 0 so that it won't trigger MINLOC
										distances(m) = 0.0
									end if
									
								end do
							
							else !ll already checked
								continue
							end if
							
							!Iterate through the linked list
							ll => ll%next
							
							!free the distance array for use again (to avoid any cross-reference)
							deallocate(distances)
							
						end do
					end if
				end do
			end do
		end do
		!write(*,*)'...done'
	
	end subroutine check_neighbours
	
	
	!For all cubes containing less than {num_neighbours} particles, append the particles
	!located in the surrounding cubes
	subroutine merge_sparse_cubes(mesh)
	
		IMPLICIT NONE
		TYPE(Mesh_t), DIMENSION(:,:,:), ALLOCATABLE :: mesh, mesh_new
		TYPE(Linked_List), POINTER :: lla, llb
		INTEGER :: i, j, k
		
		!write(*,*)'Merging sparse cubes'
		allocate(mesh_new(num_cells,num_cells,num_cells))
		call init_mesh(mesh_new)
		!Copy the entire contents of mesh into mesh_new
		do k = 1,num_cells,1
			do j = 1,num_cells,1
				do i = 1,num_cells,1
					call ll_append(mesh_new(i,j,k)%p,mesh(i,j,k)%p)
				end do
			end do
		end do
		
		!For cubes with less than 32 particles, append all 26 neighbouring cubes
		!Note the different loop bounds
		do k = 2,num_cells-1,1
			do j = 2,num_cells-1,1
				do i = 2,num_cells-1,1
					if (ll_size(mesh(i,j,k)%p).LT.(num_neighbours)) then
						call ll_append(mesh_new(i,j,k)%p, mesh(i-1,j-1,k-1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i,j-1,k-1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i+1,j-1,k-1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i+1,j,k-1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i,j,k-1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i-1,j,k-1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i+1,j+1,k-1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i,j+1,k-1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i-1,j+1,k-1)%p)
						
						call ll_append(mesh_new(i,j,k)%p, mesh(i+1,j-1,k)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i,j-1,k)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i-1,j-1,k)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i+1,j,k)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i-1,j,k)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i+1,j+1,k)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i,j+1,k)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i-1,j+1,k)%p)
						
						call ll_append(mesh_new(i,j,k)%p, mesh(i+1,j-1,k+1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i,j-1,k+1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i-1,j-1,k+1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i+1,j,k+1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i,j,k+1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i-1,j,k+1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i+1,j+1,k+1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i,j+1,k+1)%p)
						call ll_append(mesh_new(i,j,k)%p, mesh(i-1,j+1,k+1)%p)												
					end if
				end do
			end do
		end do
		
		!Reset original mesh
		call free_mesh(mesh)
		call init_mesh(mesh)
		!Copy everything back over
		do k = 1,num_cells,1
			do j = 1,num_cells,1
				do i = 1,num_cells,1
					call ll_append(mesh(i,j,k)%p,mesh_new(i,j,k)%p)
				end do
			end do
		end do
		!Finally release the duplicate mesh
		call free_mesh(mesh_new)
		deallocate(mesh_new)
	
	end subroutine merge_sparse_cubes
	
	
	!Populate the cubic mesh with all npart particles based on their positions
	subroutine populate_mesh(x, y, z, mesh, bds_mv)
	
		IMPLICIT NONE
		REAL, DIMENSION(:), ALLOCATABLE :: x, y, z, bds_mv
		TYPE(Mesh_t), DIMENSION(:,:,:), ALLOCATABLE :: mesh
		TYPE(Linked_List), POINTER :: new_ll
		INTEGER :: i, j_x, j_y, j_z
		
		!write(*,*)'Populating the mesh'
		!Convert the position of the particle into the coordinates of the 3D mesh
		do i = 1,npart,1
			j_x = floor(abs(x(i) - (bds_mv(1)+0.0001))/mesh_subsize) + 1
			j_y = floor(abs(y(i) - (bds_mv(2)+0.0001))/mesh_subsize) + 1
			j_z = floor(abs(z(i) - (bds_mv(3)+0.0001))/mesh_subsize) + 1
			call ll_insert(mesh(j_x,j_y,j_z)%p, (/x(i),y(i),z(i)/), i)
		end do
	
	end subroutine populate_mesh
	
	
	!Determines the bounds of the domain decomposition of the mesh
	subroutine determine_decomposition(domain_start, domain_end)
	
		IMPLICIT NONE
		INTEGER, DIMENSION(:), ALLOCATABLE :: domain_start, domain_end
		INTEGER :: i, b
		
		b = num_cells/mpi_size
		do i=1,mpi_size
			domain_start(i) = (i-1)*b + 1
			domain_end(i) = i*b
		end do 
	
	end subroutine determine_decomposition
	
	
	!Determine the dimensions of the cubic mesh based on the maximum separation distance
	!of the particles' x, y and z coordinates
	subroutine determine_mesh_bounds(x, y, z, bounds_arr, bounds_minvals)
	
		REAL, DIMENSION(:), ALLOCATABLE, INTENT(IN) :: x, y, z
		REAL, DIMENSION(:), ALLOCATABLE :: bounds_arr, bounds_minvals
	
		!write(*,*)'Calculating dimensions for the mesh'
		!Store the smallest x, y, z positions
		bounds_minvals(1) = MINVAL(x)
		bounds_minvals(2) = MINVAL(y)
		bounds_minvals(3) = MINVAL(z)
		!Determine the maximum x, y, z separation between the particles
		bounds_arr(1) = ABS(MAXVAL(x)-bounds_minvals(1))
		bounds_arr(2) = ABS(MAXVAL(y)-bounds_minvals(2))
		bounds_arr(3) = ABS(MAXVAL(z)-bounds_minvals(3))
	
	end subroutine determine_mesh_bounds
	
	
	!Initialise the cubic mesh by assigning linked lists to each pointer
	subroutine init_mesh(mesh)
		
		IMPLICIT NONE
		TYPE(Mesh_t), DIMENSION(:,:,:), ALLOCATABLE :: mesh
		TYPE(Linked_List), POINTER :: new_ll
		INTEGER :: i,j,k
		
		do k = 1,num_cells,1
			do j = 1,num_cells,1
				do i = 1,num_cells,1
					!Create an empty linked list (head node)
					call ll_init(new_ll, (/0.0,0.0,0.0/), 0)
					mesh(i,j,k)%p => new_ll
				end do
			end do
		end do
	
	end subroutine init_mesh
	
	
	!Initialise the array of neighbours by assigning linked lists to each pointer
	subroutine init_neighbours(neighbours)
	
		IMPLICIT NONE
		TYPE(Mesh_t), DIMENSION(:), ALLOCATABLE :: neighbours
		TYPE(Linked_List), POINTER :: new_ll
		INTEGER :: i
		
		do i = 1,npart,1
			call ll_init(new_ll, (/0.0,0.0,0.0/), 0)
			neighbours(i)%p => new_ll
		end do
		
	end subroutine init_neighbours
	
	
	!Releases (deallocates) all memory assigned to the linked lists in the cubic mesh
	!Note that the mesh must be reinitialised after calling this
	subroutine free_mesh(mesh)
	
		IMPLICIT NONE
		TYPE(Mesh_t), DIMENSION(:,:,:), ALLOCATABLE :: mesh
		INTEGER :: i,j,k
		
		do k = 1,num_cells,1
			do j = 1,num_cells,1
				do i = 1,num_cells,1
					call ll_free(mesh(i,j,k)%p)
					nullify(mesh(i,j,k)%p)
				end do
			end do
		end do
	
	end subroutine free_mesh
	
	
	!Releases (deallocates) all memory assigned to the linked lists in the 1D
	!neighbours array.  Note neighbours must be reinitialised after calling this
	subroutine free_neighbours(neighbours)
	
		IMPLICIT NONE
		TYPE(Mesh_t), DIMENSION(:), ALLOCATABLE :: neighbours
		INTEGER :: i
		
		do i = 1,size(neighbours),1
			call ll_free(neighbours(i)%p)
			nullify(neighbours(i)%p)
		end do
		
	end subroutine free_neighbours
	
	
	!Determines an estimate for the density based on the population of the cubes
	!in the mesh, and the distance of the center of the cubes from the origin
	subroutine get_density_estimate(mesh)
	
		IMPLICIT NONE
		TYPE(Mesh_t), DIMENSION(:,:,:), ALLOCATABLE, INTENT(IN) :: mesh
		INTEGER :: i,j,k,s
		
		open(unit=2,file='cube_density.dat',action='write')
		do i=1,num_cells,1
			do j=1,num_cells,1
				do k=1,num_cells,1
					write(2,*) get_cube_distance(i,j,k), ll_size(mesh(i,j,k)%p)
				end do
			end do
		end do
		close(2)

	end subroutine get_density_estimate
	
	
	!Determines the distance between each particle and the origin
	subroutine get_radii(x, y, z, radii)
	
		IMPLICIT NONE
		REAL, DIMENSION(:), ALLOCATABLE, INTENT(IN) :: x, y, z
		REAL, DIMENSION(:), ALLOCATABLE :: radii
		INTEGER :: i
		
		do i = 1,npart,1
			radii(i) = get_distance((/x(i),y(i),z(i)/), (/0.0,0.0,0.0/))
		end do
	
	end subroutine get_radii


	!Reads in the GADGET binary file, following the format as specified in the brief		
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
	
	
	!Reads in a file containing mock positional data based on the Poisson distribution
	subroutine read_mock_file(x, y, z)
	
		IMPLICIT NONE
		REAL, DIMENSION(:), ALLOCATABLE :: x,y,z
		
		open(1,file='test_data.dat',action='read')
		do i=1,npart,1
			read(1,*,end=200)x(i),y(i),z(i)
		end do
200		continue
		close(1)
	
	end subroutine read_mock_file
	
	
	!Returns the result of the cubic spline kernel
	function w_kernel(r, h) result(w)
	
		REAL, INTENT(IN) :: r, h
		REAL :: w, x, y
		
		x = r / h
		if ((x.GT.0).AND.(x.LE.1)) then
			w = 1 - (3/2)*x*x*(1-0.5*x)
		else if ((x.GT.1).AND.(x.LE.2)) then
			w = 0.25*(2-x)*(2-x)*(2-x)
		else
			w = 0
		end if
		
		y = 3.14159*h*h*h
		w = w / y
	
	end function w_kernel
	
	
	!Return the distance (3D Pythagorean) between two points
	function get_distance(p1, p2) result(dist)
	
		REAL, DIMENSION(3), INTENT(IN) :: p1, p2
		REAL :: dist, x, y, z
		
		x = p1(1) - p2(1); y = p1(2) - p2(2); z = p1(3) - p2(3)
		
		dist = SQRT(x*x + y*y + z*z)
	
	end function get_distance
	
	
	!Returns the distance from the center of the (i,j,k)th cube to the origin
	function get_cube_distance(i,j,k) result(dist)
	
		INTEGER, INTENT(IN) :: i,j,k
		REAL :: x, y, z, dist
		
		x = bounds_minvals(1) + (i-0.5)*mesh_subsize
		y = bounds_minvals(2) + (j-0.5)*mesh_subsize
		z = bounds_minvals(3) + (k-0.5)*mesh_subsize
		dist = get_distance((/x, y, z/), (/0.0,0.0,0.0/))
	
	end function get_cube_distance
	
	
	!Prints the size of each linked list in the 3D cubic mesh
	subroutine print_mesh_sizes(mesh)
	
		IMPLICIT NONE
		TYPE(Mesh_t), DIMENSION(:,:,:), ALLOCATABLE, INTENT(IN) :: mesh
		INTEGER :: i, j, k, s, s_total
		
		!open(unit=1,file='mesh_out.txt',action='write')
		s_total = 0
		do k = 1,num_cells,1
			do j = 1,num_cells,1
				do i = 1,num_cells,1
					s = ll_size(mesh(i,j,k)%p)
					write(*,*)'Mesh:',i,j,k,'size:',s
					s_total = s_total + s
				end do
			end do
		end do
		write(*,*)'Total:',s_total
		!close(1)
	
	end subroutine print_mesh_sizes
	
	
	!Prints the size of each linked list in the 1D neighbours array, as well
	!as the number and proportion of neighbour lists of size num_neighbours
	subroutine print_neighbour_sizes(neighbours)
	
		IMPLICIT NONE
		TYPE(Mesh_t), DIMENSION(:), ALLOCATABLE, INTENT(IN) :: neighbours
		INTEGER :: i, s, s_total, n_max_total
		
		s_total = 0; n_max_total = 0
		do i = 1,size(neighbours),1
			s = ll_size(neighbours(i)%p)
			write(*,*)'Neighbours',i,'size:',s
			if (s == num_neighbours) then
				n_max_total = n_max_total + 1
			end if
			s_total = s_total + s
		end do
		write(*,*)'Total:',s_total
		write(*,*)'Total with max neighbours:',n_max_total
		write(*,*)'Proportion with max neighbours:',n_max_total / size(neighbours)
		
	end subroutine print_neighbour_sizes
	
	!--------------------------------------------------------------
	!		LINKED_LIST SUBROUTINES AND FUNCTIONS
	!--------------------------------------------------------------
	
	!Initialises (allocates) space for a linked list node, and giving it initial position data
	subroutine ll_init(list, pos, id)
	
		TYPE(Linked_List), POINTER :: list
		REAL, DIMENSION(3), INTENT(IN) :: pos
		INTEGER, INTENT(IN) :: id
		
		allocate(list)
		nullify(list%next)
		list%pos = pos; list%id = id;
	
	end subroutine ll_init
	
	
	!Frees the nodes contained within a linked list
	subroutine ll_free(list)
		
		TYPE(Linked_List), POINTER :: list, current, next
		
		current => list
		!Traverse the entire list
		do while (associated(current))
			next => current%next
			deallocate(current)
			nullify(current)
			current => next
		end do
		
	end subroutine ll_free
	
	
	!Inserts a node into the given linked list with the given data
	subroutine ll_insert(list, pos, id)
	
		TYPE(Linked_List), POINTER, INTENT(IN) :: list
		TYPE(Linked_List), POINTER :: current, next
		REAL, DIMENSION(3), INTENT(IN) :: pos
		INTEGER, INTENT(IN) :: id
		
		current => list
		!Traverse to the end of the list
		do while (associated(current%next))
			current => current%next
		end do
		
		call ll_init(next, pos, id)
		current%next => next
	
	end subroutine ll_insert
	
	
	!Insert all the nodes in list2 into list1 (except the head node of list2)
	subroutine ll_append(list1, list2)
	
		TYPE(Linked_List), POINTER, INTENT(IN) :: list2
		TYPE(Linked_List), POINTER :: list1, current
		
		current => list2
		do while (associated(current%next))
			current => current%next
			call ll_insert(list1, current%pos, current%id)
		end do
	
	end subroutine ll_append
	
	
	!Prints the contents of a linked list
	subroutine ll_print(list)
		
		IMPLICIT NONE
		TYPE(Linked_List), POINTER, INTENT(IN) :: list
		TYPE(Linked_List), POINTER :: current
		INTEGER :: i,c
		
		current => list
		c = 0
		do while (associated(current%next))
			current => current%next
			c = c + 1
			write(*,*) 'Item:',c
			write(*,*) (current%pos(i),i=1,3)
			write(*,*) current%id
			write(*,*) ''
		end do
		
	end subroutine ll_print
	
	
	!Returns the size of the linked list (except the head node)
	!i.e the number of nodes in the linked list
	function ll_size(list) result(size)
	
		TYPE(Linked_List), POINTER, INTENT(IN) :: list
		TYPE(Linked_List), POINTER :: current
		INTEGER :: size
		
		size = 0 !Ignore head node
		current => list
		
		do while (associated(current%next))
			current => current%next
			size = size + 1
		end do
	
	end function ll_size
	

end program halo
