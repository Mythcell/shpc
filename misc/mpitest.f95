program test

	USE MPI !for MPI
	IMPLICIT NONE
	
	integer :: rank, size, pto, from, tag, count, i, ierr, source, dest
	integer :: st_source, st_tag, st_count
	integer :: status(MPI_STATUS_SIZE)
	double precision, dimension(10) :: mdata

	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
	write(*,*)'process',rank,'of',size,'is alive'
	
	dest = size - 1; source = 0
	
	if (rank.EQ.0) then
		do i=1,10
			mdata(i) = i
		end do
	
		call MPI_SEND(mdata, 10, MPI_DOUBLE_PRECISION, dest, 1000, MPI_COMM_WORLD, ierr)
		write(*,*)'Sending data from',rank,'to',dest
		
	else if (rank.EQ.dest) then
	
		tag = MPI_ANY_TAG; source = MPI_ANY_SOURCE
		
		call MPI_RECV(mdata, 10, MPI_DOUBLE_PRECISION, source, tag, MPI_COMM_WORLD, status, ierr)
		write(*,*)'Process',rank,'received data from',source+rank
		
		call MPI_GET_COUNT(status, MPI_DOUBLE_PRECISION, st_count, ierr)
		
		st_source = status(MPI_SOURCE)
		st_tag = status(MPI_TAG)
		
		print*,'status info: source = ',st_source,'tag = ',st_tag,'count = ',st_count
		write(*,*)(mdata(i),i=1,10)
		
	end if
	
	call MPI_FINALIZE(ierr)

end program test