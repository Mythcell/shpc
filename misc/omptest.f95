!
!	compile with:	gfortran -fopenmp -o omptest omptest.f95
!
program otest

	USE OMP_LIB !For OpenMP
	
	IMPLICIT NONE
	INTEGER, DIMENSION(:,:), ALLOCATABLE :: arr
	INTEGER :: a, i, j, nthreads, tid, a_s
	
	nthreads = 8; a_s = 24
	allocate(arr(a_s,a_s))
	
	call OMP_SET_NUM_THREADS(nthreads)
	
	!$OMP PARALLEL DO
	do i=1,a_s,1
		!$OMP PARALLEL DO PRIVATE(tid, a)
		do j=1,a_s,1
			a = i+j
			arr(i,j) = a
			tid = OMP_GET_THREAD_NUM()
			write(*,*)'Thread: ',tid,'added',a,'to',i,j
		end do
		!$OMP END PARALLEL DO
	end do
	!$OMP END PARALLEL DO
	
	write(*,*)
	do i=1,a_s,1
		write(*,*)(arr(i,j),j=1,a_s,1)
	end do

end program otest
