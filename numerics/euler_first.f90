!Program demonstrating the use of the first-order Euler method

program ex1c

implicit none

real :: t, tau, dt, exact, numerical, error
integer :: n

tau = 5

open(unit=1,file="err1c.dat")

!Initially start with dt = 1.0 (will halve each time)
dt = 1.0
!Halve dt 20 times
do n = 1, 20

	t = 0
	numerical = 100
	error = 0

	!Perform the first-order approximation
	do while (t <= 25)
		exact = 100*exp(-(t/tau))
		numerical = numerical - numerical*(dt/tau)
		t = t + dt
	end do

	!Calculate error in the final t=25 values
	error=(numerical-exact)/exact
	write(1,*) dt,ABS(error)
	dt = dt*0.5

end do

close(1)

end program ex1c
