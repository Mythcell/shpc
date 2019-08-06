!Program demonstrating the use of the second-order (leapfrog) Euler method

program ex1d

implicit none

real :: t, tau, dt, exact, tplus, tcurrent, tminus, error
integer :: n

tau = 5

open(unit=1,file="err1d.dat")

!Initially start with dt = 1.0 (will halve each time)
dt = 1.0
!Halve dt 20 times
do n = 1, 20
	
	!Perform first-order approximation for T_{n+1} term
	t = 0
	error = 0
	tcurrent = 100
	exact = 100*exp(-(t/tau))
	tplus = tcurrent - tcurrent*(dt/tau)
	tminus = tcurrent
	tcurrent = tplus
	t = t + dt

	!Perform second-order approximation for remaining terms
	do while (t <= 25)
		exact = 100*exp(-(t/tau))
		tplus = tminus - 2*tcurrent*(dt/tau)
		tminus = tcurrent
		tcurrent = tplus
		t = t + dt
	end do

	!Calculate the error in the final t=25 values
	error = (tplus-exact)/exact
	write(1,*) dt, ABS(error)
	dt = dt*0.5

end do

close(1)

end program ex1d
