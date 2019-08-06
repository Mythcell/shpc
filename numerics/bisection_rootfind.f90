!This program will attempt to find the roots of a function
!using the Bisection Method.  The program searches within
!an interval [-bound,bound] iterating through equal
!subintervals with a width specified by the variable "step".
!If a root is detected to exist within a subinterval
!(i.e if f(a)*f(b) < 0 for a < b), then the program applies the
!Bisection Method algorithm to approximate the root.

program rootfind

implicit none

real :: a, b, step, f, froot
integer :: bound

!Set the interval in which to search for the root
bound = 2
!Set the size of subintervals
step = 0.001

!Initialise a and b (here a < b)
a = -bound
b = a + step

!Scan all subintervals to look if a root exists
do while (b <= bound)
	!Determine if a root exists in this subinterval
	if (f(a)*f(b) < 0) then
		write(*,*)"Found a root at: ", froot(a,b)
		!write(*,*)froot(a,b)
	end if
	!Iterate to the next subinterval
	a = a + step
	b = b + step
end do

end program rootfind

!This function applies the Bisection Method for root finding
function froot(a, b) result(r)
	
real, intent(in) :: a, b
real :: d, e, r, tolerance
tolerance = 0.000001

!Use these variables to avoid conflicts
d = a
e = b

!Bisect until the interval width is less than the tolerance
do while (e-d >= tolerance)
	!Determine midpoint of the interval
	r = 0.5*(d+e)
	!Adjust new interval bounds
	if (f(d)*f(r) > 0) then
		d = r
	else
		e = r
	end if
end do
!Root is thus approximated to lie in the middle of the
!last bisected interval.
r = 0.5*(d+e)

	return
end function froot

!This function evaluates a polynomial for the given value n
function f(n) result(x)

	real, intent(in) :: n
	real :: x
	integer :: fstep
	
	!Test function
	!x = n**3 + 11*n**2 - 2*n - 120
	
	!Evaluate f[x]
	x = n**3 - 2.2*n
	!Now evaluate f[f[f[f[x]]]]
	!Only need 3 steps as f[x] already determined
	do fstep = 1, 3
		x = x**3 - 2.2*x
	end do

	return
end function f
