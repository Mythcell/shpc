!Test program to determine the value of exp(-5.5) via Taylor expansion

program ex3

implicit none

!Declare variables
real :: x, target, factorial, term, difference
integer :: i

!Initialise variables
x = -5.5
target = exp(x)
!Term is set to 1 (to correspond to first term in expansion)
term = 1
difference = 100
i = 1

write(*,*)'Target:',target

!Iterate partial sums until the value falls within the 1% accuracy bound
do while (difference > 1)
	term = term + (x**i)/factorial(i)
	difference = ABS(((target - term)/target)*100)
	i = i + 1
	write(*,*) term,difference,i
end do

write(*,*)'Steps taken:',i

end program ex3

!Function to determine the factorial of a number
function factorial(n) result(f)

	implicit none
	!n is to be interpreted as the function input parameter
	integer, intent(in) :: n
	integer :: i
	real :: f

	f = 1
	!Iterative method to work out the factorial
	do i=2,n
		f = f*i
	end do

end function factorial
