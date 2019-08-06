!This program simulates a charged particle moving in the x-y plane in the presence
!of a magnetic field in the z direction.

program ex5

implicit none

!Declare variables
!sx - position in the x direction
!sy - position in the y direction
!vx - velocity in the x direction
!vy - velocity in the y direction
!q - charge of the particle
!m - mass of the particle
!b - magnetic field strength
!t - time
!dt - time step
!te - total time to be evolved
!ox - x coordinate of particle's start position
!oy - y coordinate of particle's end position
!i - count variable for main loop
!tstep - number of total iterations
real :: sx, sy, vx, vy, q, m, b, t, dt, te, ox, oy
integer :: i, tstep

!The input file must be of the following format:
![sx], [sy], [vx], [vy], [te], [dt], [m], [q], [b]
!i.e a single line of 9 comma-delimited numbers
!e.g 1, 1, 1, 1, 2, 0.005, 0.5, 1, 1
open(unit=1,file='input.txt',action='read')

!Attempt to determine parameters from file 'input.txt'
read(1,*) sx, sy, vx, vy, te, dt, m, q, b
close(1)

!Record initial position
ox = sx
oy = sy

!Set other variables
t = 0
!Calculate the number of required iterations for the given evolution time
tstep = te/dt

!Open two files to record data on position and velocity respectively
!Position output is: [time] [x position] [y position] [distance from start position]
!Velocity output is: [time] [x velocity] [y velocity] [speed]
open(unit=1,file='position.dat')
open(unit=2,file='velocity.dat')

do i = 1, tstep
	
	!Update time
	t = t + dt
	
	!Update velocities
	vx = vx + (q/m)*vy*b*dt
	vy = vy - (q/m)*vx*b*dt

	!Update positions
	sx = sx + vx*dt + 0.5*(q/m)*vy*b*(dt**2)
	sy = sy + vy*dt - 0.5*(q/m)*vx*b*(dt**2)
	
	!Write data to files
	write(1,*) t,sx,sy,SQRT((sx-ox)**2 + (sy-oy)**2)
	write(2,*) t,vx,vy,SQRT(vx**2 + vy**2)

end do

!Close files as we are finished
close(1)
close(2)

end program ex5

