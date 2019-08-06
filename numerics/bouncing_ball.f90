! Use the Euler method to compute the trajectory of a bouncing
! ball assuming perfect reflection at the surface x = 0.
! Use SI units (meters and seconds)

program bouncing_balls
implicit none

integer :: steps
real :: x, v, g, t, dt

x = 1.0	! initial height of the ball
v = 0.0	! initial velocity of the ball
g = 9.8	! gravitational acceleration
t = 0.0	! initial time
dt = 0.001	! size of time step

open(10,file='bounce_inelastic50.dat')   ! open data file

do steps = 1, 3000    ! loop for 300 timesteps

  t = t + dt
  v = v - g*dt ! Update velocity first
  x = x + v*dt ! Then update position

  if(x.lt.0) then		! reflect the motion of the ball
    x = -x			! when it hits the surface x=0
    v = -0.5*v
  endif
  write(10,*) t, x, v	! write out data at each time step

enddo

end
