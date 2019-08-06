!Program to generate test data for an NFW profile

program fit

	IMPLICIT NONE
	
	REAL :: p, p_c, d_c, r, r_s, x, scale
	
	!standard fit, d_c = 2800, r_s = 0.25
	!1-cube fit, d_c = 600, r_s = 0.25
	!cube_density estimate, d_c = 100, r_s = 0.14
	p_c = 1; d_c = 600; r_s = 0.25;
	r = 0.0002
	
	open(unit=1,file='nfw_t.dat',action='write')
	do while (r < 1)
		x = r/r_s
		p = p_c*d_c / (x*(1+x)*(1+x))
		write(1,*)r, p
		r = r + 0.0002
	end do
	close(1)

end program fit