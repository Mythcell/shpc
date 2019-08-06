//Conjugate-Gradient

public class Exercise4 {
	
	double[] x = {1,2,0}; double[] d = {0,0,0}; double[] g = {0,0,0};
	double[][] Q = new double[3][3];
	MatrixMethods m = new MatrixMethods();
	int steps = 3;
	
	public Exercise4() {}
	
	public void solve() {
		//First determine d_0 and Q
		findDvector();
		findQ();
		//Main loop for iteration
		for (int n = 0; n < steps; n++) {
			
			//Perform the iteration for x
			findGvector();
			double a = findAlpha();
			updateXvector(a);
			
			//Perform the iteration for d
			findGvector(); //Need to update g as beta relies on new g values
			double b = findBeta();
			updateDvector(b);

			System.out.println("Iteration: " + (n+1) + "\nalpha = " + a + ", beta = " + b);
			System.out.println("x: " + x[0] + ", " + x[1] + ", " + x[2]);
			System.out.println("min: " + function(x) + "\n");
		}
		System.out.println("\nFound minimum of " + function(x) + " after " + steps + " iterations");
		System.out.println(" at x = " + x[0] + ", y = " + x[1] + " and z = " + x[2]);
	}
	
	public void updateXvector(double alpha) {
		for (int i = 0; i < x.length; i++) {
			x[i] = x[i] + alpha*d[i];
		}
	}
	
	public void updateDvector(double beta) {
		for (int i = 0; i < d.length; i++) {
			d[i] = -g[i] + beta*d[i];
		}
	}
	
	public double findAlpha() {
		double top = m.dotProduct(g, d);
		double bottom = m.dotProduct(d, m.multMatrixVector(Q, d));
		return (-(top/bottom));
	}
	
	public double findBeta() {
		double top = m.dotProduct(g, m.multMatrixVector(Q, d));
		double bottom = m.dotProduct(d, m.multMatrixVector(Q, d));
		return (top/bottom);
	}
	
	public void findDvector() {
		double[] values = fGrad(x);
		for (int i = 0; i < values.length; i++) {
			d[i] = -values[i];
		}
	}
	
	public void findGvector() {g = fGrad(x);}
	
	//Set the values of the Q matrix
	public void findQ() {
		Q[0][0] = 8; Q[0][1] = 3; Q[0][2] = -6;
		Q[1][0] = 3; Q[1][1] = 4; Q[1][2] = -3;
		Q[2][0] = -6; Q[2][1] = -3; Q[2][2] = 12;
	}
	
	//Determine the gradient vector for the given values of x
	public double[] fGrad(double[] v) {
		double x = v[0]; double y = v[1]; double z = v[2];
		double[] grad = new double[3];
		grad[0] = 8*x + 3*y - 6*z + 4;
		grad[1] = 3*x + 4*y - 3*z - 3;
		grad[2] = -6*x - 3*y + 12*z + 2;
		return grad;
	}
	
	//Evaluate the function
	public double function(double[] v) {
		double x = v[0]; double y = v[1]; double z = v[2];
		return (4*Math.pow(x, 2) + 2*Math.pow(y, 2) + 6*Math.pow(z, 2) + 3*x*y - 6*x*z - 3*y*z + 4*x - 3*y + 2*z + 2);
	}

	public static void main(String[] args) {
		Exercise4 e = new Exercise4();
		e.solve();
	}

}
