//Newton-Raphson method

public class Exercise2 {
	
	//vals_prev is initialised to 0.1 to avoid conflicting with main loop in solve()
	double[] vals = {0,0}; double[] vals_prev = {0.1,0.1}; double tolerance = 0.0000001;
	int count = 0;
	//Create an object of the MatrixMethods class
	MatrixMethods m = new MatrixMethods();
	
	//Constructor to create object
	public Exercise2() {}
	
	public void solve() {
		//Loop while the difference in one of the values is above the tolerance
		while (Math.abs(vals[0] - vals_prev[0]) > tolerance) {
			
			//Update old values with current values
			vals_prev[0] = vals[0]; vals_prev[1] = vals[1];
			
			//Calculate $H^{-1}(x)\nabla f(x)$
			double[] calc = m.multMatrixVector(m.inverse2x2Matrix(getHessian(vals)),getGrad(vals));
			
			//Update new values
			vals[0] = vals[0] - calc[0]; vals[1] = vals[1] - calc[1];
			
			count++;
			System.out.println("Iteration: " + count + "\t x = " + vals[0] + ", y = " + vals[1]);
		}
		System.out.println("\nFound minimum of " + function(vals) + " at x = " + vals[0] + " and y = " + vals[1]);
		System.out.println("Solution found in " + count + " iterations.");
	}
	
	public double[][] getHessian(double[] v) {
		//Declare new 2x2 matrix
		double[][] result = new double[2][2];
		//Populate the matrix with the Hessian values
		result[0][0] = 12*Math.pow(v[0], 2) - 24*v[0] + 16;
		result[0][1] = 4;
		result[1][0] = 4;
		result[1][1] = 4;
		return result;
	}
	
	public double[] getGrad(double[] v) {
		//Define x, y for convenience
		double x = v[0]; double y = v[1];
		//Declare new array of size 2
		double[] result = new double[2];
		//Populate this array with the required values
		result[0] = 4*Math.pow(x, 3) - 12*Math.pow(x, 2) + 16*x + 4*y;
		result[1] = 4*y + 4*x + 4;
		return result;
	}
	
	//Return f(x,y)
	public double function(double[] v) {
		double x = v[0]; double y = v[1];
		return (Math.pow(x, 4) - 4*Math.pow(x, 3) + 8*Math.pow(x, 2) + 4*x*y + 2*Math.pow(y, 2) + 4*y + 6);
	}

	public static void main(String[] args) {
		Exercise2 e = new Exercise2();
		e.solve();
	}

}
