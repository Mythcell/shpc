//Newton-Rahpson Steepest Descent

public class Exercise3 {
	
	double[] vals = {0.5,0}; double[] vals_prev = {0.1,0.1}; double tolerance = 0.0001;
	double[] d = {0,0}; double h = 0.0001;
	int count = 0;
	MatrixMethods m = new MatrixMethods();
	
	public Exercise3() {}
	
	public void solve() {
		System.out.println("Initial x and y: " + vals[0] + ", " + vals[1]);
		//Enter main iterative loop (x_{k+1} = x_k + a_k d_k)
		while (Math.abs(vals[0] - vals_prev[0]) > tolerance) {
			vals_prev[0] = vals[0]; vals_prev[1] = vals[1];
			
			//Determine the d vector
			determine_dvalues();
			
			//Find the value of alpha that minimises f(x + ad)
			double a = findAlpha();
			System.out.println("Found alpha = " + a);
			
			//Update values
			vals[0] = vals[0] + a*d[0]; vals[1] = vals[1] + a*d[1];
			count++;
			System.out.println("New x and y: " + vals[0] + ", " + vals[1]);
		}
		double minimum = function(vals);
		System.out.println("\nFound minimum of " + minimum + "\n at x = " + vals[0] + " and y = " + vals[1]);
		System.out.println("Solution found in " + count + " iterations.");
	}
	
	public double findAlpha() {
		//Define some arbitrary initial value to begin the search
		double a = 1; double a_prev = 0;
		//Perform the Newton-Raphson method for the line search
		while (Math.abs(a - a_prev) > tolerance) {
			a_prev = a;
			a = a - (fprime(a)/fprimeprime(a));
		}
		return a;
	}
	
	//Determine the d vector for the current x values
	public void determine_dvalues() {
		double[] values = fGrad(vals);
		d[0] = -values[0]; d[1] = -values[1];
	}
	
	//Approximate f''(a)
	public double fprimeprime(double a) {
		return (fprime(a+h) - fprime(a))/h;
	}
	
	//Approximate f'(a)
	public double fprime(double a) {
		return (function(a+h) - function(a))/h;
	}

	//Determine the gradient of f(x,y)
	public double[] fGrad(double[] v) {
		double x = v[0]; double y = v[1];
		double[] result = new double[2];
		result[0] = 8*x - 3*Math.pow(y, 2) + 5;
		result[1] = 8*Math.pow(y, 3) - 6*x*y - 2;
		return result;
	}
	
	//Implicitly determine the function for the given value of a
	public double function(double a) {
		double[] values = {vals[0] + a*d[0],vals[1] + a*d[1]};
		return function(values);
	}
	
	//Explicitly determine the function for the given x, y values
	public double function(double[] v) {
		double x = v[0]; double y = v[1];
		return (4*Math.pow(x, 2) + 2*Math.pow(y, 4) - 3*x*Math.pow(y, 2) + 5*x - 2*y);
	}

	public static void main(String[] args) {
		Exercise3 e = new Exercise3();
		e.solve();
	}

}
