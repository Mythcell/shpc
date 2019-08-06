//Newton-Raphson method

public class Exercise1 {
	
	//Initialise variables
	double xprev = 0; double x = 10; double tolerance = 0.0000001;
	int count = 0;
	
	//Constructor to create object
	public Exercise1() {}
	
	public void solve() {
		//Keep calculating until the difference in successive terms is less than the tolerance
		while (Math.abs(x - xprev) > tolerance) {
			xprev = x;
			x = x - (fprime(x)/fprimeprime(x));
			count++;
			System.out.println("Iteration: " + count + "\t x: " + x + ", xprev: " + xprev);
		}
		System.out.println("\nFound minimum of " + function(x) + " at x = " + x);
		System.out.println("Solution found in " + count + " iterations.");
	}
	
	//Get the value of f(x)
	public double function(double x) {
		return (Math.pow(x, 5) - 3*Math.pow(x, 4) - 5*Math.pow(x, 3) + 15*Math.pow(x, 2) + 4*x - 12);
	}
	
	//Get the value of f'(x)
	public double fprime(double x) {
		return (5*Math.pow(x, 4) - 12*Math.pow(x, 3) - 15*Math.pow(x, 2) + 30*x + 4);
	}
	
	//Get the value of f''(x)
	public double fprimeprime(double x) {
		return (20*Math.pow(x, 3) - 36*Math.pow(x, 2) - 30*x + 30);
	}

	public static void main(String[] args) {
		Exercise1 e = new Exercise1();
		e.solve();
	}

}
