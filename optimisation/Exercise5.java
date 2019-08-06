//Sequential-Quadratic

public class Exercise5 {
	
	double[] x = {0,2}; double[] dx = {0,0};
	MatrixMethods m = new MatrixMethods();
	
	public Exercise5() {}
	
	public void solve() {
		findInitialValues();
		findDeltaX();
		
		double scale = 0.75;
		//double scale = 1;
		updateX(scale);
		
		checkFandH();
	}
	
	public void updateX(double s) {
		x[0] = x[0] + s*dx[0];
		x[1] = x[1] + s*dx[1];
	}
	
	public void findDeltaX() {
		dx[0] = 62.0/53.0;
		dx[1] = -(58.0/53.0);
	}
	
	public void findInitialValues() {
		System.out.println("Initial x:");
		m.vectorToString(x);
		System.out.println("f(x) = " + f(x));
		System.out.println("grad(f):");
		m.vectorToString(fGrad(x));
		System.out.println("h(x) = " + h(x));
		System.out.println("grad(h):");
		m.vectorToString(hGrad(x));
	}
	
	public void checkFandH() {
		System.out.println("\nUpdated x:");
		m.vectorToString(x);
		System.out.println("New f(x1) = " + f(x));
		System.out.println("New h(x1) = " + h(x));
		if (h(x) > 0) {
			System.err.println("Constraint broken!");
		} else {
			System.out.println("Constraint satisfied.");
		}
	}
	
	public double f(double[] v) {
		double x = v[0]; double y = v[1];
		return (Math.pow(x, 4) - 2*Math.pow(x, 2)*y + Math.pow(x, 2) + Math.pow(y, 2) - 2*x + 1);
	}
	
	public double[] fGrad(double[] v) {
		double x = v[0]; double y = v[1];
		double[] result = new double[2];
		result[0] = 4*Math.pow(x, 3) - 4*x*y + 2*x - 2;
		result[1] = -2*Math.pow(x, 2) + 2*y;
		return result;
	}
	
	public double h(double[] v) {
		double x = v[0]; double y = v[1];
		return (4*Math.pow(x, 2) - Math.pow(y, 2) + 2*x - 3*y);
	}
	
	public double[] hGrad(double[] v) {
		double x = v[0]; double y = v[1];
		double[] result = new double[2];
		result[0] = 8*x + 2;
		result[1] = -2*y - 3;
		return result;
	}

	public static void main(String[] args) {
		Exercise5 e = new Exercise5();
		e.solve();
	}

}
