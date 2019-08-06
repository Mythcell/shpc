/**
 * A class containing various, useful methods for matrix operations
 * and some vector operations.
 * Designed for the SHPC4001 Optimisation Assignment
 * 
 * @author Mitchell Cavanagh, 21727193
 */
public class MatrixMethods {
	
	public MatrixMethods() {}
	
	/**
	 * Multiplies two matrices together using matrix multiplication.
	 * The two matrices must have compatible dimensions.
	 * @param matrix1 An m x n matrix
	 * @param matrix2 An n x j matrix
	 * @return An m x j matrix with the result of the multiplication,
	 * or null if the multiplication cannot be done.
	 */
	public double[][] multMatrixMatrix(double[][] matrix1, double[][] matrix2) {
		
		if (matrix1[0].length != matrix2.length) {
			System.err.println("Incorrect dimensions in multMatrixMatrix()");
			return null;
		}
		
		double[][] result = new double[matrix1.length][matrix2[0].length];
		
		for (int i = 0; i < matrix1.length; i++) {
			for (int j = 0; j < matrix2[0].length; j++) {
				double value = 0;
				for (int k = 0; k < matrix1[0].length; k++) {
					value += matrix1[i][k]*matrix2[k][j];
				}
				result[i][j] = value;
			}
		}
		return result;
	}
	
	/**
	 * This method performs a matrix multiplication of an nxn matrix with a 1xn vector.
	 * If the operation cannot be performed the the method returns an error.
	 * @param matrix A 2 dimensional square array of doubles with dimensions nxn.
	 * @param vector A 1 dimensional array of doubles with dimensions 1xn.
	 * @return A 1 dimensional array of doubles corresponding to the result of the
	 * matrix multiplication, OR null if the operation cannot be performed.
	 */
	public double[] multMatrixVector(double[][] matrix, double[] vector) {
		
		if (matrix[0].length != vector.length) {
			System.err.println("Incorrect dimensions in multMatrixVector()");
			return null;
		}

		double[] result = new double[matrix.length];
		
		for (int i = 0; i < matrix.length; i++) {
			double value = 0;
			for (int j = 0; j < vector.length; j++) {
				value += matrix[i][j]*vector[j];
			}
			result[i] = value;
		}
		return result;
	}
	
	/**
	 * Determines the inverse of a 2x2 matrix.
	 * @param matrix A 2x2 matrix
	 * @return The inverse matrix, or null if the matrix is not a 2x2 matrix
	 * or if its determinant is 0
	 */
	public double[][] inverse2x2Matrix(double[][] matrix) {
		
		if ((matrix.length != 2) || (matrix[0].length) != 2) {
			System.err.println("inverse2x2Matrix requires a 2 x 2 matrix");
			return null;
		}
		double[][] result = new double[2][2];
		double det = determinant(matrix);
		
		if (det == 0) {
			System.err.println("inverse2x2Matrix - matrix is NOT invertible");
			return null;
		}
		
		result[0][0] = matrix[1][1]/det;
		result[1][1] = matrix[0][0]/det;
		result[0][1] = -matrix[0][1]/det;
		result[1][0] = -matrix[1][0]/det;
		
		return result;
	}
	
	/**
	 * Finds the determinant of a 2x2 matrix
	 * @param matrix A 2x2 matrix
	 * @return The determinant, or -1 if the matrix is not 2x2
	 */
	public double determinant(double[][] matrix) {
		
		if ((matrix.length != 2) || (matrix[0].length != 2)) {
			System.err.println("determinant requires a 2 x 2 matrix");
			return -1;
		}
		return (matrix[0][0]*matrix[1][1] - matrix[0][1]*matrix[1][0]);
	}
	
	/**
	 * Determines the dot product of two vectors.  The two vectors must have
	 * the same length.
	 * @param v1 The first vector
	 * @param v2 The second vector
	 * @return A single scalar with the result of the dot product,
	 * or -1 if the operation cannot be performed.
	 */
	public double dotProduct(double[] v1, double[] v2) {
		
		if (v1.length != v2.length) {
			System.err.println("Incorrect dimensions in dotProduct()");
			return -1;
		}
		
		double result = 0;
		for (int i = 0; i < v1.length; i++) {
			result += v1[i]*v2[i];
		}
		return result;
	}
	
	/**
	 * Helper method to print a matrix to stdout
	 * @param matrix The matrix to be printed
	 */
	public void matrixToString(double[][] matrix) {
		for (int i = 0; i < matrix.length; i++) {
			for (int j = 0; j < matrix[i].length; j++) {
				System.out.print(matrix[i][j] + " ");
			}
			System.out.println();
		}
	}
	
	/**
	 * Helper method to print a vector to stdout
	 * @param vector The vector to be printed
	 */
	public void vectorToString(double[] vector) {
		for (int i = 0; i < vector.length; i++) {
			System.out.print(vector[i] + " ");
		}
		System.out.println();
	}

}
