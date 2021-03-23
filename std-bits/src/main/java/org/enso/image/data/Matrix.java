package org.enso.image.data;

import org.enso.image.opencv.OpenCV;
import org.opencv.core.*;

/** Methods for standard library operations on Matrix. */
public class Matrix {

  static {
    OpenCV.loadShared();
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
  }

  /**
   * Create a matrix filled with zeros.
   *
   * @param rows the number of rows.
   * @param cols the number of columns
   * @param channels the number of channels.
   * @return the zero matrix.
   */
  public static Mat zeros(int rows, int cols, int channels) {
    return Mat.zeros(rows, cols, CvType.CV_64FC(channels));
  }

  /**
   * Create a matrix filled with zeros.
   *
   * @param m the matrix defining the shape.
   * @return the matrix of zeros with the same shape as the provided one.
   */
  public static Mat zeros(Mat m) {
    return Matrix.zeros(m.channels(), m.rows(), m.cols());
  }

  /**
   * Create a matrix filled with ones.
   *
   * @param rows the number of rows.
   * @param cols the number of columns.
   * @param channels the number of channels.
   * @return the matrix of ones.
   */
  public static Mat ones(int rows, int cols, int channels) {
    return Mat.ones(rows, cols, CvType.CV_64FC(channels));
  }

  /**
   * Create an identity matrix.
   *
   * @param rows the number of rows.
   * @param cols the number of columns.
   * @param channels the number of channels.
   * @return the identity matrix.
   */
  public static Mat identity(int rows, int cols, int channels) {
    return Mat.eye(rows, cols, CvType.CV_64FC(channels));
  }

  /**
   * Create a matrix from the vector.
   *
   * @param values the array of input values.
   * @param channels the number of channels.
   * @param rows the number of rows.
   * @return the new matrix created from array.
   */
  public static Mat from_vector(double[] values, int channels, int rows) {
    return new MatOfDouble(values).reshape(channels, rows);
  }

  /**
   * Get the values of a matrix as an array.
   *
   * @param mat the matrix.
   * @return the array of the matrix values.
   */
  public static Object to_vector(Mat mat) {
    double[] data = new double[(int) mat.total() * mat.channels()];
    mat.get(0, 0, data);
    return data;
  }

  /**
   * Compare two matrices for equality.
   *
   * @param mat1 the first matrix to compare.
   * @param mat2 the second matrix to compare.
   * @return {@code true} when two matrices contain the same elements and {@code false} otherwise.
   */
  public static boolean is_equals(Mat mat1, Mat mat2) {
    if (mat1.size().equals(mat2.size()) && mat1.type() == mat2.type()) {
      Mat dst = Mat.zeros(mat1.size(), mat1.type());
      Core.subtract(mat1, mat2, dst);
      return Core.sumElems(dst).equals(Scalar.all(0));
    }
    return false;
  }

  /**
   * Get the matrix element by the row and the column.
   *
   * @param mat the matrix.
   * @param row the row index.
   * @param column the column index.
   * @return the value located at the given row and the column of the matrix.
   */
  public static double[] get(Mat mat, int row, int column) {
    double[] data = new double[mat.channels()];
    int[] idx = new int[] { row, column };

    mat.get(idx, data);
    return data;
  }

  /**
   * Per-element addition of two matrices.
   *
   * @param mat1 the first matrix to add.
   * @param mat2 the second matrix to add.
   * @param dst the matrix holding the result of the operation.
   */
  public static void add(Mat mat1, Mat mat2, Mat dst) {
    Core.add(mat1, mat2, dst);
  }

  /**
   * Add the scalar to each element of the matrix.
   *
   * @param mat the matrix.
   * @param scalar the scalar to add.
   * @param dst the matrix holding the result of the operation.
   */
  public static void add(Mat mat, Scalar scalar, Mat dst) {
    Core.add(mat, scalar, dst);
  }

  /**
   * Per-element subtraction of two matrices.
   *
   * @param mat1 the first matrix to subtract from.
   * @param mat2 the second matrix to subtract.
   * @param dst the matrix holding the result of the operation.
   */
  public static void subtract(Mat mat1, Mat mat2, Mat dst) {
    Core.subtract(mat1, mat2, dst);
  }

  /**
   * Subtract the scalar from each element of the matrix.
   * @param mat the matrix.
   * @param scalar the scalar to subtract.
   * @param dst the matrix holding the result of the operation.
   */
  public static void subtract(Mat mat, Scalar scalar, Mat dst) {
    Core.subtract(mat, scalar, dst);
  }

  /**
   * Per-element multiplication of two matrices.
   *
   * @param mat1 the first matrix to multiply.
   * @param mat2 the second matrix to multiply.
   * @param dst the matrix holding the result of the operation.
   */
  public static void multiply(Mat mat1, Mat mat2, Mat dst) {
    Core.multiply(mat1, mat2, dst);
  }

  /**
   * Multiply the scalar with each element of the matrix.
   * @param mat the matrix.
   * @param scalar the scalar to multiply with.
   * @param dst the matrix holding the result of the operation.
   */
  public static void multiply(Mat mat, Scalar scalar, Mat dst) {
    Core.multiply(mat, scalar, dst);
  }

  /**
   * Per-element division of two matrices.
   *
   * @param mat1 the dividend matrix.
   * @param mat2 the divisor matrix.
   * @param dst the matrix holding the result of the operation.
   */
  public static void divide(Mat mat1, Mat mat2, Mat dst) {
    Core.divide(mat1, mat2, dst);
  }

  /**
   * Divite each element of the matrix by the scalar.
   *
   * @param mat the matrix.
   * @param scalar the scalar to divide on.
   * @param dst the matrix holding the result of the operation.
   */
  public static void divide(Mat mat, Scalar scalar, Mat dst) {
    Core.divide(mat, scalar, dst);
  }

  /**
   * Normalize the matrix into a range of [alpha .. beta] so that the minimum value of the matrix
   * becomes alpha and the maximum value of the matrix becomes beta.
   *
   * @param mat the matrix to normalize.
   * @param alpha the minimum value of the range.
   * @param beta the maximum value of the range.
   * @return the normalized matrix.
   */
  public static Mat normalize(Mat mat, double alpha, double beta) {
    Mat dst = new Mat();
    Core.normalize(mat, dst, alpha, beta, Core.NORM_MINMAX);
    return dst;
  }
}
