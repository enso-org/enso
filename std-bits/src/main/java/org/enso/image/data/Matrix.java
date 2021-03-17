package org.enso.image.data;

import org.enso.image.opencv.OpenCV;
import org.opencv.core.*;
import org.opencv.imgcodecs.Imgcodecs;

import java.util.Base64;

/** Methods for operations on Matrix. */
public class Matrix {

  static {
    OpenCV.loadShared();
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
  }

  private static final byte MAX_SIGNED_BYTE = -1;
  private static final double MAX_UNSIGNED_BYTE = 255;

  private static final String EXTENSION_PNG = ".png";

  /**
   * Create a matrix filled with zeros.
   *
   * @param rows the number of rows.
   * @param cols the number of columns
   * @param channels the number of channels.
   * @return the zero matrix.
   */
  public static Mat zeros(int rows, int cols, int channels) {
    return Mat.zeros(rows, cols, CvType.CV_8UC(channels));
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
    byte[] bytes = new byte[channels * rows * cols];
    for (int i = 0; i < channels * rows * cols; i++) {
      bytes[i] = MAX_SIGNED_BYTE;
    }
    return new MatOfByte(bytes).reshape(channels, rows);
  }

  /**
   * Create an identity matrix.
   *
   * @param rows the number of rows.
   * @param cols the number of columns.
   * @param channels the number of channels.
   * @return the identity matrix.
   */
  public static Mat eye(int rows, int cols, int channels) {
    Mat ones = Matrix.ones(rows, cols, channels);
    Mat eye = Mat.eye(rows, cols, CvType.CV_8UC(channels));
    Mat dst = Mat.zeros(eye.size(), eye.type());

    Core.multiply(ones, eye, dst);
    return dst;
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
    byte[] bytes = new byte[values.length];
    for (int i = 0; i < values.length; i++) {
      bytes[i] = denormalize(values[i]);
    }
    return new MatOfByte(bytes).reshape(channels, rows);
  }

  /**
   * Get the values of a matris as an array.
   *
   * @param mat the matrix.
   * @return the array of the matrix values.
   */
  public static Object to_vector(Mat mat) {
    byte[] data = new byte[(int) mat.total() * mat.channels()];
    mat.get(0, 0, data);
    return Matrix.normalize(data);
  }

  /**
   * Encode the matrix into the base64 string.
   *
   * @param mat the matrix to encode.
   * @return the base64 string representing the matrix data.
   */
  public static String to_base64(Mat mat) {
    MatOfByte buf = new MatOfByte();
    Imgcodecs.imencode(EXTENSION_PNG, mat, buf);
    byte[] bufData = new byte[(int) buf.total() * buf.channels()];
    buf.get(0, 0, bufData);
    return Base64.getEncoder().encodeToString(bufData);
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
   * Get the matrices' element by the row and the column.
   *
   * @param mat the matrix.
   * @param row the row index.
   * @param column the column index.
   * @return the value located at the given row and the column of the matrix.
   */
  public static Object get(Mat mat, int row, int column) {
    byte[] data = new byte[mat.channels()];
    int[] idx = new int[] { row, column };

    mat.get(idx, data);
    return Matrix.normalize(data);
  }

  /**
   * Add the scalar to each element of the matrix.
   *
   * @param mat the matrix.
   * @param scalar the scalar to add.
   * @param dst the matrix holding the result of the operation.
   */
  public static void add(Mat mat, Scalar scalar, Mat dst) {
    Core.add(mat, denormalize(scalar), dst);
  }

  /**
   * Subtract the scalar from each element of the matrix.
   * @param mat the matrix.
   * @param scalar the scalar to subtract.
   * @param dst the matrix holding the result of the operation.
   */
  public static void subtract(Mat mat, Scalar scalar, Mat dst) {
    Core.subtract(mat, denormalize(scalar), dst);
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
   * Divite each element of the matrix by the scalar.
   *
   * @param mat the matrix
   * @param scalar the scalar to divide on.
   * @param dst the matrix holding the result of the operation.
   */
  public static void divide(Mat mat, Scalar scalar, Mat dst) {
    Core.divide(mat, scalar, dst);
  }

  /**
   * Normalize the byte array.
   *
   * @param bytes the byte array to normalize.
   * @return return normalized values in the range of 0.0 to 1.0.
   */
  private static double[] normalize(byte[] bytes) {
    double[] buf = new double[bytes.length];
    for (int i = 0; i < bytes.length; i++) {
      buf[i] = Matrix.normalize(bytes[i]);
    }
    return buf;
  }

  /**
   * Normalize the byte value.
   *
   * @param value the value to normalize.
   * @return return the normalized value in the range of [0.0 .. 1.0].
   */
  private static double normalize(byte value) {
    return (value & 0xff) / MAX_UNSIGNED_BYTE;
  }

  /**
   * Denormalize the value into the byte range.
   *
   * @param scalar the normalized scalar.
   * @return the scalar scaled from normalized range [0.0 .. 1.0] to a byte range.
   */
  private static Scalar denormalize(Scalar scalar) {
    return scalar.mul(Scalar.all(MAX_UNSIGNED_BYTE));
  }

  /**
   * Denormalized the value into a byte range.
   *
   * <p>The following holds: {@code denormalize(normalize(value)) == value}.
   *
   * @param value the normalized value
   * @return the value scaled from normalized range [0.0 .. 1.0] to a byte range.
   */
  private static byte denormalize(double value) {
    return (byte) (value * MAX_UNSIGNED_BYTE);
  }

}
