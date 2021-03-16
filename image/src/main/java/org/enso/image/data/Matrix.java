package org.enso.image.data;

import org.enso.opencv.OpenCV;
import org.opencv.core.*;
import org.opencv.imgcodecs.Imgcodecs;

import java.util.Base64;

public class Matrix {

  static {
    OpenCV.loadShared();
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
  }

  private static final byte MAX_SIGNED_BYTE = -1;
  private static final double MAX_UNSIGNED_BYTE = 255;

  private static final String EXTENSION_PNG = ".png";

  public static Mat create(Mat mat) {
    return Mat.zeros(mat.size(), mat.type());
  }

  public static Mat zeros(int rows, int cols, int channels) {
    return Mat.zeros(rows, cols, CvType.CV_8UC(channels));
  }

  public static Mat zeros(Mat m) {
    return Matrix.zeros(m.channels(), m.rows(), m.cols());
  }

  public static Mat ones(int rows, int cols, int channels) {
    byte[] bytes = new byte[channels * rows * cols];
    for (int i = 0; i < channels * rows * cols; i++) {
      bytes[i] = MAX_SIGNED_BYTE;
    }
    return new MatOfByte(bytes).reshape(channels, rows);
  }

  public static Mat eye(int rows, int cols, int channels) {
    Mat ones = Matrix.ones(rows, cols, channels);
    Mat eye = Mat.eye(rows, cols, CvType.CV_8UC(channels));
    Mat dst = Mat.zeros(eye.size(), eye.type());

    Core.multiply(ones, eye, dst);
    return dst;
  }

  public static Mat from_vector(double[] values, int channels, int rows) {
    byte[] bytes = new byte[values.length];
    for (int i = 0; i < values.length; i++) {
      bytes[i] = denormalize(values[i]);
    }
    return new MatOfByte(bytes).reshape(channels, rows);
  }

  public static Object to_vector(Mat mat) {
    byte[] data = new byte[(int) mat.total() * mat.channels()];
    mat.get(0, 0, data);
    return Matrix.normalize(data);
  }

  public static String to_base64(Mat image) {
    MatOfByte buf = new MatOfByte();
    Imgcodecs.imencode(EXTENSION_PNG, image, buf);
    byte[] bufData = new byte[(int) buf.total() * buf.channels()];
    buf.get(0, 0, bufData);
    return Base64.getEncoder().encodeToString(bufData);
  }

  public static boolean is_equals(Mat mat1, Mat mat2) {
    if (mat1.size().equals(mat2.size()) && mat1.type() == mat2.type()) {
      Mat dst = Mat.zeros(mat1.size(), mat1.type());
      Core.subtract(mat1, mat2, dst);
      return Core.sumElems(dst).equals(Scalar.all(0));
    }
    return false;
  }

  public static Object get(Mat mat, int row, int column) {
    byte[] data = new byte[mat.channels()];
    int[] idx = new int[] { row, column };

    mat.get(idx, data);
    return Matrix.normalize(data);
  }

  public static void add(Mat mat, Scalar scalar, Mat dst) {
    Core.add(mat, denormalize(scalar), dst);
  }

  public static void subtract(Mat mat, Scalar scalar, Mat dst) {
    Core.subtract(mat, denormalize(scalar), dst);
  }

  public static void multiply(Mat mat, Scalar scalar, Mat dst) {
    Core.multiply(mat, scalar, dst);
  }

  public static void divide(Mat mat, Scalar scalar, Mat dst) {
    Core.divide(mat, scalar, dst);
  }

  private static double[] normalize(byte[] bytes) {
    double[] buf = new double[bytes.length];
    for (int i = 0; i < bytes.length; i++) {
      buf[i] = Matrix.normalize(bytes[i]);
    }
    return buf;
  }

  private static double normalize(byte pixelValue) {
    return (pixelValue & 0xff) / MAX_UNSIGNED_BYTE;
  }

  private static Scalar denormalize(Scalar scalar) {
    return scalar.mul(Scalar.all(MAX_UNSIGNED_BYTE));
  }

  private static byte denormalize(double pixelValue) {
    return (byte) (pixelValue * MAX_UNSIGNED_BYTE);
  }

}
