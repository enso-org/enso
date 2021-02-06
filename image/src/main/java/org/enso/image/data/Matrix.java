package org.enso.image.data;

import org.opencv.core.CvType;
import org.opencv.core.Mat;

public class Matrix {

  // type depth constants
  public static final int
      CV_8U = CvType.CV_8U,
      CV_8UC1 = CvType.CV_8UC1,
      CV_8S = CvType.CV_8S,
      CV_16U = CvType.CV_16U,
      CV_16S = CvType.CV_16S,
      CV_32S = CvType.CV_32S,
      CV_32F = CvType.CV_32F,
      CV_64F = CvType.CV_64F,
      CV_16F = CvType.CV_16F;

  public static int CV_8UC(int channels) {
    return CvType.CV_8UC(channels);
  }

  public static Mat zeros(Mat m) {
    return Mat.zeros(m.size(), m.type());
  }

  public static Mat zeros(int rows, int cols, int type) {
    return Mat.zeros(rows, cols, type);
  }

  public static Mat ones(int rows, int cols, int type) {
    return Mat.ones(rows, cols, type);
  }

  public static Mat eye(int rows, int cols, int type) {
    return Mat.eye(rows, cols, type);
  }

  public static Object to_vector(Mat mat) {
    switch (dataSize(mat.type())) {
      case CvType.CV_8U:
      case CvType.CV_8S:
        byte[] buf8 = new byte[(int) mat.total() * mat.channels()];
        mat.get(0, 0, buf8);
        return buf8;
      case CvType.CV_16U:
      case CvType.CV_16S:
        short[] buf16 = new short[(int) mat.total() * mat.channels()];
        mat.get(0, 0, buf16);
        return buf16;
      case CvType.CV_32S:
        int[] buf32s = new int[(int) mat.total() * mat.channels()];
        mat.get(0, 0, buf32s);
        return buf32s;
      case CvType.CV_32F:
        float[] buf32f = new float[(int) mat.total() * mat.channels()];
        mat.get(0, 0, buf32f);
        return buf32f;
      case CvType.CV_64F:
        double[] buf64f = new double[(int) mat.total() * mat.channels()];
        mat.get(0, 0, buf64f);
        return buf64f;
      case CvType.CV_16F:
        short[] buf16f = new short[(int) mat.total() * mat.channels()];
        mat.get(0, 0, buf16f);
        return buf16f;
    }
    return null;
  }

  private static int dataSize(int type) {
    return CvType.ELEM_SIZE(type) / CvType.channels(type);
  }
}
