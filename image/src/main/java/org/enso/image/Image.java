package org.enso.image;

import org.enso.opencv.OpenCV;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfInt;
import org.opencv.imgcodecs.Imgcodecs;

public class Image {

  public static final int READ_FLAG_EMPTY = -127;

  static {
    OpenCV.loadShared();
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
  }

  public static class ReadFailedException extends Exception {

    public ReadFailedException(String path) {
      super("Failed to read " + path);
    }
  }

  public static class WriteFailedException extends Exception {

    public WriteFailedException(String path) {
      super("Failed to write " + path);
    }
  }

  public static Mat read(String path, int flags) throws ReadFailedException {
    Mat mat;
    if (flags == READ_FLAG_EMPTY) {
      mat = Imgcodecs.imread(path);
    } else {
      mat = Imgcodecs.imread(path, flags);
    }
    if (mat.empty()) {
      throw new ReadFailedException(path);
    }
    return mat;
  }

  public static void write(String path, Mat image, MatOfInt flags) throws WriteFailedException {
    boolean result;
    if (flags.empty()) {
      result = Imgcodecs.imwrite(path, image);
    } else {
      result = Imgcodecs.imwrite(path, image, flags);
    }
    if (!result) {
      throw new WriteFailedException(path);
    }
  }

}
