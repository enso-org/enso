package org.enso.image;

import org.enso.image.opencv.OpenCV;
import org.opencv.core.Core;
import org.opencv.core.CvType;
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
    Mat input;
    if (flags == READ_FLAG_EMPTY) {
      input = Imgcodecs.imread(path);
    } else {
      input = Imgcodecs.imread(path, flags);
    }

    if (input.empty()) {
      throw new ReadFailedException(path);
    }

    return input;
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

  public static Double normalize(byte pixelValue) {
    return ((int) pixelValue + 128) / 255.0;
  }

}
