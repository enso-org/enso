package org.enso.image;

import org.enso.image.opencv.OpenCV;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfInt;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;

public class Codecs {

  public static final int READ_FLAG_EMPTY = -127;

  static {
    OpenCV.loadShared();
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
  }

  /** An error occurred when reading a file. */
  public static class ReadFailedException extends Exception {

    public ReadFailedException(String path) {
      super("Failed to read " + path);
    }
  }

  /** An error occurred when writing a file */
  public static class WriteFailedException extends Exception {

    public WriteFailedException(String path) {
      super("Failed to write " + path);
    }
  }

  /**
   * Read an image from the file.
   *
   * @param path the file path to read from.
   * @param flags the read flags.
   * @return the matrix holding the image data.
   */
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

    switch (input.channels()) {
      case 3:
        Imgproc.cvtColor(input, input, Imgproc.COLOR_BGR2RGB);
        break;
      case 4:
        Imgproc.cvtColor(input, input, Imgproc.COLOR_BGRA2RGBA);
        break;
    }

    return input;
  }

  /**
   * Write an image to the file.
   *
   * @param path the file path to write to.
   * @param image the matrix representing the image.
   * @param flags the write flags.
   */
  public static void write(String path, Mat image, MatOfInt flags) throws WriteFailedException {
    Mat output = new Mat();
    switch (image.channels()) {
      case 3:
        Imgproc.cvtColor(image, output, Imgproc.COLOR_RGB2BGR);
        break;
      case 4:
        Imgproc.cvtColor(image, output, Imgproc.COLOR_RGBA2BGRA);
        break;
      default:
        output = image;
    }

    boolean result;
    if (flags.empty()) {
      result = Imgcodecs.imwrite(path, output);
    } else {
      result = Imgcodecs.imwrite(path, output, flags);
    }

    if (!result) {
      throw new WriteFailedException(path);
    }
  }

}
