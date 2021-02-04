package org.enso.image;

import org.enso.opencv.OpenCV;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.imgcodecs.Imgcodecs;

public class Algorithm {

  static {
    OpenCV.loadShared();
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
  }

  public static Mat read(String path) {
    return Imgcodecs.imread(path, Imgcodecs.IMREAD_UNCHANGED);
  }

  public static void write(String path, Mat image) {
    Imgcodecs.imwrite(path, image);
  }

  public static Mat transform(double alpha, int beta, Mat image) {
    Mat newImage = Mat.zeros(image.size(), image.type());
    byte[] imageData = new byte[(int) (image.total()*image.channels())];
    image.get(0, 0, imageData);
    byte[] newImageData = new byte[(int) (newImage.total()*newImage.channels())];

    for (int y = 0; y < image.rows(); y++) {
      for (int x = 0; x < image.cols(); x++) {
        for (int c = 0; c < image.channels(); c++) {
          double pixelValue = imageData[(y * image.cols() + x) * image.channels() + c];
          pixelValue = pixelValue < 0 ? pixelValue + 256 : pixelValue;
          newImageData[(y * image.cols() + x) * image.channels() + c] =
              clip(alpha * pixelValue + beta);
        }
      }
    }

    newImage.put(0, 0, newImageData);
    return newImage;
  }

  public static Mat transform1(double alpha, int beta, Mat image) {
    Mat newImage = Mat.zeros(image.size(), image.type());

    for (int y = 0; y < image.rows(); y++) {
      for (int x = 0; x < image.cols(); x++) {
        double[] pixel = image.get(y, x);
        saturatePixel(alpha, beta, pixel);
        newImage.put(y, x, pixel);
      }
    }

    return newImage;
  }

  private static void saturatePixel(double alpha, int beta, double[] pixel) {
    for (int i = 0; i < pixel.length; i++) {
      double pixelValue = pixel[i];
      pixelValue = pixelValue < 0 ? pixelValue + 256 : pixelValue;
      pixel[i] = alpha * pixelValue + beta;
    }
  }

  private static byte clip(double val) {
    int iVal = (int) Math.round(val);
    iVal = Math.min(Math.max(iVal, 0), 255);
    return (byte) iVal;
  }
}
