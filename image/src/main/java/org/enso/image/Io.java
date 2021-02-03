package org.enso.image;

import org.enso.opencv.OpenCV;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.imgcodecs.Imgcodecs;

public class Io {

  static {
    OpenCV.loadShared();
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
  }

  public static Mat read(String path) {
    return Imgcodecs.imread(path);
  }
}
