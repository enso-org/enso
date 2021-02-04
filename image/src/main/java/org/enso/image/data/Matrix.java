package org.enso.image.data;

import org.opencv.core.Mat;

public class Matrix {

  public static Mat zeros(int rows, int cols, int type) {
    return Mat.zeros(rows, cols, type);
  }
}
