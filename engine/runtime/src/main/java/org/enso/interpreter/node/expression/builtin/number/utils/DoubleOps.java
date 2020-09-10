package org.enso.interpreter.node.expression.builtin.number.utils;

public class DoubleOps {
  public static final double EPSILON = 2.2204460492503131e-16;

  public static boolean equal(double a, double b) {
    return Math.abs(a - b) < EPSILON;
  }

  public static boolean lessOrEqual(double a, double b) {
    return a <= b;
  }

  public static boolean less(double a, double b) {
    return a < b && !equal(a, b);
  }

  public static boolean greaterOrEqual(double a, double b) {
    return a >= b;
  }

  public static boolean greater(double a, double b) {
    return a > b && !equal(a, b);
  }
}
