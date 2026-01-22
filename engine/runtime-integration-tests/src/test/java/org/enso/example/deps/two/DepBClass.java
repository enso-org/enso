package org.enso.example.deps.two;

import org.enso.example.deps.AwaitLoading;
import org.enso.example.deps.one.DepAClass;

public class DepBClass {
  static {
    System.err.println("Started DepBClass");
    AwaitLoading.waitForTwo();
    System.err.println("Loading DepBClass");
    System.err.println("Finished DepBClass");
  }

  public static double sigmoid(double x) {
    return 1 / (1 + DepAClass.expToNeg(x));
  }

  public static double expPow(double x) {
    return Math.exp(x);
  }
}
