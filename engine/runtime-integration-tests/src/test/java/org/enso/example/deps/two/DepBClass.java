package org.enso.example.deps.two;

import org.enso.example.deps.AwaitLoading;
import org.enso.example.deps.one.DepAClass;

public class DepBClass {
  private static final DepAClass OTHER;

  static {
    System.err.println("Started DepBClass");
    AwaitLoading.waitForTwo();
    System.err.println("Loading DepBClass");
    OTHER = new DepAClass();
    System.err.println("Finished DepBClass");
  }

  public double sigmoid(double x) {
    return 1 / (1 + OTHER.expToNeg(x));
  }

  public double expPow(double x) {
    return Math.exp(x);
  }
}
