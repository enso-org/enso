package org.enso.example.deps.one;

import org.enso.example.deps.AwaitLoading;
import org.enso.example.deps.two.DepBClass;

public class DepAClass {
  private static final DepBClass OTHER;

  static {
    System.err.println("Started DepAClass");
    AwaitLoading.waitForTwo();
    System.err.println("Loading DepAClass");
    OTHER = new DepBClass();
    System.err.println("Finished DepAClass");
  }

  public static double expToNeg(double x) {
    return OTHER.expPow(-x);
  }
}
