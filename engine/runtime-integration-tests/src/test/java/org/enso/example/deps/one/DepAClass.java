package org.enso.example.deps.one;

import org.enso.example.deps.AwaitLoading;
import org.enso.example.deps.two.DepBClass;

public class DepAClass {
  static {
    System.err.println("Started DepAClass");
    AwaitLoading.waitForTwo();
    System.err.println("Loading DepAClass");
    System.err.println("Finished DepAClass");
  }

  public static double expToNeg(double x) {
    return DepBClass.expPow(-x);
  }
}
