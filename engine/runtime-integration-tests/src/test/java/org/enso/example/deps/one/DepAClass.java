package org.enso.example.deps.one;

import org.enso.example.deps.two.DepBClass;

public class DepAClass {
  public static double expToNeg(double x) {
    return DepBClass.expPow(-x);
  }
}
