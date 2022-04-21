package org.enso.base.statistics.models;

public class Linear {
  public final double slope;
  public final double intercept;
  public final double rSquared;

  public Linear(double slope, double intercept, double rSquared) {
    this.slope = slope;
    this.intercept = intercept;
    this.rSquared = rSquared;
  }
}
