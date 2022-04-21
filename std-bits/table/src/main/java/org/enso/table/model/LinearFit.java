package org.enso.table.model;

public class LinearFit {
  public final double slope;
  public final double intercept;
  public final double rSquared;

  public LinearFit(double slope, double intercept, double rSquared) {
    this.slope = slope;
    this.intercept = intercept;
    this.rSquared = rSquared;
  }
}
