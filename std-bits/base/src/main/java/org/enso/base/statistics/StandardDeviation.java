package org.enso.base.statistics;

public class StandardDeviation extends Variance {
  public StandardDeviation(boolean population) {
    super(population);
  }

  @Override
  public double evaluate(long n, double[] sums) {
    return Math.sqrt(super.evaluate(n, sums));
  }
}
