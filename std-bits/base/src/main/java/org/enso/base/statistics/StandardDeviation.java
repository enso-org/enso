package org.enso.base.statistics;

public class StandardDeviation implements MomentStatistic {
  private final Variance variance;

  public StandardDeviation(boolean population) {
    this.variance = new Variance(population);
  }

  @Override
  public int order() {
    return this.variance.order();
  }

  @Override
  public double evaluate(long n, double[] sums) {
    return Math.sqrt(this.variance.evaluate(n, sums));
  }
}
