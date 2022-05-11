package org.enso.base.statistics;

public class Mean implements MomentStatistic {
  @Override
  public int order() {
    return 1;
  }

  @Override
  public double evaluate(long n, double[] sums) {
    return n == 0 ? Double.NaN : sums[0] / n;
  }
}
