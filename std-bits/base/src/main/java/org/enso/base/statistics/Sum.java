package org.enso.base.statistics;

public class Sum implements MomentStatistic {
  @Override
  public int order() {
    return 1;
  }

  @Override
  public double evaluate(long n, double[] sums) {
    return sums[0];
  }
}
