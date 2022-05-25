package org.enso.base.statistics;

public interface MomentStatistic {
  /**
   * Maximum order needed to compute the statistic
   *
   * @return Max order needed. 0 if only need the count.
   */
  int order();

  /**
   * Compute the statistic
   *
   * @param n the count of valid values
   * @param sums the totals of each order
   * @return computed statistic
   */
  double evaluate(long n, double[] sums);
}
