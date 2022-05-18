package org.enso.base.statistics;

public class Kurtosis implements MomentStatistic {
  private final Variance variance;

  public Kurtosis() {
    this.variance = new Variance(false);
  }

  @Override
  public int order() {
    return 4;
  }

  @Override
  public double evaluate(long n, double[] sums) {
    if (n < 4) {
      return Double.NaN;
    }

    double avg = sums[0] / n;
    double var = variance.evaluate(n, sums);
    double scale = n * (n + 1) / ((n - 1) * (n - 2) * (n - 3) * var * var);
    double shift = 3.0 * (n - 1.0) * (n - 1.0) / ((n - 2.0) * (n - 3.0));
    return (sums[3] - 4 * avg * sums[2] + 6 * avg * avg * sums[1] - 3 * avg * avg * avg * sums[0])
        * scale - shift;
  }
}
