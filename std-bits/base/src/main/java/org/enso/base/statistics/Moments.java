package org.enso.base.statistics;

/** Set of descriptive statistics for numerical data sets */
public class Moments {

  /** Statistic to compute the total of the values. */
  public static final MomentStatistic SUM = new Sum();

  /** Statistic to compute the mean average of the values. */
  public static final MomentStatistic MEAN = new Mean();

  /** Statistic to compute the sample variance of the values. */
  public static final MomentStatistic VARIANCE = new Variance(false);

  /** Statistic to compute the population variance of the values. */
  public static final MomentStatistic VARIANCE_POPULATION = new Variance(true);

  /** Statistic to compute the sample standard deviation of the values. */
  public static final MomentStatistic STANDARD_DEVIATION = new StandardDeviation(false);

  /** Statistic to compute the population standard deviation of the values. */
  public static final MomentStatistic STANDARD_DEVIATION_POPULATION = new StandardDeviation(true);

  /** Statistic to compute the sample skewness of the values. */
  public static final MomentStatistic SKEW = new Skew(false);

  /** Statistic to compute the population skewness of the values. */
  public static final MomentStatistic SKEW_POPULATION = new Skew(true);

  /** Statistic to compute the sample kurtosis of the values. */
  public static final MomentStatistic KURTOSIS = new Kurtosis();

  private long count;
  private double[] totals;

  /**
   * Create a new instance.
   *
   * @param order the maximum order of moments to compute.
   */
  public Moments(int order) {
    this.count = 0;
    this.totals = new double[order];
  }

  public Moments add(double value) {
    count++;
    double v = value;
    for (int i = 0; i < totals.length; i++) {
      totals[i] += v;
      v *= value;
    }

    return this;
  }

  public double compute(MomentStatistic s) {
    return s.evaluate(count, totals);
  }
}
