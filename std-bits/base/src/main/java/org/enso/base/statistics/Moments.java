package org.enso.base.statistics;

import java.util.Arrays;

/***
 * Set of descriptive statistics for numerical data sets
 */
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

  /**
   * Compute a set of statistics on a data set.
   *
   * @param data set of values.
   * @param statistics set of statistics to compute.
   * @return computed statistics.
   */
  public static double[] compute(Double[] data, MomentStatistic[] statistics) {
    if (statistics.length == 0) {
      return new double[0];
    }

    int order = Arrays.stream(statistics).mapToInt(s -> s == null ? 0 : s.order()).max().getAsInt();

    long count = 0;
    double[] totals = new double[order];
    for (Double value : data) {
      if (value == null || Double.isNaN(value)) {
        continue;
      }

      count++;
      double v = value;
      for (int i = 0; i < order; i++) {
        totals[i] += v;
        v *= value;
      }
    }

    final long _count = count;
    return Arrays.stream(statistics)
        .mapToDouble(s -> s == null ? Double.NaN : s.evaluate(_count, totals))
        .toArray();
  }
}
