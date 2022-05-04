package org.enso.base.statistics;

import java.util.Arrays;

/***
 * Set of descriptive statistics for numerical data sets
 */
public class SingleValue {

  /**
   * Statistic to compute the total of the values.
   */
  public static final Statistic SUM = new Sum();

  /**
   * Statistic to compute the mean average of the values.
   */
  public static final Statistic MEAN = new Mean();

  public static final Statistic VARIANCE = new Variance(false);

  public static final Statistic VARIANCE_POPULATION = new Variance(true);

  public static final Statistic STANDARD_DEVIATION = new StandardDeviation(false);

  public static final Statistic STANDARD_DEVIATION_POPULATION = new StandardDeviation(true);

  public static final Statistic SKEW = new Skew(false);

  public static final Statistic SKEW_POPULATION = new Skew(true);

  public static double[] compute(double[] data, Statistic[] statistics) {
    if (statistics.length == 0) {
      return new double[0];
    }

    // Get Order
    int order = Arrays.stream(statistics).mapToInt(Statistic::order).max().getAsInt();

    // Compute
    long count = 0;
    double[] totals = new double[order];
    for (double value : data) {
      if (Double.isNaN(value)) {
        continue;
      }

      count++;
      double v = value;
      for (int i = 0; i < order; i++) {
        totals[i] += v;
        v *= value;
      }
    }

    // Create Stats
    final long _count = count;
    return Arrays.stream(statistics).mapToDouble(s -> s.evaluate(_count, totals)).toArray();
  }
}
