package org.enso.base.statistics;

import org.graalvm.polyglot.Context;

/** Class to compute covariance and correlations between series. */
public class CorrelationStatistics {
  private long count = 0;
  private double totalX = 0.0;
  private double totalXX = 0.0;
  private double totalY = 0.0;
  private double totalYY = 0.0;
  private double totalXY = 0.0;

  private void append(Double x, Double y) {
    if (x == null || x.isNaN() || y == null || y.isNaN()) {
      return;
    }

    count++;
    totalX += x;
    totalXX += x * x;
    totalY += y;
    totalYY += y * y;
    totalXY += x * y;
  }

  /*
   * Count of non-null pairs of values.
   */
  public long getCount() {
    return count;
  }

  /*
   * Sum of X values.
   */
  public double getTotalX() {
    return totalX;
  }

  /*
   * Sum of Y values.
   */
  public double getTotalY() {
    return totalY;
  }

  /*
   * Sum of X^2 values.
   */
  public double getTotalXX() {
    return totalXX;
  }

  /*
   * Sum of X * Y values.
   */
  public double getTotalXY() {
    return totalXY;
  }

  /*
   * Sum of Y^2 values.
   */
  public double getTotalYY() {
    return totalYY;
  }

  /*
   * Compute the covariance of X and Y.
   */
  public double covariance() {
    if (count < 2) {
      return Double.NaN;
    }

    return (totalXY - totalX * totalY / count) / count;
  }

  /*
   * Compute the Pearson correlation between X and Y.
   */
  public double pearsonCorrelation() {
    if (count < 2) {
      return Double.NaN;
    }

    double n_stdev_x = Math.sqrt(count * totalXX - totalX * totalX);
    double n_stdev_y = Math.sqrt(count * totalYY - totalY * totalY);
    return (count * totalXY - totalX * totalY) / (n_stdev_x * n_stdev_y);
  }

  /*
   * Compute the R-Squared between X and Y (which equals the Pearson correlation ^ 2).
   */
  public double rSquared() {
    double correl = this.pearsonCorrelation();
    return correl * correl;
  }

  /**
   * Create the CorrelationStats between two series
   *
   * @param x Array of X values
   * @param y Array of Y values
   * @return CorrelationStats object for the 2 series.
   */
  public static CorrelationStatistics compute(Double[] x, Double[] y) {
    if (x.length != y.length) {
      throw new IllegalArgumentException("Left and right lengths are not the same.");
    }

    CorrelationStatistics output = new CorrelationStatistics();
    for (int i = 0; i < x.length; i++) {
      output.append(x[i], y[i]);
    }
    return output;
  }

  public static CorrelationStatistics[][] computeMatrix(Double[][] data) {
    int len = data[0].length;

    Context context = Context.getCurrent();
    CorrelationStatistics[][] output = new CorrelationStatistics[data.length][];
    for (int i = 0; i < data.length; i++) {
      if (data[i].length != len) {
        throw new IllegalArgumentException("Data lengths are not consistent.");
      }
      output[i] = new CorrelationStatistics[data.length];
      for (int j = 0; j < data.length; j++) {
        if (j < i) {
          output[i][j] = output[j][i];
        } else {
          output[i][j] = compute(data[i], data[j]);
        }

        context.safepoint();
      }
    }
    return output;
  }

  public static double spearmanRankCorrelation(Double[] x, Double[] y) {
    double[][] pairedRanks = Rank.pairedRanks(x, y, Rank.Method.AVERAGE);

    Context context = Context.getCurrent();
    CorrelationStatistics computation = new CorrelationStatistics();
    for (int i = 0; i < pairedRanks[0].length; i++) {
      computation.append(pairedRanks[0][i], pairedRanks[1][i]);
      context.safepoint();
    }
    return computation.pearsonCorrelation();
  }
}
