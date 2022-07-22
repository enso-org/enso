package org.enso.base.statistics;

public class Regression {
  /**
   * Performs a least squares fit of a line to the data.
   *
   * @param known_xs Set of known X values.
   * @param known_ys Set of known Y values.
   * @return A fitted linear model (y = Intercept + Slope x) and the r-squared value.
   * @throws IllegalArgumentException if the number of elements in the arrays is different or a
   *     singular X value is provided.
   */
  public static LinearModel fit_linear(Double[] known_xs, Double[] known_ys)
      throws IllegalArgumentException, FitError {
    CorrelationStatistics stats = CorrelationStatistics.compute(known_xs, known_ys);

    double denominator = denominator(stats);
    if (denominator == 0) {
      throw new FitError("Singular X value.");
    }

    double slope = slope(stats, denominator);
    return new LinearModel(slope, intercept(stats, slope), stats.rSquared());
  }

  /**
   * Performs a least squares fit of a line to the data with a given intercept.
   *
   * @param known_xs Set of known X values.
   * @param known_ys Set of known Y values.
   * @param intercept The intercept of the line.
   * @return A fitted linear model (y = Intercept + Slope x) and the r-squared value.
   * @throws IllegalArgumentException if the number of elements in the arrays is different or a
   *     singular X value is provided.
   */
  public static LinearModel fit_linear(Double[] known_xs, Double[] known_ys, double intercept)
      throws IllegalArgumentException {
    CorrelationStatistics stats = CorrelationStatistics.compute(known_xs, known_ys);
    return new LinearModel(slopeWithIntercept(stats, intercept), intercept, stats.rSquared());
  }

  private static double denominator(CorrelationStatistics stats) {
    return stats.getTotalXX() - stats.getTotalX() * stats.getTotalX() / stats.getCount();
  }

  private static double slope(CorrelationStatistics stats, double denominator) {
    return (stats.getTotalXY() - stats.getTotalX() * stats.getTotalY() / stats.getCount())
        / denominator;
  }

  private static double slopeWithIntercept(CorrelationStatistics stats, double intercept) {
    return (-intercept * stats.getTotalX() + stats.getTotalXY()) / stats.getTotalXX();
  }

  private static double intercept(CorrelationStatistics stats, double slope) {
    return (stats.getTotalY() - stats.getTotalX() * slope) / stats.getCount();
  }
}
