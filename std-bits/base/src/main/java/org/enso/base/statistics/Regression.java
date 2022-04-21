package org.enso.base.statistics;

import org.enso.base.statistics.models.Linear;

public class Regression {
  /**
   * Given a set of known y-values and a set of fitted values, compute the R² (coefficient of determination)
   **/
  public static double rSquared(double[] known_ys, double[] fitted_ys) {
    double n = 0, y = 0, yy = 0, ff = 0, fy = 0;

    for (int i = 0; i < known_ys.length; i++) {
      n++;
      y += known_ys[i];
      yy += known_ys[i] * known_ys[i];
      fy += known_ys[i] * fitted_ys[i];
      ff += fitted_ys[i] * fitted_ys[i];
    }

    return 1 - (yy - y * y / n) / (yy - 2 * fy + ff);
  }

  public static Linear linear(double[] known_xs, double[] known_ys) throws FitError {
    double n = 0, x = 0, xy = 0, y = 0, xx = 0, yy = 0;

    for (int i = 0; i < known_xs.length; i++) {
      n++;
      x += known_xs[i];
      xy += known_xs[i] * known_ys[i];
      y += known_ys[i];
      xx += known_xs[i] * known_xs[i];
      yy += known_ys[i] * known_ys[i];
    }

    if (n == 0) {
      throw new FitError("No known values.");
    }

    double denominator = xx - x * x / n;
    if (denominator == 0) {
      throw new FitError("Singular X value.");
    }

    double slope = (xy - x * y / n) / denominator;
    double intercept = (y - x * slope) / n;
    double sstot = yy - y * y / n;
    double ssres =
        yy
            - 2 * intercept * y
            - 2 * slope * xy
            + intercept * intercept * n
            + 2 * slope * intercept * x
            + slope * slope * xx;

    return new Linear(slope, intercept, 1 - ssres / sstot);
  }
}
