package org.enso.base.statistics;

public class Multivariate {
  public static double[][] pearsonCorrelation(double[][] input) {
    double[][] expectations = computeExpectations(input);

    double[][] correlations = new double[input.length][];
    return correlations;
  }

  private static boolean allValid(double[][] input, int row) {
    for (int i = 0; i < input.length; i++) {
      if (Double.isNaN(input[i][row])) {
        return false;
      }
    }

    return true;
  }

  private static double[][] computeExpectations(double[][] input) {
    int m = input.length;
    if (m == 0) {
      return new double[0][];
    }

    int n = input[0].length;

    // Set up Results array
    double[][] results = new double[m][];
    for (int i = 0; i < m; i++) {
      if (input[i].length != n) {
        throw new IllegalArgumentException("Input arrays of different size");
      }

      // N X XX E(XiYj)
      results[i] = new double[input.length + 3];
    }

    // Compute Expectations
    for (int row = 0; row < n; row++) {
      // Check validity
      if (allValid(input, row)) {
        for (int i = 0; i < m; i++) {
          double x = input[i][row];

          results[i][0]++;
          results[i][1] += x;
          results[i][2] += x * x;

          for (int j = 0; j < m; j++) {
            double y = input[i][row];
            results[i][j + 3] = (results[i][j + 3] * m + x * y);
          }
        }
      }
    }

    return results;
  }
}
