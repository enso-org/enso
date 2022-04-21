package org.enso.base.statistics;

import java.util.Arrays;

/***
 * Set of descriptive statistics for numerical data sets
 */
public enum SingleValue {
  COUNT {
    @Override
    public int order() {
      return 0;
    }

    @Override
    public double evaluate(long n, double[] sums) {
      return n;
    }
  },
  SUM {
    @Override
    public int order() {
      return 1;
    }

    @Override
    public double evaluate(long n, double[] sums) {
      return sums[0];
    }
  },
  MEAN {
    @Override
    public int order() {
      return 1;
    }

    @Override
    public double evaluate(long n, double[] sums) {
      return n == 0 ? Double.NaN : sums[0] / n;
    }
  },
  VARIANCE {
    @Override
    public int order() {
      return 2;
    }

    @Override
    public double evaluate(long n, double[] sums) {
      return n < 2 ? Double.NaN : (n * sums[1] - sums[0] * sums[0]) / (n - 1);
    }
  },
  VARIANCE_POPULATION {
    @Override
    public int order() {
      return 2;
    }

    @Override
    public double evaluate(long n, double[] sums) {
      return n == 0 ? Double.NaN : sums[1] - sums[0] * sums[0] / n;
    }
  },
  STANDARD_DEVIATION {
    @Override
    public int order() {
      return VARIANCE.order();
    }

    @Override
    public double evaluate(long n, double[] sums) {
      return Math.sqrt(VARIANCE.evaluate(n, sums));
    }
  },
  STANDARD_DEVIATION_POPULATION {
    @Override
    public int order() {
      return VARIANCE_POPULATION.order();
    }

    @Override
    public double evaluate(long n, double[] sums) {
      return Math.sqrt(VARIANCE_POPULATION.evaluate(n, sums));
    }
  },
  SKEW {
    @Override
    public int order() {
      return 3;
    }

    @Override
    public double evaluate(long n, double[] sums) {
      double mean = MEAN.evaluate(n, sums);
      double st_dev = STANDARD_DEVIATION.evaluate(n, sums);
      double scale = n / ((n - 1.0) * (n - 2.0) * st_dev * st_dev * st_dev);
      return (sums[2] - 3 * mean * st_dev * st_dev + 2 * mean * mean * sums[0]) * scale;
    }
  },
  SKEW_POPULATION {
    @Override
    public int order() {
      return 3;
    }

    @Override
    public double evaluate(long n, double[] sums) {
      double mean = MEAN.evaluate(n, sums);
      double stdev = STANDARD_DEVIATION_POPULATION.evaluate(n, sums);
      return (sums[2]/n - 3 * mean * stdev * stdev - mean * mean * mean) / (stdev * stdev * stdev);
    }
  };

  public abstract int order();

  public abstract double evaluate(long n, double[] sums);

  public static double[] compute(double[] data, SingleValue[] statistics) {
    if (statistics.length == 0) {
      return new double[0];
    }

    // Get Order
    int order = Arrays.stream(statistics).mapToInt(SingleValue::order).max().getAsInt();

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
