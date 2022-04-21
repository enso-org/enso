package org.enso.base.statistics;

import java.util.Arrays;

public enum SingleValue {
  COUNT {
    @Override
    public int order() {
      return 0;
    }

    @Override
    public double evaluate(long n, double[] sums, double min, double max) {
      return n;
    }
  },
  MINIMUM {
    @Override
    public int order() {
      return 0;
    }

    @Override
    public double evaluate(long n, double[] sums, double min, double max) {
      return min;
    }
  },
  MAXIMUM {
    @Override
    public int order() {
      return 0;
    }

    @Override
    public double evaluate(long n, double[] sums, double min, double max) {
      return max;
    }
  },
  SUM {
    @Override
    public int order() {
      return 1;
    }

    @Override
    public double evaluate(long n, double[] sums, double min, double max) {
      return sums[0];
    }
  },
  MEAN {
    @Override
    public int order() {
      return 1;
    }

    @Override
    public double evaluate(long n, double[] sums, double min, double max) {
      return sums[0] / n;
    }
  },
  VARIANCE {
    @Override
    public int order() {
      return 2;
    }

    @Override
    public double evaluate(long n, double[] sums, double min, double max) {
      return (n * sums[1] - sums[0] * sums[0]) / (n - 1);
    }
  },
  VARIANCE_POPULATION {
    @Override
    public int order() {
      return 2;
    }

    @Override
    public double evaluate(long n, double[] sums, double min, double max) {
      return sums[1] - sums[0] * sums[0] / n;
    }
  };

  public abstract int order();

  public abstract double evaluate(long n, double[] sums, double min, double max);

  public static double[] compute(double[] data, SingleValue[] statistics) {
    if (statistics.length == 0) {
      return new double[0];
    }

    // Get Order
    int order = Arrays.stream(statistics).mapToInt(SingleValue::order).max().getAsInt();

    // Compute
    long count = 0;
    double[] totals = new double[order];
    double min = Double.MAX_VALUE;
    double max = Double.MIN_VALUE;
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
      if (min > value) {
        min = value;
      }
      if (max < value) {
        max = value;
      }
    }

    // Create Stats
    final long _count = count;
    final double _min = min;
    final double _max = max;
    return Arrays.stream(statistics).mapToDouble(s -> s.evaluate(_count, totals, _min, _max)).toArray();
  }
}
