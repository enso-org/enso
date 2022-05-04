package org.enso.base.statistics;

public class Skew implements Statistic {
  private final Mean mean;
  private final StandardDeviation standardDeviation;

  public Skew(boolean population) {
    this.mean = new Mean();
    this.standardDeviation = new StandardDeviation(population);
  }

  @Override
  public int order() {
    return 3;
  }

  @Override
  public double evaluate(long n, double[] sums) {
    double avg = mean.evaluate(n, sums);
    double st_dev = standardDeviation.evaluate(n, sums);
    double scale = n / ((n - 1.0) * (n - 2.0) * st_dev * st_dev * st_dev);
    return (sums[2] - 3 * avg * st_dev * st_dev + 2 * avg * avg * sums[0]) * scale;
  }
}
