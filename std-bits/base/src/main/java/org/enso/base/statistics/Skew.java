package org.enso.base.statistics;

public class Skew implements MomentStatistic {
  private final Boolean population;
  private final StandardDeviation standardDeviation;

  public Skew(boolean population) {
    this.population = population;
    this.standardDeviation = new StandardDeviation(population);
  }

  @Override
  public int order() {
    return 3;
  }

  @Override
  public double evaluate(long n, double[] sums) {
    if (n < 3) {
      return Double.NaN;
    }

    double avg = sums[0] / n;
    double st_dev = standardDeviation.evaluate(n, sums);
    double scale =
        1.0 / (st_dev * st_dev * st_dev) / (population ? n : ((n - 1.0) * (n - 2.0) / n));
    return (sums[2] - 3 * avg * sums[1] + 2 * avg * avg * sums[0]) * scale;
  }
}
