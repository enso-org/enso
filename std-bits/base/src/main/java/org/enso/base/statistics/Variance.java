package org.enso.base.statistics;

public class Variance implements MomentStatistic {
  private final boolean population;

  public Variance(boolean population) {
    this.population = population;
  }

  public boolean isPopulation() {
    return population;
  }

  @Override
  public int order() {
    return 2;
  }

  @Override
  public double evaluate(long n, double[] sums) {
    if (population) {
      return n < 1 ? Double.NaN : (sums[1] - sums[0] * sums[0] / n) / n;
    } else {
      return n < 2 ? Double.NaN : (sums[1] - sums[0] * sums[0] / n) / (n - 1);
    }
  }
}
