package org.enso.base.statistics;

public class Variance implements Statistic {
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
    return n < (population ? 1 : 2)
        ? Double.NaN
        : (n * sums[1] - sums[0] * sums[0]) / (population ? n : n - 1);
  }
}
