package org.enso.base.statistics;

/** Set of descriptive statistics for numerical data sets */
public class Moments {
  private long count;
  private double[] totals;

  /**
   * Create a new instance.
   *
   * @param order the maximum order of moments to compute.
   */
  public Moments(int order) {
    this.count = 0;
    this.totals = new double[order];
  }

  public Moments add(double value) {
    count++;
    double v = value;
    for (int i = 0; i < totals.length; i++) {
      totals[i] += v;
      v *= value;
    }

    return this;
  }

  public long getCount() {
    return count;
  }

  public double[] getTotals() {
    return totals;
  }
}
