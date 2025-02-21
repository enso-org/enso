package org.enso.interpreter.bench.result;

import org.openjdk.jmh.util.Statistics;

public record Percentile(double value, double percentile) {
  public static Percentile fromJMHStats(double percentile, Statistics stats) {
    return new Percentile(stats.getPercentile(percentile), percentile);
  }
}
