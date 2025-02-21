package org.enso.interpreter.bench.result;

import org.openjdk.jmh.util.Statistics;

public record MeasurementStatistics(
    double stddev,
    double mean,
    double min,
    double max,
    double error50,
    double error95,
    Percentile[] percentiles) {
  public static MeasurementStatistics fromJMH(Statistics stats) {
    return new MeasurementStatistics(
        stats.getStandardDeviation(),
        stats.getMean(),
        stats.getMin(),
        stats.getMax(),
        stats.getMeanErrorAt(0.5),
        stats.getMeanErrorAt(0.95),
        new Percentile[] {
          Percentile.fromJMHStats(10, stats),
          Percentile.fromJMHStats(25, stats),
          Percentile.fromJMHStats(50, stats),
          Percentile.fromJMHStats(75, stats),
          Percentile.fromJMHStats(90, stats)
        });
  }
}
