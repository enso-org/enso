package org.enso.interpreter.bench;

import org.openjdk.jmh.results.Result;
import org.openjdk.jmh.results.RunResult;

public class BenchmarkResultProcessor {
  /**
   * Matches the new result with historic results from the report and updates the report.
   * @param label The name by which this result should be referred to as in the result.
   * @param report Historic runs report.
   * @param result Fresh JMH benchmark result.
   * @return
   */
  public BenchmarkItem processResult(String label, Report report, RunResult result) {
    Result primary = result.getPrimaryResult();
    ReportItem item = report.findOrCreateByLabel(label);
    item.addScore(primary.getScore());
    return new BenchmarkItem(primary, item);
  }
}
