package org.enso.interpreter.bench;

import org.openjdk.jmh.results.Result;

/** Convenience class for clients to compare historic results with the last JMH run. */
public class BenchmarkItem {
  private final Result result;
  private final ReportItem previousResults;

  public BenchmarkItem(Result result, ReportItem previousResults) {
    this.result = result;
    this.previousResults = previousResults;
  }

  public Result getResult() {
    return result;
  }

  public ReportItem getPreviousResults() {
    return previousResults;
  }

  /**
   * @return Best historic score for the given benchmark (including current run).
   */
  public double getBestScore() {
    return previousResults.getBestScore().orElse(result.getScore());
  }

  public double getScore() {
    return result.getScore();
  }

  public String getLabel() {
    return result.getLabel();
  }
}
