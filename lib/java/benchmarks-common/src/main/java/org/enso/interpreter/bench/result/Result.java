package org.enso.interpreter.bench.result;

import buildinfo.Info;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.TimeUnit;
import org.openjdk.jmh.results.RunResult;

/**
 * @See `results_schema.json` for the schema of the results file.
 */
public record Result(
    String label,
    String timestamp,
    double score,
    double scoreError,
    long samples,
    double min,
    double max,
    double mean,
    double stdDev,
    int warmupIterations,
    long warmupMillis,
    int measureIterations,
    long measureMillis,
    String commitId,
    String branch,
    Configuration configuration) {
  public static Result fromJMHResult(RunResult result) {
    var params = result.getParams();
    var benchName = params.getBenchmark();
    var timestamp = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
    var measureIterations = result.getParams().getMeasurement().getCount();
    var warmupIterations = result.getParams().getWarmup().getCount();
    var measureMillis =
        result.getParams().getMeasurement().getTime().convertTo(TimeUnit.MILLISECONDS);
    var warmupMillis = result.getParams().getWarmup().getTime().convertTo(TimeUnit.MILLISECONDS);
    var score = result.getPrimaryResult().getScore();
    var scoreError = result.getPrimaryResult().getScoreError();
    var min = result.getPrimaryResult().getStatistics().getMin();
    var max = result.getPrimaryResult().getStatistics().getMax();
    var mean = result.getPrimaryResult().getStatistics().getMean();
    var stdDev = result.getPrimaryResult().getStatistics().getStandardDeviation();
    var samples = result.getPrimaryResult().getStatistics().getN();
    var commitId = Info.commit();
    var branch = Info.ref();
    var configuration = Configuration.fromSystemProperties();
    return new Result(
        benchName,
        timestamp,
        score,
        scoreError,
        samples,
        min,
        max,
        mean,
        stdDev,
        warmupIterations,
        warmupMillis,
        measureIterations,
        measureMillis,
        commitId,
        branch,
        configuration);
  }
}
