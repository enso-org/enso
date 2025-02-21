package org.enso.interpreter.bench.result;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.TimeUnit;
import org.enso.version.BuildVersion;
import org.openjdk.jmh.results.RunResult;

/**
 * @See `results_schema.json` for the schema of the results file.
 */
public record Result(
    String label,
    String timestamp,
    double score,
    long samples,
    int warmupIterations,
    long warmupMillis,
    int measureIterations,
    long measureMillis,
    String commitId,
    String branch,
    MeasurementStatistics measurementStatistics) {
  public static Result fromJMHResult(RunResult result) {
    var params = result.getParams();
    var benchName = params.getBenchmark();
    var timestamp = createTimestamp();
    var measureIterations = result.getParams().getMeasurement().getCount();
    var warmupIterations = result.getParams().getWarmup().getCount();
    var measureMillis =
        result.getParams().getMeasurement().getTime().convertTo(TimeUnit.MILLISECONDS);
    var warmupMillis = result.getParams().getWarmup().getTime().convertTo(TimeUnit.MILLISECONDS);
    var score = result.getPrimaryResult().getScore();
    var samples = result.getPrimaryResult().getStatistics().getN();
    var commitId = BuildVersion.commit();
    var branch = BuildVersion.ref();
    var stats = MeasurementStatistics.fromJMH(result.getPrimaryResult().getStatistics());
    return new Result(
        benchName,
        timestamp,
        score,
        samples,
        warmupIterations,
        warmupMillis,
        measureIterations,
        measureMillis,
        commitId,
        branch,
        stats);
  }

  private static String createTimestamp() {
    var localDateTime = LocalDateTime.now();
    var zonedDateTime = localDateTime.atZone(ZoneOffset.UTC);
    var formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'");
    return zonedDateTime.format(formatter);
  }
}
