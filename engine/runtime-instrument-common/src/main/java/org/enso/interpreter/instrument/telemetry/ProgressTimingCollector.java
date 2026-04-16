package org.enso.interpreter.instrument.telemetry;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Collects timing data from {@link org.enso.base.ProgressHandle} completions and periodically emits
 * a telemetry summary of the top-N slowest components ranked by average per-item time.
 *
 * <p>Starts a periodic flush timer on construction. The caller is responsible for invoking {@link
 * #flushAsTelemetry} and {@link #shutdown} during engine teardown (before logging is torn down) to
 * guarantee delivery of the final telemetry batch.
 */
public final class ProgressTimingCollector {

  static final int TOP_N = 10;
  static final int PERIODIC_FLUSH_MINUTES = 5;
  static final int MAX_DISTINCT_HANDLES = 100_000;

  private static final Logger logger =
      LoggerFactory.getLogger("org.enso.telemetry.SlowestComponents");

  private final ConcurrentHashMap<String, TimingStats> stats = new ConcurrentHashMap<>();

  private volatile String cachedProjectId;
  private volatile String cachedSessionId;

  private ScheduledFuture<?> periodicTask;

  /**
   * Creates a new collector that schedules periodic telemetry flushes on the given executor.
   *
   * @param scheduler the executor to use for periodic flush scheduling
   */
  public ProgressTimingCollector(ScheduledExecutorService scheduler) {
    this.periodicTask =
        scheduler.scheduleAtFixedRate(
            this::flushAsTelemetry,
            PERIODIC_FLUSH_MINUTES,
            PERIODIC_FLUSH_MINUTES,
            TimeUnit.MINUTES);
  }

  /**
   * Records a single progress handle completion.
   *
   * @param handleName the progress handle name identifying the operation
   * @param itemCount the {@code up_to} value (workload size) from {@code Progress.run}
   * @param elapsedMs total elapsed time in milliseconds
   */
  public void recordTiming(String handleName, long itemCount, long elapsedMs) {
    if (stats.size() >= MAX_DISTINCT_HANDLES && !stats.containsKey(handleName)) {
      evictLowest();
    }
    stats.compute(
        handleName,
        (key, existing) -> {
          double perItem = (double) elapsedMs / Math.max(itemCount, 1);
          if (existing == null) {
            return new TimingStats(1, itemCount, elapsedMs, perItem, perItem * perItem);
          }
          return existing.record(itemCount, elapsedMs);
        });
  }

  /** Computes the top-N slowest components and emits them as telemetry messages. */
  public synchronized void flushAsTelemetry() {
    var topTen = computeTopN();
    if (topTen.isEmpty()) {
      return;
    }
    emitTelemetryMessages(topTen);
  }

  /** Returns the current top-N entries sorted by average per-item time descending. */
  List<Map.Entry<String, TimingStats>> computeTopN() {
    return stats.entrySet().stream()
        .sorted(
            Comparator.comparingDouble(
                    (Map.Entry<String, TimingStats> e) -> e.getValue().avgPerItem())
                .reversed())
        .limit(TOP_N)
        .toList();
  }

  /** Returns whether stats contain an entry for the given handle name. For testing only. */
  boolean containsHandle(String name) {
    return stats.containsKey(name);
  }

  /** Shuts down the periodic task and resets all state. Does not shut down the executor. */
  public synchronized void shutdown() {
    if (periodicTask != null) {
      periodicTask.cancel(false);
      periodicTask = null;
    }
    stats.clear();
    cachedProjectId = null;
    cachedSessionId = null;
  }

  private void emitTelemetryMessages(List<Map.Entry<String, TimingStats>> topTen) {
    var projectId = resolveProjectId();
    var sessionId = resolveSessionId();
    for (int i = 0; i < topTen.size(); i++) {
      var entry = topTen.get(i);
      var s = entry.getValue();
      var sAvgPerItem = s.avgPerItem();
      logger
          .atTrace()
          .setMessage(
              "Slow component: handle_name={}, invocations={}, total_items={},"
                  + " total_ms={}, avg_per_item_ms={}, stddev_per_item_ms={},"
                  + " rank={}, session_id={}, project_id={}")
          .addArgument(entry.getKey())
          .addArgument(s.invocations)
          .addArgument(s.totalItems)
          .addArgument(s.totalMs)
          .addArgument(Math.round(sAvgPerItem))
          .addArgument(Math.round(s.stddevPerItem(sAvgPerItem)))
          .addArgument(i + 1)
          .addArgument(sessionId)
          .addArgument(projectId)
          .log();
    }
  }

  private void evictLowest() {
    int toEvict = Math.max(1, stats.size() / 5);
    stats.entrySet().stream()
        .sorted(
            Comparator.comparingDouble(
                (Map.Entry<String, TimingStats> e) -> e.getValue().avgPerItem()))
        .limit(toEvict)
        .map(Map.Entry::getKey)
        .toList()
        .forEach(stats::remove);
  }

  private String resolveProjectId() {
    if (cachedProjectId == null) {
      var id = System.getenv("ENSO_CLOUD_PROJECT_ID");
      if (id == null) {
        id = System.getenv("ENSO_LOCAL_PROJECT_ID");
      }
      cachedProjectId = id != null ? id : "unknown";
    }
    return cachedProjectId;
  }

  private String resolveSessionId() {
    if (cachedSessionId == null) {
      var id = System.getenv("ENSO_CLOUD_PROJECT_SESSION_ID");
      if (id == null) {
        id = System.getenv("ENSO_LOCAL_PROJECT_SESSION_ID");
      }
      cachedSessionId = id != null ? id : UUID.randomUUID().toString();
    }
    return cachedSessionId;
  }

  /** Running per-item statistics for a single progress handle name. */
  record TimingStats(
      long invocations,
      long totalItems,
      long totalMs,
      double sumPerItemMs,
      double sumPerItemMsSquared) {

    TimingStats record(long itemCount, long elapsedMs) {
      double perItem = (double) elapsedMs / Math.max(itemCount, 1);
      return new TimingStats(
          invocations + 1,
          totalItems + itemCount,
          totalMs + elapsedMs,
          sumPerItemMs + perItem,
          sumPerItemMsSquared + perItem * perItem);
    }

    double avgPerItem() {
      return invocations == 0 ? 0 : sumPerItemMs / invocations;
    }

    double stddevPerItem(double mean) {
      if (invocations < 2) {
        return 0;
      }
      double variance = sumPerItemMsSquared / invocations - mean * mean;
      return Math.sqrt(Math.max(variance, 0));
    }

    double stddevPerItem() {
      return stddevPerItem(avgPerItem());
    }
  }
}
