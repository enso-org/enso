package org.enso.interpreter.instrument.telemetry;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ProgressTimingCollectorTest {

  private ScheduledExecutorService scheduler;
  private ProgressTimingCollector collector;

  @Before
  public void setUp() {
    scheduler = Executors.newSingleThreadScheduledExecutor();
    collector = new ProgressTimingCollector(scheduler);
  }

  @After
  public void cleanup() {
    collector.shutdown();
    scheduler.shutdownNow();
  }

  @Test
  public void perItemComputationIsCorrect() {
    // 100 items in 5000ms -> 50ms/item
    collector.recordTiming("A", 100, 5000);
    // 50 items in 1000ms -> 20ms/item
    collector.recordTiming("A", 50, 1000);

    var topN = collector.computeTopN();
    assertEquals(1, topN.size());

    var stats = topN.get(0).getValue();
    assertEquals(2, stats.invocations());
    assertEquals(150, stats.totalItems());
    assertEquals(6000, stats.totalMs());
    // avg per item = (50 + 20) / 2 = 35
    assertEquals(35.0, stats.avgPerItem(), 0.01);
  }

  @Test
  public void stddevComputationIsCorrect() {
    // 1 item in 100ms -> 100ms/item
    collector.recordTiming("B", 1, 100);
    // 1 item in 200ms -> 200ms/item
    collector.recordTiming("B", 1, 200);

    var topN = collector.computeTopN();
    var stats = topN.get(0).getValue();
    // avg = 150, stddev = sqrt(((100-150)^2 + (200-150)^2) / 2) = 50
    assertEquals(150.0, stats.avgPerItem(), 0.01);
    assertEquals(50.0, stats.stddevPerItem(), 0.01);
  }

  @Test
  public void singleInvocationHasZeroStddev() {
    collector.recordTiming("C", 10, 500);

    var topN = collector.computeTopN();
    var stats = topN.get(0).getValue();
    assertEquals(50.0, stats.avgPerItem(), 0.01);
    assertEquals(0.0, stats.stddevPerItem(), 0.01);
  }

  @Test
  public void topNOrdersByAvgPerItemDescending() {
    collector.recordTiming("slow", 1, 1000); // 1000 ms/item
    collector.recordTiming("medium", 1, 500); // 500 ms/item
    collector.recordTiming("fast", 1, 100); // 100 ms/item

    var topN = collector.computeTopN();
    assertEquals(3, topN.size());
    assertEquals("slow", topN.get(0).getKey());
    assertEquals("medium", topN.get(1).getKey());
    assertEquals("fast", topN.get(2).getKey());
  }

  @Test
  public void topNLimitsToTenEntries() {
    for (int i = 0; i < 15; i++) {
      collector.recordTiming("handle_" + i, 1, (i + 1) * 100L);
    }

    var topN = collector.computeTopN();
    assertEquals(ProgressTimingCollector.TOP_N, topN.size());
    // The slowest (handle_14, 1500ms) should be first
    assertEquals("handle_14", topN.get(0).getKey());
    // The 10th slowest (handle_5, 600ms) should be last
    assertEquals("handle_5", topN.get(9).getKey());
  }

  @Test
  public void flushDoesNotClearData() {
    collector.recordTiming("X", 1, 100);
    collector.recordTiming("X", 1, 200);

    collector.flushAsTelemetry();

    // Data should still be there
    var topN = collector.computeTopN();
    assertEquals(1, topN.size());
    assertEquals(2, topN.get(0).getValue().invocations());
  }

  @Test
  public void accumulatesAcrossMultipleFlushes() {
    collector.recordTiming("Z", 1, 100);
    collector.flushAsTelemetry();

    collector.recordTiming("Z", 1, 200);
    collector.flushAsTelemetry();

    // Both recordings should be accumulated
    var topN = collector.computeTopN();
    assertEquals(1, topN.size());
    var stats = topN.get(0).getValue();
    assertEquals(2, stats.invocations());
    assertEquals(150.0, stats.avgPerItem(), 0.01);
  }

  @Test
  public void threadSafety() throws Exception {
    int threads = 8;
    int recordsPerThread = 1000;
    var latch = new CountDownLatch(threads);
    var executor = Executors.newFixedThreadPool(threads);
    var errors = new ConcurrentLinkedQueue<Throwable>();

    for (int t = 0; t < threads; t++) {
      final int threadId = t;
      executor.submit(
          () -> {
            try {
              for (int i = 0; i < recordsPerThread; i++) {
                collector.recordTiming("handle_" + (threadId % 4), 1, 10);
              }
            } catch (Throwable e) {
              errors.add(e);
            } finally {
              latch.countDown();
            }
          });
    }

    assertTrue("Threads should complete in time", latch.await(10, TimeUnit.SECONDS));
    executor.shutdown();
    assertTrue("No errors during concurrent recording", errors.isEmpty());

    // 4 distinct handles, each with 2 threads * 1000 records = 2000 invocations
    var topN = collector.computeTopN();
    assertEquals(4, topN.size());
    long totalInvocations = topN.stream().mapToLong(e -> e.getValue().invocations()).sum();
    assertEquals(threads * recordsPerThread, totalInvocations);
  }

  @Test
  public void zeroItemCountHandledGracefully() {
    // up_to is forced to max(1) in Progress.run, but guard against 0 defensively
    collector.recordTiming("edge", 0, 100);

    var topN = collector.computeTopN();
    assertEquals(1, topN.size());
    // With itemCount=0, Math.max(0,1)=1, so perItem = 100/1 = 100
    assertEquals(100.0, topN.get(0).getValue().avgPerItem(), 0.01);
  }

  @Test
  public void evictsLowestAvgPerItemWhenMaxDistinctHandlesReached() {
    int max = ProgressTimingCollector.MAX_DISTINCT_HANDLES;
    // "fast" has the lowest avgPerItem (1ms/item), should be evicted
    collector.recordTiming("fast", 1, 1);
    // All others have 100ms/item
    for (int i = 1; i < max; i++) {
      collector.recordTiming("handle_" + i, 1, 100);
    }

    assertTrue(collector.containsHandle("fast"));

    // Recording a new handle should evict bottom 20% by avgPerItem, including "fast"
    collector.recordTiming("newcomer", 1, 100);

    assertTrue("newcomer should be present", collector.containsHandle("newcomer"));
    assertFalse("fast should have been evicted", collector.containsHandle("fast"));
  }
}
