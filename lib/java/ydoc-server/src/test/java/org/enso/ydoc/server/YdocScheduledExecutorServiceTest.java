package org.enso.ydoc.server;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.Test;

public class YdocScheduledExecutorServiceTest {

  /**
   * Waits for scheduled tasks to become ready by using the executor's waitForTasks method. This
   * avoids Thread.sleep and uses the executor's own timing mechanism.
   */
  private void waitUntilTasksReady(YdocScheduledExecutorService service)
      throws InterruptedException {
    long delay;
    while ((delay = service.getNextTaskDelayNanos()) > 0) {
      service.waitForTasks(delay);
    }
  }

  /**
   * Waits for a condition to become true, processing tasks in the meantime. Times out after the
   * specified duration.
   */
  private void waitForCondition(
      YdocScheduledExecutorService service,
      java.util.function.BooleanSupplier condition,
      long timeoutMs)
      throws InterruptedException {
    long deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(timeoutMs);
    while (!condition.getAsBoolean()) {
      long remaining = deadline - System.nanoTime();
      if (remaining <= 0) {
        fail("Timeout waiting for condition");
      }
      service.processPendingTasks();
      long delay = service.getNextTaskDelayNanos();
      if (delay > 0) {
        service.waitForTasks(Math.min(delay, remaining));
      } else if (delay == -1) {
        service.waitForTasks(Math.min(TimeUnit.MILLISECONDS.toNanos(10), remaining));
      }
    }
  }

  @Test
  public void testImmediateTaskExecution() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    AtomicInteger counter = new AtomicInteger(0);

    // Submit immediate tasks
    service.submit(() -> counter.incrementAndGet());
    service.submit(() -> counter.incrementAndGet());
    service.submit(() -> counter.incrementAndGet());

    // Process tasks
    int executed = service.processPendingTasks();

    assertEquals(3, executed);
    assertEquals(3, counter.get());
  }

  @Test
  public void testTaskExecutionOrder() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    List<Integer> executionOrder = new ArrayList<>();

    // Submit tasks in order
    service.submit(() -> executionOrder.add(1));
    service.submit(() -> executionOrder.add(2));
    service.submit(() -> executionOrder.add(3));

    service.processPendingTasks();

    // Verify FIFO order
    assertEquals(List.of(1, 2, 3), executionOrder);
  }

  @Test
  public void testScheduledTaskWithDelay() throws InterruptedException {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    AtomicInteger counter = new AtomicInteger(0);

    // Schedule a task with delay
    service.schedule(() -> counter.incrementAndGet(), 50, TimeUnit.MILLISECONDS);

    // Wait for the delay using executor's mechanism
    waitUntilTasksReady(service);

    // Process again - should execute now
    int executed2 = service.processPendingTasks();
    assertEquals(1, executed2);
    assertEquals(1, counter.get());
  }

  @Test
  public void testMultipleScheduledTasks() throws InterruptedException {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    List<Integer> executionOrder = new ArrayList<>();

    // Schedule tasks with different delays
    service.schedule(() -> executionOrder.add(3), 60, TimeUnit.MILLISECONDS);
    service.schedule(() -> executionOrder.add(1), 20, TimeUnit.MILLISECONDS);
    service.schedule(() -> executionOrder.add(2), 40, TimeUnit.MILLISECONDS);

    // Wait and process tasks as they become ready
    waitForCondition(service, () -> executionOrder.size() == 3, 500);
    service.processPendingTasks();

    assertEquals(List.of(1, 2, 3), executionOrder);
  }

  @Test
  public void testMixedImmediateAndScheduledTasks() throws InterruptedException {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    List<String> executionOrder = new ArrayList<>();

    // Mix immediate and scheduled tasks
    service.submit(() -> executionOrder.add("immediate1"));
    service.schedule(() -> executionOrder.add("scheduled1"), 30, TimeUnit.MILLISECONDS);
    service.submit(() -> executionOrder.add("immediate2"));

    // Process immediate tasks first
    service.processPendingTasks();
    assertEquals(List.of("immediate1", "immediate2"), executionOrder);

    // Wait and process scheduled task
    waitUntilTasksReady(service);
    service.processPendingTasks();
    assertEquals(List.of("immediate1", "immediate2", "scheduled1"), executionOrder);
  }

  @Test
  public void testCallableWithResult() throws Exception {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();

    var future = service.submit(() -> "Hello World");

    service.processPendingTasks();

    assertTrue(future.isDone());
    assertEquals("Hello World", future.get());
  }

  @Test
  public void testCallableWithException() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();

    var future =
        service.submit(
            () -> {
              throw new RuntimeException("Test exception");
            });

    service.processPendingTasks();

    assertTrue(future.isDone());
    try {
      future.get();
      fail("Expected exception");
    } catch (Exception e) {
      assertTrue(e.getMessage().contains("Test exception"));
    }
  }

  @Test
  public void testScheduledCallable() throws Exception {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();

    var future = service.schedule(() -> 42, 30, TimeUnit.MILLISECONDS);

    assertFalse(future.isDone());

    waitUntilTasksReady(service);
    service.processPendingTasks();

    assertTrue(future.isDone());
    assertEquals(Integer.valueOf(42), future.get());
  }

  @Test
  public void testTasksExecuteOnOwnerThread() throws InterruptedException {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    long ownerThreadId = Thread.currentThread().threadId();

    List<Long> immediateTaskThreadIds = new ArrayList<>();
    List<Long> scheduledTaskThreadIds = new ArrayList<>();

    // Submit immediate tasks from different threads
    service.submit(() -> immediateTaskThreadIds.add(Thread.currentThread().threadId()));

    Thread submitterThread =
        new Thread(
            () -> {
              service.submit(() -> immediateTaskThreadIds.add(Thread.currentThread().threadId()));
              service.schedule(
                  () -> scheduledTaskThreadIds.add(Thread.currentThread().threadId()),
                  30,
                  TimeUnit.MILLISECONDS);
            });
    submitterThread.start();
    submitterThread.join();

    // Schedule a task from the owner thread
    service.schedule(
        () -> scheduledTaskThreadIds.add(Thread.currentThread().threadId()),
        50,
        TimeUnit.MILLISECONDS);

    // Process immediate tasks on owner thread
    service.processPendingTasks();

    // All immediate tasks should have executed on owner thread
    assertEquals(2, immediateTaskThreadIds.size());
    for (Long threadId : immediateTaskThreadIds) {
      assertEquals(ownerThreadId, threadId.longValue());
    }

    // Wait for scheduled tasks and process
    waitForCondition(service, () -> scheduledTaskThreadIds.size() == 2, 500);
    service.processPendingTasks();

    // All scheduled tasks should have executed on owner thread
    assertEquals(2, scheduledTaskThreadIds.size());
    for (Long threadId : scheduledTaskThreadIds) {
      assertEquals(ownerThreadId, threadId.longValue());
    }
  }

  @Test
  public void testHasPendingTasks() throws InterruptedException {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();

    assertFalse(service.hasPendingTasks());

    service.submit(() -> {});
    assertTrue(service.hasPendingTasks());

    service.processPendingTasks();
    assertFalse(service.hasPendingTasks());

    service.schedule(() -> {}, 30, TimeUnit.MILLISECONDS);
    assertFalse(service.hasPendingTasks()); // Not ready yet

    waitUntilTasksReady(service);
    assertTrue(service.hasPendingTasks()); // Now ready
  }

  @Test
  public void testGetNextTaskDelay() throws InterruptedException {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();

    assertEquals(-1, service.getNextTaskDelayNanos()); // No tasks

    service.submit(() -> {});
    assertEquals(0, service.getNextTaskDelayNanos()); // Immediate task

    service.processPendingTasks();
    assertEquals(-1, service.getNextTaskDelayNanos()); // No tasks again

    service.schedule(() -> {}, 100, TimeUnit.MILLISECONDS);
    long delay = service.getNextTaskDelayNanos();
    assertTrue(delay > 0 && delay <= TimeUnit.MILLISECONDS.toNanos(100));
  }

  @Test
  public void testShutdown() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();

    assertFalse(service.isShutdown());

    service.shutdown();
    assertTrue(service.isShutdown());

    // Should not accept new tasks after shutdown
    try {
      service.submit(() -> {});
      fail("Expected IllegalStateException");
    } catch (IllegalStateException e) {
      // Expected
    }
  }

  @Test
  public void testExceptionHandlingInTask() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    AtomicInteger counter = new AtomicInteger(0);

    // Submit task that throws exception
    service.submit(
        () -> {
          throw new RuntimeException("Test exception");
        });

    // Submit another task to verify service continues
    service.submit(() -> counter.incrementAndGet());

    // Process tasks - should handle exception and continue
    service.processPendingTasks();

    // Second task should have executed despite first one throwing
    assertEquals(1, counter.get());
  }

  @Test
  public void testScheduledFutureCompareToIsStable() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();

    // Schedule tasks with distinct delays so their executeAtNanos values differ
    ScheduledFuture<?> earlier = service.schedule(() -> {}, 100, TimeUnit.MILLISECONDS);
    ScheduledFuture<?> later = service.schedule(() -> {}, 200, TimeUnit.MILLISECONDS);

    // Self-comparison must always be 0, regardless of System.nanoTime() drift.
    // Before the fix, two getDelay() calls inside compareTo could return different
    // values because each calls System.nanoTime() independently, making
    // this.compareTo(this) non-zero.
    for (int i = 0; i < 1000; i++) {
      assertEquals("self-comparison must be 0 on iteration " + i, 0, earlier.compareTo(earlier));
      assertEquals("self-comparison must be 0 on iteration " + i, 0, later.compareTo(later));
    }

    // Relative ordering must be consistent: earlier < later
    for (int i = 0; i < 1000; i++) {
      assertTrue(
          "earlier.compareTo(later) must be negative on iteration " + i,
          earlier.compareTo(later) < 0);
      assertTrue(
          "later.compareTo(earlier) must be positive on iteration " + i,
          later.compareTo(earlier) > 0);
    }
  }

  @Test
  public void testScheduledCallableCompareToIsStable() throws Exception {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();

    ScheduledFuture<String> earlier = service.schedule(() -> "a", 100, TimeUnit.MILLISECONDS);
    ScheduledFuture<String> later = service.schedule(() -> "b", 200, TimeUnit.MILLISECONDS);

    for (int i = 0; i < 1000; i++) {
      assertEquals("self-comparison must be 0 on iteration " + i, 0, earlier.compareTo(earlier));
      assertEquals("self-comparison must be 0 on iteration " + i, 0, later.compareTo(later));
    }

    for (int i = 0; i < 1000; i++) {
      assertTrue(
          "earlier.compareTo(later) must be negative on iteration " + i,
          earlier.compareTo(later) < 0);
      assertTrue(
          "later.compareTo(earlier) must be positive on iteration " + i,
          later.compareTo(earlier) > 0);
    }
  }

  @Test
  public void testHighPriorityTasksExecuteFirst() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    List<String> executionOrder = new ArrayList<>();

    // Submit regular tasks first
    service.submit(() -> executionOrder.add("regular1"));
    service.submit(() -> executionOrder.add("regular2"));

    // Submit high-priority tasks via execute() with HighPriorityRunnable
    service.execute(
        new YdocScheduledExecutorService.HighPriorityRunnable(() -> executionOrder.add("high1")));
    service.execute(
        new YdocScheduledExecutorService.HighPriorityRunnable(() -> executionOrder.add("high2")));

    service.processPendingTasks();

    // High-priority tasks should execute before regular tasks
    assertEquals(List.of("high1", "high2", "regular1", "regular2"), executionOrder);
  }

  @Test
  public void testHighPriorityViewRoutesToHighPriorityQueue() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    var highPriorityView = service.createHighPriorityView();
    List<String> executionOrder = new ArrayList<>();

    // Submit regular tasks directly
    service.submit(() -> executionOrder.add("regular1"));
    service.submit(() -> executionOrder.add("regular2"));

    // Submit via high-priority view
    highPriorityView.execute(() -> executionOrder.add("high1"));
    highPriorityView.execute(() -> executionOrder.add("high2"));

    service.processPendingTasks();

    // View tasks should execute before regular tasks
    assertEquals(List.of("high1", "high2", "regular1", "regular2"), executionOrder);
  }

  @Test
  public void testHighPriorityTasksInterleavedDuringProcessing() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    var highPriorityView = service.createHighPriorityView();
    List<String> executionOrder = new ArrayList<>();

    // Submit regular tasks where the first one submits a high-priority task during execution
    service.submit(
        () -> {
          executionOrder.add("regular1");
          // Simulate a WebSocket message arriving while processing regular tasks
          highPriorityView.execute(() -> executionOrder.add("high-interleaved"));
        });
    service.submit(() -> executionOrder.add("regular2"));

    service.processPendingTasks();

    // The high-priority task submitted during regular1 should execute before regular2
    assertEquals(List.of("regular1", "high-interleaved", "regular2"), executionOrder);
  }

  @Test
  public void testDebugStatsDisabledByDefault() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();

    service.submit(() -> {});
    service.processPendingTasks();

    assertEquals("", service.getDebugStats());
  }

  @Test
  public void testDebugStatsEmptyWhenNoTasksExecuted() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService(true);

    assertEquals("", service.getDebugStats());
  }

  @Test
  public void testDebugStatsCollectedForRegularTasks() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService(true);

    service.submit(() -> {});
    service.submit(() -> {});
    service.submit(() -> {});

    service.processPendingTasks();

    String stats = service.getDebugStats();
    assertTrue(stats.contains("Task execution stats:"));
    assertTrue(stats.contains("Regular:"));
    assertTrue(stats.contains("count=3"));
  }

  @Test
  public void testDebugStatsSeparateHighPriorityFromRegular() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService(true);

    // Submit regular tasks
    service.submit(() -> {});
    service.submit(() -> {});

    // Submit high-priority tasks
    service.execute(new YdocScheduledExecutorService.HighPriorityRunnable(() -> {}));
    service.execute(new YdocScheduledExecutorService.HighPriorityRunnable(() -> {}));
    service.execute(new YdocScheduledExecutorService.HighPriorityRunnable(() -> {}));

    service.processPendingTasks();

    String stats = service.getDebugStats();
    assertTrue(stats.contains("High priority:"));
    assertTrue(stats.contains("Regular:"));
    // High priority line should contain count=3
    String[] lines = stats.split("\n");
    for (String line : lines) {
      if (line.contains("High priority:")) {
        assertTrue(line.contains("count=3"));
      }
      if (line.contains("Regular:")) {
        assertTrue(line.contains("count=2"));
      }
    }
  }

  @Test
  public void testDebugStatsIncludeScheduledTasks() throws InterruptedException {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService(true);

    service.schedule(() -> {}, 10, TimeUnit.MILLISECONDS);

    waitUntilTasksReady(service);
    service.processPendingTasks();

    String stats = service.getDebugStats();
    assertTrue(stats.contains("Regular:"));
    // Scheduled tasks count as regular
    String[] lines = stats.split("\n");
    for (String line : lines) {
      if (line.contains("Regular:")) {
        assertTrue(line.contains("count=1"));
      }
    }
  }

  @Test
  public void testEventLoopPattern() throws InterruptedException {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    AtomicInteger counter = new AtomicInteger(0);

    // Schedule recurring tasks
    service.submit(() -> counter.incrementAndGet());
    service.schedule(() -> counter.incrementAndGet(), 10, TimeUnit.MILLISECONDS);
    service.schedule(() -> counter.incrementAndGet(), 20, TimeUnit.MILLISECONDS);

    // Wait for all tasks to be ready and process them
    waitForCondition(service, () -> counter.get() == 3, 500);
    service.processPendingTasks();

    // All tasks should have executed
    assertEquals(3, counter.get());
  }
}
